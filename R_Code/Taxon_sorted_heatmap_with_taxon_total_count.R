if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
install.packages("stringr")


library(GetoptLong)
library(circlize)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr) 
library(purrr)
library(ComplexHeatmap)

# --- DATA IMPORT ---
df <- read.delim("Figure_3_v2_updates.tsv", header = TRUE, sep = "\t", check.names = FALSE)

tax_levels <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
genes <- c("dsrAB", "phsA", "aprA")

output_dir <- "Top10_Heatmaps_Taxonomy_Ordered_with_count_all_taxon_v2_update"
if(!dir.exists(output_dir)) dir.create(output_dir)

# --- Heatmap ---

for (i in 1:length(tax_levels)) {
  current_rank <- tax_levels[i]
  
  # Split and Clean Taxonomy
  df_processed <- df %>%
    separate(Taxon, into = tax_levels, sep = ";", fill = "right", remove = FALSE) %>%
    mutate(across(all_of(tax_levels), ~ {
      x <- str_trim(.)
      is_blank <- is.na(x) | x == "" | str_detect(x, "^[a-z]__$")
      ifelse(is_blank, "Unclassified", x)
    }))
  
  # --- UPDATED: Group, Sum, and Count Occurrences ---
  df_rank <- df_processed %>%
    group_by(Taxon_Name = !!sym(current_rank)) %>%
    summarise(
      Full_Path = min(Taxon), 
      across(all_of(genes), sum),
      Taxon_Count = n(),               # New column to count merged rows
      .groups = "drop"
    )
  
  # FILTER FOR TOP 10 PER GENE
  top_taxa <- map(genes, function(g) {
    df_rank %>%
      slice_max(order_by = !!sym(g), n = 10, with_ties = FALSE) %>%
      pull(Taxon_Name)
  }) %>% unlist() %>% unique()
  
  # Filter and SORT BY TAXONOMY
  df_filtered <- df_rank %>% 
    filter(Taxon_Name %in% top_taxa) %>%
    arrange(Full_Path) 
  
  # Heatmap Matrix
  plot_mat <- as.matrix(df_filtered[, genes])
  
  # --- UPDATED: Add counts to the row labels ---
  # Clean prefix (e.g., "p__") then add the count suffix
  clean_names <- str_remove(df_filtered$Taxon_Name, "^[a-z]__")
  row_labels <- paste0(clean_names, " (", df_filtered$Taxon_Count, ")")
  rownames(plot_mat) <- row_labels
  
  # Colors and Plotting
  max_val <- max(plot_mat)
  col_fun <- colorRamp2(c(0, max_val), c("white", "darkblue"))
  
  file_name <- paste0(output_dir, "/Top10_", current_rank, "_TaxonomySorted.png")
  png(file_name, width = 900, height = 800, res = 120)
  
  ht <- Heatmap(
    plot_mat,
    name = "Number of Taxon with Gene Presence",
    col = col_fun,
    cluster_columns = FALSE, 
    cluster_rows = FALSE,         
    show_row_dend = FALSE,        
 #   column_title = paste("Top Taxa at", current_rank, "Level (Sorted by Taxonomy)"),
    row_title = current_rank,
    rect_gp = gpar(col = "white", lwd = 1),
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid.text(sprintf("%d", plot_mat[i, j]), x, y, gp = gpar(fontsize = 10))
    }
  )
  
  draw(ht)
  dev.off()
  
  cat("Finished rank:", current_rank, "\n")
}

# --- UPDATED Heatmap Loop for Adobe Illustrator ---

for (i in 1:length(tax_levels)) {
  current_rank <- tax_levels[i]
  
  # ... [Your existing data processing code stays the same] ...
  df_processed <- df %>%
    separate(Taxon, into = tax_levels, sep = ";", fill = "right", remove = FALSE) %>%
    mutate(across(all_of(tax_levels), ~ {
      x <- str_trim(.)
      is_blank <- is.na(x) | x == "" | str_detect(x, "^[a-z]__$")
      ifelse(is_blank, "Unclassified", x)
    }))
  
  df_rank <- df_processed %>%
    group_by(Taxon_Name = !!sym(current_rank)) %>%
    summarise(
      Full_Path = min(Taxon), 
      across(all_of(genes), sum),
      Taxon_Count = n(),
      .groups = "drop"
    )
  
  top_taxa <- map(genes, function(g) {
    df_rank %>%
      slice_max(order_by = !!sym(g), n = 10, with_ties = FALSE) %>%
      pull(Taxon_Name)
  }) %>% unlist() %>% unique()
  
  df_filtered <- df_rank %>% 
    filter(Taxon_Name %in% top_taxa) %>%
    arrange(Full_Path) 
  
  plot_mat <- as.matrix(df_filtered[, genes])
  clean_names <- str_remove(df_filtered$Taxon_Name, "^[a-z]__")
  row_labels <- paste0(clean_names, " (", df_filtered$Taxon_Count, ")")
  rownames(plot_mat) <- row_labels
  
  max_val <- max(plot_mat)
  col_fun <- colorRamp2(c(0, max_val), c("white", "darkblue"))
  
  # --- CHANGE 1: Update file extension to .pdf ---
  file_name <- paste0(output_dir, "/Top10_", current_rank, "_TaxonomySorted.pdf")
  
  # --- CHANGE 2: Use cairo_pdf (Vector format) ---
  # Note: width and height in pdf() are in INCHES, not pixels.
  # 7x7 or 8x8 is usually a good starting point for heatmaps.
  cairo_pdf(file_name, width = 8, height = 7)
  
  ht <- Heatmap(
    plot_mat,
    name = "Number of Taxa with Gene Presence",
    col = col_fun,
    cluster_columns = FALSE, 
    cluster_rows = FALSE,          
    show_row_dend = FALSE,        
    row_title = current_rank,
    rect_gp = gpar(col = "white", lwd = 1),
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid.text(sprintf("%d", plot_mat[i, j]), x, y, gp = gpar(fontsize = 10))
    }
  )
  
  draw(ht)
  dev.off()
  
  cat("Finished rank (PDF):", current_rank, "\n")
}
