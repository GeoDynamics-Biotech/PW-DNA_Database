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
# Replace "your_file_name.tsv" with your actual file name
df <- read.delim("IMG_thiosulfate_all.tsv", header = TRUE, sep = "\t", check.names = FALSE)

# Taxonomy levels matching your column suffixes
tax_levels <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus")

# --- All 6 gene and COG columns of interest ---
genes <- c("KO_dsrB", "KO_dsrA", "KO_phsA", "KO_aprA", "COG_phsA", "COG_dsrAB")

output_dir <- "Top_Heatmaps_Taxonomy_Ordered_with_count_all_taxon_v2_update_for_IMG_all"
if(!dir.exists(output_dir)) dir.create(output_dir)

# --- Heatmap Loop (PNG & PDF) ---

for (i in 1:length(tax_levels)) {
  current_rank <- tax_levels[i]
  col_name <- paste0("GTDB ", current_rank)
  
  # Clean Taxonomy and build a hierarchical path for sorting
  df_processed <- df %>%
    mutate(across(starts_with("GTDB "), ~ {
      x <- str_trim(.)
      is_blank <- is.na(x) | x == "" | str_detect(x, "^[a-z]__$")
      ifelse(is_blank, "Unclassified", x)
    })) %>%
    unite("Full_Path", starts_with("GTDB "), sep = ";", remove = FALSE)
  
  # Group, Sum, and Count Occurrences
  df_rank <- df_processed %>%
    group_by(Taxon_Name = !!sym(col_name)) %>%
    summarise(
      Full_Path = min(Full_Path), 
      across(all_of(genes), sum),
      Taxon_Count = n(),               # Count merged rows
      .groups = "drop"
    )
  
  # --- UPDATED: FILTER FOR TOP 10 PER GENE (EXCLUDING ZEROS) ---
  top_taxa <- map(genes, function(g) {
    df_rank %>%
      filter(!!sym(g) > 0) %>%                               # Remove 0-count taxa first
      slice_max(order_by = !!sym(g), n = 10, with_ties = FALSE) %>%
      pull(Taxon_Name)
  }) %>% unlist() %>% unique()
  
  # Filter and SORT BY TAXONOMY
  df_filtered <- df_rank %>% 
    filter(Taxon_Name %in% top_taxa) %>%
    arrange(Full_Path) 
  
  # Heatmap Matrix
  plot_mat <- as.matrix(df_filtered[, genes])
  
  # Add counts to the row labels
  clean_names <- str_remove(df_filtered$Taxon_Name, "^[a-z]__")
  row_labels <- paste0(clean_names, " (", df_filtered$Taxon_Count, ")")
  rownames(plot_mat) <- row_labels
  
  # Colors and Plotting Parameters
  max_val <- max(plot_mat)
  if(max_val == 0) max_val <- 1 # prevent colorRamp2 error if all zeros
  col_fun <- colorRamp2(c(0, max_val), c("white", "darkblue"))
  
  # ==========================================
  # 1. GENERATE PNG OUTPUT
  # ==========================================
  file_name_png <- paste0(output_dir, "/Top10_", current_rank, "_TaxonomySorted.png")
  png(file_name_png, width = 1200, height = 800, res = 120)
  
  ht <- Heatmap(
    plot_mat,
    name = "Gene Count for each Taxa",
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
  
  # ==========================================
  # 2. GENERATE PDF OUTPUT (Adobe Illustrator)
  # ==========================================
  file_name_pdf <- paste0(output_dir, "/Top10_", current_rank, "_TaxonomySorted.pdf")
  cairo_pdf(file_name_pdf, width = 10, height = 7)
  
  draw(ht) # Reuse the same heatmap object
  dev.off()
  
  cat("Finished rank:", current_rank, "\n")
}

