# Install missing packages if necessary
install.packages(c("tidyverse", "stringr", "scales", "RColorBrewer", "ggh4x"))

library(tidyverse)
library(stringr)
library(scales) 
library(RColorBrewer)
library(ggh4x) 

# --- CONFIGURATION & SETUP ---
abundance_file_path <- "North_america_amplicon_by_primer.txt"
tax_levels <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")

# Output directory for scientific plots
output_dir <- "North_America_Scientific_Plots_Primer_Grid_V6"
if(!dir.exists(output_dir)) dir.create(output_dir)

# --- 1. DATA IMPORT ---
raw_matrix <- read.delim(abundance_file_path, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)

# Extract metadata rows based on your file structure:
# Row 1: Primer, Row 2: Basin, Row 3: Sample names
primers <- as.character(raw_matrix[1, 3:ncol(raw_matrix)])
basins <- as.character(raw_matrix[2, 3:ncol(raw_matrix)])
sample_names <- as.character(raw_matrix[3, 3:ncol(raw_matrix)])

sample_meta <- data.frame(
  Sample = sample_names,
  Basin = basins,
  Primer = primers,
  stringsAsFactors = FALSE
)

data_raw <- raw_matrix[4:nrow(raw_matrix), ]
colnames(data_raw) <- c("Methanogenesis", "RDP_taxon_full", sample_names)
data_raw <- data_raw %>% mutate(across(all_of(sample_names), as.numeric))
data_raw$Methanogenesis <- trimws(data_raw$Methanogenesis)

# Replace NA or blank Methanogenesis values with "Not Applicable" (for Bacteria, Eukaryotes, Non-Methanogenic Archaea)
data_raw$Methanogenesis[is.na(data_raw$Methanogenesis) | data_raw$Methanogenesis == "" | data_raw$Methanogenesis == "NA"] <- "Not Applicable"

# --- 2. PROCESSING FUNCTION ---

process_and_plot_rank_bubble <- function(input_data, rank_index, rank_name, output_dir, meta_map) {
  
  current_rank_col <- tax_levels[rank_index]
  
  # A. TAXONOMY CLEANING AND AGGREGATION
  df_processed <- input_data %>%
    separate(col = RDP_taxon_full, into = tax_levels, sep = ";", fill = "right", remove = FALSE) %>%
    mutate(across(all_of(tax_levels), ~trimws(.))) %>%
    mutate(across(all_of(tax_levels), ~na_if(., ""))) 
  
  # Logic to find previous non-empty rank if current rank is NA
  df_processed$Taxon_Label <- pmap_chr(df_processed[tax_levels[1:rank_index]], function(...) {
    lvls <- c(...)
    current_val <- lvls[rank_index]
    
    if (!is.na(current_val)) {
      return(current_val)
    } else {
      prev_levels <- lvls[1:(rank_index-1)]
      last_known <- tail(prev_levels[!is.na(prev_levels)], 1)
      
      if (length(last_known) == 0) {
        return("Unclassified_Root")
      } else {
        return(paste0("Unclassified_", last_known))
      }
    }
  })
  
  df_processed <- df_processed %>%
    group_by(Methanogenesis, Taxon_Label) %>%
    summarise(across(all_of(meta_map$Sample), sum, na.rm = TRUE), .groups = 'drop') %>%
    rename(Taxon = Taxon_Label)
  
  # B. RELATIVE ABUNDANCE CALCULATION
  df_long <- df_processed %>%
    pivot_longer(cols = all_of(meta_map$Sample), names_to = "Sample", values_to = "Count") %>%
    group_by(Sample) %>%
    mutate(Relative_Abundance = Count / sum(Count, na.rm = TRUE)) %>% 
    ungroup() %>%
    group_by(Sample, Methanogenesis, Taxon) %>%
    summarise(Relative_Abundance = sum(Relative_Abundance), .groups = 'drop') %>%
    left_join(meta_map, by = "Sample") %>%
    filter(Relative_Abundance > 0)
  
  if(nrow(df_long) == 0) {
    cat(paste("No data found for rank:", rank_name, "- skipping.\n"))
    return(NULL)
  }
  
  # C. ORDERING & GROUPING
  ordered_basins <- sort(unique(df_long$Basin))
  df_long$Basin <- factor(df_long$Basin, levels = ordered_basins)
  
  df_filtered <- df_long %>%
    mutate(Methanogenesis = factor(Methanogenesis, levels = c("Hydrogenotrophic", "Methylotrophic", "Acetoclastic", "Versatile", "Not Applicable"))) %>%
    arrange(desc(Methanogenesis), desc(Taxon)) %>%
    mutate(Taxon = factor(Taxon, levels = unique(Taxon)))
  
  # D. CSV EXPORT
  csv_filename <- paste0(output_dir, "/", tolower(rank_name), "_abundance_by_primer_grid.csv")
  write.csv(df_filtered, csv_filename, row.names = FALSE)
  
  # E. WIDTHS & COLORS
  basin_widths <- df_filtered %>% group_by(Basin) %>% summarise(n_samples = n_distinct(Sample)) %>% 
    arrange(Basin) %>% mutate(rel_width = pmax(n_samples, 4)) %>% pull(rel_width)
  
  methano_colors <- c(
    "Hydrogenotrophic" = "#3182bd", 
    "Methylotrophic" = "#e6550d", 
    "Acetoclastic" = "#31a354", 
    "Versatile" = "#756bb1",
    "Not Applicable" = "#bdbdbd"
  )
  
  # F. PLOTTING WITH PRIMER x BASIN GRID
  p <- ggplot(df_filtered, aes(x = Sample, y = Taxon)) +
    geom_point(aes(size = Relative_Abundance, fill = Methanogenesis), shape = 21, color = "black", stroke = 0.3) +
    facet_grid(Primer ~ Basin, scales = "free", space = "free", switch = "both") +
    force_panelsizes(cols = unit(basin_widths, "null")) +
    scale_y_discrete(expand = c(0, 0.8)) + # Expanded padding top/bottom of Y axis
    scale_size_area(max_size = 8, labels = percent, name = "Relative Abundance") +
    scale_fill_manual(values = methano_colors, drop = FALSE) +
    labs(x = NULL, y = rank_name) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 9), # Slightly larger font for readability
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.x = element_text(
        face = "bold", 
        size = 9, 
        angle = 45, 
        vjust = 0.5, 
        hjust = 0.5 
      ),
      strip.text.y = element_text(
        face = "bold", 
        size = 9, 
        angle = 0
      ),
      strip.clip = "off", 
      panel.spacing.y = unit(0.5, "lines"), # Increased vertical spacing between panels
      panel.spacing.x = unit(0.3, "lines"),
      panel.grid.major = element_line(color = "grey95", linewidth = 0.1),
      legend.position = "right", 
      plot.margin = margin(t = 10, r = 10, b = 70, l = 10)
    )
  
  # G. SAVE AS BOTH PNG AND PDF (Increased height multiplier for better vertical spacing)
  num_rows <- length(unique(df_filtered$Taxon))
  plot_height <- max(10, (num_rows * 0.28) + 4)
  
  base_name <- paste0(output_dir, "/", tolower(rank_name), "_primer_grid")
  
  ggsave(paste0(base_name, ".png"), plot = p, width = 16, height = plot_height, dpi = 300)
  ggsave(paste0(base_name, ".pdf"), plot = p, width = 16, height = plot_height, device = "pdf")
  
  cat(paste("Successfully saved CSV, PNG, and PDF for:", rank_name, "\n"))
}

# --- 3. EXECUTE ---
for (i in 1:length(tax_levels)) {
  process_and_plot_rank_bubble(
    input_data = data_raw, rank_index = i, rank_name = tax_levels[i], 
    output_dir = output_dir, meta_map = sample_meta
  )
}
