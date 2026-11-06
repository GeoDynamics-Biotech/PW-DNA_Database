# Load necessary libraries
library(tidyverse)
library(stringr)
library(scales)

# --- 1. DATA PREPARATION & COLOR DEFINITION ---

taxonomy_file_path <- "supplemental_figure1_udate.csv" 
data <- read.csv(taxonomy_file_path, header = TRUE, stringsAsFactors = FALSE)

# The order you list these here is exactly how they will appear in the legend and bars
custom_colors <- c(
  "Methanocalculus" = "midnightblue",
  "Methanoculleus" = "navy",
  "Methanoregula" = "darkblue",
  "Methanospirillum" = "mediumblue",
  "Methanobacterium" = "blue",
  "Methanothermobacter" = "royalblue",
  "Methanothermococcus" = "cornflowerblue",
  "Methanococcus" = "skyblue2",
  "UBA207" = "skyblue",
  "UBA349" = "lightblue",
  "Methanohalophilus" = "#993300",
  "Methanolobus" = "chocolate",
  "Methanomethylovorans" = "darkorange",
  "Methermicoccus" = "orange", 
  "UBA9915" = "sandybrown",
  "UBA472" =  "#FFCC66",
  "VadinCA11" = "tan",
  "Methanothrix" = "green3"
#  "Other"              = "#999999", # Grey for rare taxa
 # "Unclassified_Genus" = "#d3d3d3"  # Light Grey
)

tax_levels <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
tax_prefixes <- c("d__", "p__", "c__", "o__", "f__", "g__", "s__")

LEGEND_CUTOFF <- 20
HIGH_LEVEL_CUTOFF <- 100

parse_and_clean_taxonomy <- function(df) {
  df %>%
    filter(!is.na(Basin) & Basin != "" & Basin != " ") %>%
    rename(User_Genome = Basin, Classification = Taxon) %>% 
    separate(
      col = Classification,
      into = tax_levels,
      sep = ";",
      fill = "right"
    ) %>%
    mutate(across(all_of(tax_levels), ~ str_remove(., "^[dpcofgs]__"))) %>%
    mutate(across(all_of(tax_levels), ~ str_trim(.))) %>%
    mutate(across(all_of(tax_levels), ~ case_when(
      . == "" | is.na(.) ~ paste0("Unclassified_", cur_column()),
      TRUE ~ .
    )))
}

# --- 2. CALCULATE RELATIVE ABUNDANCE & FORCE ORDER ---

tax_data_cleaned <- parse_and_clean_taxonomy(data)

tax_data_filtered <- tax_data_cleaned %>%
  pivot_longer(
    cols = all_of(tax_levels),
    names_to = "Level",
    values_to = "Taxon"
  ) %>%
  mutate(Level = factor(Level, levels = tax_levels)) %>%
  
  group_by(Level, Taxon) %>%
  mutate(Taxon_Count_Global = n()) %>%
  ungroup() %>%
  
  group_by(Level) %>%
  mutate(
    Taxon_Rank = dense_rank(desc(Taxon_Count_Global)),
    current_cutoff = if_else(Level %in% c("Family", "Genus", "Species"), LEGEND_CUTOFF, HIGH_LEVEL_CUTOFF),
    Taxon_Group = if_else(Taxon_Rank <= current_cutoff, Taxon, "Other")
  ) %>%
  ungroup() %>%
  
  group_by(User_Genome, Level, Taxon_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(User_Genome, Level) %>%
  mutate(Relative_Abundance = Count / sum(Count)) %>%
  ungroup() %>%
  rename(Taxon = Taxon_Group) %>%
  
  # --- NEW: FORCE TAXON ORDERING ---
  # This matches the factor levels to the names in your custom_colors vector
  mutate(Taxon = factor(Taxon, levels = unique(c(names(custom_colors), unique(Taxon)))))

# --- 3. GENERATE PLOTS ---

output_dir <- "taxonomy_abundance_plots_methanogen_basin_v2"
if(!dir.exists(output_dir)) dir.create(output_dir)

for (rank in tax_levels) {
  
  plot_data <- tax_data_filtered %>% filter(Level == rank)
  
  p <- ggplot(plot_data, aes(x = User_Genome, y = Relative_Abundance, fill = Taxon)) +
    geom_bar(stat = "identity", width = 0.8) +
    
    # Values uses the vector colors, breaks ensures the order follows the levels we set
    scale_fill_manual(values = custom_colors, drop = FALSE) + 
    
    scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste("Relative Abundance:", rank),
      x = "Basin",
      y = "Relative Abundance",
      fill = rank
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      panel.grid.major.x = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 5, title.position = "top"))
  
  filename <- paste0(output_dir, "/", tolower(rank), "_abundance.png")
  ggsave(filename, plot = p, width = 12, height = 8, dpi = 300)
}
