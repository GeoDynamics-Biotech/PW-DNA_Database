library(ggplot2)
library(scales)
library(patchwork)

# --- 1. PROCESS DATASET 1 ---
df1 <- read.delim("Geochem_Metadata_Bar_Plot_v2.txt", header = FALSE, sep = "\t", col.names = c("Variable", "Value"))
df1$Variable <- factor(df1$Variable, levels = df1$Variable)

p1 <- ggplot(df1, aes(x = Variable, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  theme_minimal() +
  labs(x = "Metadata Type", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.major.x = element_blank())

# --- 2. PROCESS DATASET 2 ---
df2 <- read.delim("Geochem_Barplot_v2.txt", header = FALSE, sep = "\t", col.names = c("Variable", "Value"))
df2$Variable <- factor(df2$Variable, levels = df2$Variable)

df2_main <- subset(df2, Value >= 0.05)
df2_tail <- subset(df2, Value < 0.05)

# Plot B
p2_main <- ggplot(df2_main, aes(x = Variable, y = Value)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  theme_minimal() +
  labs(x = "Geochemical Data", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank())

# Plot C
p2_tail <- ggplot(df2_tail, aes(x = Variable, y = Value)) +
  geom_bar(stat = "identity", fill = "darkorange", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  theme_minimal() +
  labs(x = "Geochemical Data represented in <5% of samples", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.major.x = element_blank())

# --- 3. STACK AND AUTOMATICALLY TAG ---
# We use plot_annotation to add the A, B, C labels automatically
final_stack <- (p1 / p2_main / p2_tail) + 
  plot_layout(heights = c(1, 1, 1.2)) +
  plot_annotation(tag_levels = 'A', tag_suffix = ')') & 
  theme(plot.tag = element_text(face = "bold", size = 14)) # Makes the A) B) C) bold and large

# Save to PDF
ggsave("triple_stacked_labeled.pdf", plot = final_stack, width = 14, height = 16)

# Save as PNG (Raster format - best for PowerPoints and web)
# Adding dpi = 300 ensures the PNG is high resolution and not blurry
ggsave("triple_stacked_labeled.png", plot = final_stack, width = 14, height = 16, dpi = 300)

