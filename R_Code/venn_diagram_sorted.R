# 1. Install and load necessary libraries
if (!require(ggvenn)) install.packages("ggvenn")
if (!require(ggplot2)) install.packages("ggplot2")

library(ggvenn)
library(ggplot2)

# 2. Load your data
# check.names = FALSE ensures "Mouser (2016)" stays exactly as written
data <- read.table("Figure6_updated_v2.txt", header = TRUE, sep = "\t", 
                   fill = TRUE, na.strings = c("", "NA"), check.names = FALSE)

# 3. Convert columns into a list while PRESERVING ORDER
# This takes the columns from left-to-right exactly as presented in the file
column_names <- colnames(data)
venn_list <- lapply(data, function(x) x[!is.na(x)])

# 4. Generate the plot using ggvenn
# The order in 'fill_color' will map directly to the order of your columns
venn_plot <- ggvenn(
  venn_list, 
  columns = column_names,        # This forces the order from the file
  fill_color = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
  stroke_size = 0.5, 
  set_name_size = 4,
  text_size = 3.5,
  show_percentage = TRUE,        # Shows percentages automatically
  digits = 1                     # Number of decimal places for percentages
) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) #+
 # labs(title = "Venn Diagram of Sample Locations")


# 5. Save the plot
# Save as PDF (Vector format - best for zooming and publications)
ggsave("newupdate_figure6.pdf", plot = venn_plot, width = 8, height = 8, units = "in")
# Save as PNG (Raster format - best for PowerPoint/Web)
ggsave("newupdate_figure6.png", plot = venn_plot, width = 8, height = 8, units = "in", dpi = 300)

# Display the plot in RStudio
print(venn_plot)
