# 1. LOAD LIBRARIES --------------------------------------------------------
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(dplyr)

# Disable S2 spherical geometry to avoid common sf errors with basin datasets
sf_use_s2(FALSE)

# 2. LOAD DATA -------------------------------------------------------------
gdb_path <- "ene_014_shale_basins/playsAndBasins/Data_02142014.gdb"

basins_all <- st_read(
  gdb_path,
  layer = "World_Unconventional_Shale_Basins_2013_v2",
  int64_as_string = TRUE,
  quiet = TRUE
)

# Geometry cleanup and projection
st_geometry(basins_all) <- "SHAPE"
basins_all <- basins_all %>%
  st_zm(drop = TRUE) %>%      
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  st_transform(4326)

# Load base world map
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# 3. SAMPLE DATA PROCESSING ------------------------------------------------
samples <- read.csv("Global_Map_lat_long_v2.csv", stringsAsFactors = FALSE)
samples$Well.Type[samples$Well.Type == "" | is.na(samples$Well.Type)] <- "Undefined"
samples$Molecular.Type[samples$Molecular.Type == "" | is.na(samples$Molecular.Type)] <- "Undefined"

# Convert to sf object and apply jittering for overlapping points
samples_sf <- st_as_sf(samples, coords = c("longitude", "latitude"), crs = 4326)
samples_sf <- st_jitter(samples_sf, amount = 0.5)

# 4. PALETTE & SCALES ------------------------------------------------------

well_colors <- c(
  "Coal Bed Methane"      = "orange",
  "Conventional Oil/Gas"  = "royalblue3",
  "Shale Gas/Oil"         = "lightskyblue"
)

mol_shapes <- c(
  "Metagenome"        = 21, 
  "Genome"            = 22, 
  "Metatranscriptome" = 23, 
  "Amplicon"          = 24, 
  "Clone Library"     = 25, 
  "qPCR"              = 4   
)

# 5. VISUALIZATION ---------------------------------------------------------

p <- ggplot() +
  geom_sf(data = world_map, fill = "white", color = "black", linewidth = 0.1) +
  
  geom_sf(
    data = basins_all,
    fill = "darkolivegreen3",      
    color = "#225522",             
    linewidth = 0.3,               
    alpha = 1.0                    
  ) +
  
  geom_sf(
    data = samples_sf,
    aes(fill = Well.Type, shape = Molecular.Type),
    color = "black",               
    size = 2.5,      
    stroke = 0.3                   
  ) +
  
  scale_fill_manual(values = well_colors, name = "Well Type") +
  scale_shape_manual(values = mol_shapes, name = "Molecular Data Type") +
  
  guides(fill = guide_legend(override.aes = list(shape = 22))) +
  
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(title = NULL, subtitle = NULL) +
  theme(
    panel.background = element_rect(fill = "steelblue3", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.03, 0.3),
    legend.justification = c(0, 0),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = NA, color = NA),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1.1, "lines"),   
    legend.key.width  = unit(1.4, "lines"),
    legend.spacing     = unit(0.5, "lines"),  
    legend.margin      = margin(8, 10, 8, 10),
    legend.box.margin  = margin(6, 6, 6, 6)
  )

# 6. SAVE ------------------------------------------------------------------

ggsave(
  "global_shale_map_solid_legend_final_v2.png",
  plot = p,
  width = 14,
  height = 9,
  units = "in",
  dpi = 300
)

ggsave(
  "global_shale_map_solid_legend_final_v2.pdf",
  plot = p,
  width = 14,
  height = 9,
  units = "in",
  device = cairo_pdf  
)
