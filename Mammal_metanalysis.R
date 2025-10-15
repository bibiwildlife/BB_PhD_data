########location of metaanalysis#########
library(maps)
library(mapdata)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan)

plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

library(stringr)


location_analysis = read.csv ("Publication data_location analysis_csv.csv")


# Filter rows with non-NA latitude
location_analysis_formap <- location_analysis[!is.na(location_analysis$latitude), ]

# Remove "N" or "E" and convert to numeric
location_analysis_formap$latitude <- ifelse(
  grepl("S", location_analysis_formap$latitude), 
  -as.numeric(sub(" S", "", location_analysis_formap$latitude)), 
  as.numeric(sub(" N", "", location_analysis_formap$latitude))
)

location_analysis_formap$longitude <- as.numeric(sub(" E", "", location_analysis_formap$longitude))

# Check for NA values in latitude
sum(is.na(location_analysis_formap$latitude))

# Check for NA values in longitude
sum(is.na(location_analysis_formap$longitude))

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

# Get Asia map data
asia_map <- ne_countries(scale = "medium", continent = "Asia", returnclass = "sf")



# Generate a color palette with deeper, colorblind-friendly colors using RColorBrewer
color_palette <- RColorBrewer::brewer.pal(length(unique(location_analysis_formap$mammal_group)), "Dark2")

# Plot the map with colorblind-friendly deep colors and no mammal group labels
lastplot <- ggplot(data = asia_map) +
  geom_sf(fill = "lightgray", color = "white") +
  geom_point(data = location_analysis_formap, 
             aes(x = longitude, y = latitude, color = mammal_group), size = 3) +
  scale_color_manual(values = color_palette) +
  labs(
    title = "Location of Asia-wide Meta analysis",
    color = "Mammal group"
  ) +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "white", color = "white"),  # Set plot background to white
    panel.background = element_rect(fill = "white", color = "white"), # Set panel background to white
    plot.title = element_text(size = 20, face = "bold"),  # Increase title font size
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_text(size = 12),  # Keep axis labels
    axis.ticks = element_line(color = "black"),  # Keep axis ticks
    legend.title = element_text(size = 14, face = "bold"),  # Increase legend title font size
    legend.text = element_text(size = 12), # Increase legend text font size
    legend.key.size = unit(0.4, "cm"), # Adjust the size of the legend keys
    legend.spacing = unit(0.2, "cm"), # Adjust spacing between legend items
    legend.position = c(0.15, 0.2), # Position legend in the bottom-left corner (relative to the map)
    legend.background = element_rect(fill = "white", color = "black") # Add white background with black border around the legend
  )
lastplot
ggsave("asiawide_metanalysis.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")





#for biome wide data
# Load required libraries
library(sf)
library(dplyr)

# Step 1: Extract the Ecoregions2017.zip file
unzip("Ecoregions2017.zip", exdir = "biomes_data")

# Step 2: Check if extraction was successful
if (length(list.files("biomes_data", recursive = TRUE)) == 0) {
  stop("Extraction failed: No files found in 'biomes_data'.")
}

# Step 3: Load the biome shapefile
biomes <- st_read("biomes_data/Ecoregions2017.shp")

# Step 4: Load the dataset with GPS coordinates (replace with your actual file)
data <- read.csv("Publication data_location analysis_csv.csv")

#Remove rows with missing or invalid latitude/longitude values
data <- data %>% filter(!is.na(latitude) & !is.na(longitude))

convert_coords <- function(coord) {
  # Extract the numeric part
  num <- as.numeric(str_extract(coord, "[0-9.]+"))
  
  # Apply negative values for South (S) and West (W)
  if (str_detect(coord, "[SsWw]")) {
    num <- -num
  }
  
  return(num)
}

# Apply conversion function to latitude and longitude
data$latitude <- sapply(data$latitude, convert_coords)
data$longitude <- sapply(data$longitude, convert_coords)



# Step 5: Convert dataset to spatial points
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)


# Fix invalid geometries and transform to planar projection
biomes <- st_make_valid(biomes)  
biomes <- st_transform(biomes, 3857)  # Transform to EPSG:3857 (Mercator)
data_sf <- st_transform(data_sf, 3857)  

# Perform spatial join
data_with_biomes <- st_join(data_sf, biomes, left = TRUE)

# Transform back to lat/lon (EPSG:4326)
data_with_biomes <- st_transform(data_with_biomes, 4326)

result <- data_with_biomes %>%
  dplyr::select(any_of(c(
    "author_of_the_papers", "year_of_publishing", "paper_heading", 
    "mammal_species", "mammal_group", "country", 
    "BIOME_NAME", "ECO_NAME", "REALM", "SHAPE_LENG", "SHAPE_AREA", "geometry"
  )))

# Save results to CSV
write.csv(result, "gps_with_biomes.csv", row.names = FALSE)





#making a map with habitat
library(maps)
library(mapdata)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan)


library(stringr)


location_analysis = read.csv ("Location_final.csv")


# Filter rows with non-NA latitude
location_analysis_formap <- location_analysis[!is.na(location_analysis$latitude), ]

# Remove "N" or "E" and convert to numeric
location_analysis_formap$latitude <- ifelse(
  grepl("S", location_analysis_formap$latitude), 
  -as.numeric(sub(" S", "", location_analysis_formap$latitude)), 
  as.numeric(sub(" N", "", location_analysis_formap$latitude))
)

location_analysis_formap$longitude <- as.numeric(sub(" E", "", location_analysis_formap$longitude))

# Check for NA values in latitude
sum(is.na(location_analysis_formap$latitude))

# Check for NA values in longitude
sum(is.na(location_analysis_formap$longitude))

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

# Get Asia map data
asia_map <- ne_countries(scale = "medium", continent = "Asia", returnclass = "sf")



# Generate a color palette with deeper, colorblind-friendly colors using RColorBrewer
color_palette <- RColorBrewer::brewer.pal(length(unique(location_analysis_formap$Habitat)), "Dark2")

# Plot the map with colorblind-friendly deep colors and no mammal group labels
lastplot <- ggplot(data = asia_map) +
  geom_sf(fill = "lightgray", color = "white") +
  geom_point(data = location_analysis_formap, 
             aes(x = longitude, y = latitude, color = Habitat), size = 3) +
  scale_color_manual(values = color_palette) +
  labs(
    color = "Habitat"
  ) +
  theme_classic() + 
  theme(
    plot.background = element_rect(fill = "white", color = "white"),  # Set plot background to white
    panel.background = element_rect(fill = "white", color = "white"), # Set panel background to white
    plot.title = element_text(size = 20, face = "bold"),  # Increase title font size
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_text(size = 12),  # Keep axis labels
    axis.ticks = element_line(color = "black"),  # Keep axis ticks
    legend.title = element_text(size = 14, face = "bold"),  # Increase legend title font size
    legend.text = element_text(size = 12), # Increase legend text font size
    legend.key.size = unit(0.4, "cm"), # Adjust the size of the legend keys
    legend.spacing = unit(0.2, "cm"), # Adjust spacing between legend items
    legend.position = c(0.15, 0.2), # Position legend in the bottom-left corner (relative to the map)
    legend.background = element_rect(fill = "white", color = "black") # Add white background with black border around the legend
  )
lastplot
ggsave("asiawide_metanalysis_habitat.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")





#######understanding of biases########
library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan)

plant_trait = read.csv ("Mammal review_for analysis_csv.csv")


#remove all the duplicates at mammal_species and plant_species level
plant_trait_final <- plant_trait %>%
  distinct(mammal_species, updated_species, .keep_all = TRUE)



library(ggplot2)

ggplot(plant_trait_final, aes(x = Asia_zone)) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(plant_trait_final, aes(x = Habitat)) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(forcats)  # For reordering factor levels

ggplot(plant_trait_final, aes(x = fct_infreq(mammal_species))) + #reordering the factor level in desecnding order
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(plant_trait_final, aes(x = fct_infreq(mammal_genus))) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(plant_trait_final, aes(x = fct_infreq(location_country))) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(plant_trait_final, aes(x = fct_infreq(mammal_type))) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(plant_trait_final, aes(x = fct_infreq(mammal_group))) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#remove all the duplicates at mammal species, plant_species and habitat level
plant_trait_habitat <- plant_trait %>%
  distinct(mammal_species, updated_species, Habitat, .keep_all = TRUE)

ggplot(plant_trait_habitat, aes(x = Habitat)) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#for each habitat, what are the unique species
library(ggplot2)
library(dplyr)
library(ggthemes)  # For colorblind-friendly palettes


plant_trait_habitat <- plant_trait_habitat[!is.na(plant_trait_habitat$Habitat), ]


library(ggplot2)

plot <- ggplot(plant_trait_habitat, aes(x = mammal_type, fill = mammal_type)) +
  geom_bar() +
  scale_fill_viridis_d(option = "C") +  # Colorblind-friendly color palette
  theme_classic(base_size = 16) +  # Larger text for readability
  labs(y = "Count") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis label
    panel.background = element_rect(fill = "white", color = NA),  # White background
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "gray90", color = "black"),  # Border for facet labels
    strip.text = element_text(face = "bold"),  # Bold text for facet labels
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # ADD BORDER TO FACETS
  ) +
  facet_wrap(~ Habitat)  # Separate facets for each habitat

plot

# Save the plot with a white background
ggsave("mammal_types_asia_habitat.png", plot, width = 12, height = 6, dpi = 300, bg = "white")







library(ggplot2)
library(dplyr)
library(ggthemes)  # For colorblind-friendly palettes

# Create the plot
library(ggplot2)
library(dplyr)

plot <- plant_trait_final %>%
  mutate(mammal_group = factor(mammal_group)) %>%  # Ensure mammal_group is a factor
  ggplot(aes(x = mammal_group, fill = mammal_group)) +
  geom_bar() +
  scale_fill_viridis_d(option = "C") +  # Colorblind-friendly color palette
  theme_bw(base_size = 14) +  # Use theme_bw() to get facet borders
  labs( y = "Count") +  # Fixed missing comma
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis label
    strip.background = element_rect(fill = "gray90", color = "black"),  # Facet label background
    strip.text = element_text(face = "bold"),  # Bold facet labels
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # ADD BLACK BORDER TO EACH FACET
  ) +
  facet_wrap(~ mammal_type, scales = "free_x")  # Separate facets for each mammal type

# Save the plot with a white background
ggsave("mammal_family_asia.png", plot, width = 12, height = 6, dpi = 300, bg = "white")



######## proportion of plant family/genus in mammal group diets############
library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan)

plant_trait = read.csv ("Mammal review_for analysis_csv.csv")


#remove all the duplicates at mammal_species and plant_species level
plant_trait_final <- plant_trait %>%
  distinct(mammal_scientific_name, updated_species, .keep_all = TRUE)




#family
library(dplyr)
library(ggplot2)
library(forcats)

# 1. Prepare the data (distinct species, filter for NA families, calculate frequency)
Plant_family_analysis <- plant_trait %>%
  distinct(mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(family)) %>%
  group_by(mammal_type, family) %>%
  summarise(family_freq = n(), .groups = 'drop') %>%
  mutate(family_perc = family_freq / sum(family_freq))  # Proportion (0 to 1)


# Count the number of unique plant species each mammal type eats
species_count <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  summarise(num_species = n_distinct(family))

# View results
print(species_count)



# 2. Select the top 5 families for each mammal type based on family frequency
Plant_family_analysis_top5 <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  arrange(desc(family_freq)) %>%
  slice_head(n = 5) %>%
  ungroup()

# 3. Calculate the relative proportion of each family within each mammal_type
Plant_family_analysis_top5 <- Plant_family_analysis_top5 %>%
  group_by(mammal_type) %>%
  mutate(family_perc = family_freq / sum(family_freq))  # Proportion (0 to 1)

# 4. Reorder the `family` levels by `family_perc` within each `mammal_type`
Plant_family_analysis_top5 <- Plant_family_analysis_top5 %>%
  arrange(mammal_type, desc(family_perc)) %>%
  mutate(family = factor(family, levels = unique(family)))

# 5. Create the stacked bar graph with the Dark2 color-blind friendly palette
lastplot_family <- ggplot(Plant_family_analysis_top5, aes(x = mammal_type, y = family_perc, fill = family)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Plant Family"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Larger x-axis text
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    axis.text.y = element_text(size = 14),  # Larger y-axis text
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 16),  # Larger legend title
    legend.text = element_text(size = 14)  # Larger legend text
  ) +
  scale_fill_brewer(palette = "Dark2") +  # Apply Dark2 color-blind friendly palette
  scale_y_continuous(labels = scales::percent)  # Display y-axis as percentage

# Display the plot
lastplot_family


custom_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", 
                   "#E6AB02", "#A6761D", "#666666", "#FF5733", "#33FF57")

lastplot <- lastplot +
  scale_fill_manual(values = custom_colors)


ggsave("asiawide_metanalysis_Family.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")





##Genus##

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(forcats)

# 1. Prepare the data (distinct species, filter for NA families, calculate frequency)
Plant_family_analysis <- plant_trait %>%
  distinct(mammal_type, updated_genus, .keep_all = TRUE) %>%
  filter(!is.na(family)) %>%
  group_by(mammal_type, updated_genus) %>%
  summarise(genus_freq = n(), .groups = 'drop')


# Count the number of unique plant species each mammal type eats
species_count <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  summarise(num_species = n_distinct(updated_genus))

# View results
print(species_count)

Plant_family_analysis %>% distinct(updated_genus)

# 2. Select the top 5 genera for each mammal type based on genus frequency
Plant_family_analysis_top5 <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  arrange(desc(genus_freq)) %>%
  slice_head(n = 5) %>%
  ungroup()  # Ensure no grouping for the next steps

# 3. Calculate the relative proportion of each genus within each mammal_type
Plant_family_analysis_top5 <- Plant_family_analysis_top5 %>%
  group_by(mammal_type) %>%
  mutate(genus_perc = genus_freq / sum(genus_freq))  # proportion (0 to 1)

# 4. Sort mammal types in descending order based on the total genus_freq
Plant_family_analysis_top5 <- Plant_family_analysis_top5 %>%
  group_by(mammal_type) %>%
  mutate(total_genus_freq = sum(genus_freq)) %>%
  ungroup() %>%
  arrange(desc(total_genus_freq), mammal_type)  # Sorting by total frequency

# Reorder `updated_genus` for each `mammal_type` based on `genus_perc`
Plant_family_analysis_top5 <- Plant_family_analysis_top5 %>%
  group_by(mammal_type) %>%
  mutate(updated_genus = fct_reorder(updated_genus, genus_perc, .desc = TRUE)) %>%
  ungroup()



custom_palette <- c(RColorBrewer::brewer.pal(8, "Dark2"), 
                    viridis::viridis_pal(option = "D")(20)[1:20])

lastplot_genus = ggplot(Plant_family_analysis_top5, aes(x = mammal_type, y = genus_perc, fill = updated_genus)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Plant Genus"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  scale_fill_manual(values = custom_palette) +  # Apply the custom color palette
  scale_y_continuous(labels = scales::percent)


lastplot_genus


ggsave("asiawide_metanalysis_Genus.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")



library(ggplot2)
library(patchwork)

# Add thin black border (box) around each plot
lastplot_family_boxed <- lastplot_family + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3)  # Thin border
  )

lastplot_genus_boxed <- lastplot_genus + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.3)
  )

library(patchwork)
library(grid)

library(patchwork)

# Add tags to your plots
lastplot_family_boxed <- lastplot_family_boxed + 
  labs(tag = "(a)") +
  theme(plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0, 1))  # Top-left corner

lastplot_genus_boxed <- lastplot_genus_boxed + 
  labs(tag = "(b)") +
  theme(plot.tag = element_text(size = 18, face = "bold"),
        plot.tag.position = c(0, 1))  # Top-left corner

# Create divider (using the most reliable method)
thin_line <- ggplot() +
  annotate("segment", x = 0, xend = 0, y = -Inf, yend = Inf, 
           color = "black", linewidth = 0.5) +
  theme_void()

# Combine everything
combined_plot <- lastplot_family_boxed + thin_line + lastplot_genus_boxed +
  plot_layout(ncol = 3, widths = c(1, 0.02, 1))

# Result will now show (a) and (b) labels
combined_plot
# Save
ggsave("combined_plots_genus and family percentage.png", combined_plot, width = 18, height = 10, dpi = 300)


####### Fruit colours ###########
# Load required libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# Assign colors to `colour_nature`
fruit_colors <- c(
  "Light" = "#A65628",
  "Bright" = "#E41A1C",
  "Dark" = "#377EB8"
)

#for ficus
Plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(updated_genus %in% c("Ficus", "ficus")) %>%
  filter(!is.na(colour_nature)) %>%
  mutate(colour_nature = str_trim(colour_nature))  


# Create a contingency table (count occurrences)
chi_table <- table(Plant_trait_ficus$mammal_type, Plant_trait_ficus$colour_nature)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)


# Data preparation
Fruit_colour <- Plant_trait_ficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(colour_nature)) %>%  # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(mammal_type, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(colour_perc))  # Arrange by descending proportion



# Create the stacked bar graph
lastplot <- ggplot(Fruit_colour, aes(x = mammal_type, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit Colour"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 30, hjust = 1, color = "black"),  # Larger x-axis text
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.text.y = element_text(size = 30, color = "black"),  # Larger y-axis text
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 30, color = "black"),  # Larger legend title
    legend.text = element_text(size = 30, color = "black")  # Larger legend text
  ) +
  scale_fill_manual(values = fruit_colors) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent)  # Display y-axis as percentage

# Display the plot
print(lastplot)


ggsave("asiawide_metanalysis_ficus_Fruitcolour.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")



#nonficus
# Assign colors to `colour_nature`
fruit_colors <- c(
  "Light" = "#A65628",
  "Bright" = "#E41A1C",
  "Dark" = "#377EB8"
)


Plant_trait_nonficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!updated_genus %in% c("Ficus", "ficus")) %>%
  filter(!is.na(colour_nature))


# Create a contingency table (count occurrences)
chi_table <- table(Plant_trait_nonficus$mammal_type, Plant_trait_nonficus$colour_nature)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)



# Data preparation
Fruit_colour <- Plant_trait_nonficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(colour_nature)) %>%  # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(mammal_type, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(colour_perc))  # Arrange by descending proportion

# Create the stacked bar graph
lastplot_nonficus <- ggplot(Fruit_colour, aes(x = mammal_type, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit Colour"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 30, hjust = 1, color = "black"),  # Larger x-axis text
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.text.y = element_text(size = 30, color = "black"),  # Larger y-axis text
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 30, color = "black"),  # Larger legend title
    legend.text = element_text(size = 30, color = "black")  # Larger legend text
  ) +
  scale_fill_manual(values = fruit_colors) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent)  # Display y-axis as percentage

# Display the plot
print(lastplot_nonficus)


ggsave("asiawide_metanalysis_nonficus_Fruitcolour.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")






#for different habitat
# Assign colors to `colour_nature`
fruit_colors <- c(
  "Light" = "#A65628",
  "Bright" = "#E41A1C",
  "Dark" = "#377EB8"
)


plant_trait_nonficus_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!updated_genus %in% c("Ficus", "ficus")) %>%
  filter(!is.na(Habitat))

interaction_count <- plant_trait_nonficus_habitat %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


interaction_count <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, Habitat) %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# Data preparation
Fruit_colour <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(Habitat)) %>% 
  filter(!is.na(colour_nature)) %>% # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(mammal_type, Habitat, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type, Habitat) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(colour_perc))  # Arrange by descending proportion

# Load required library
library(dplyr)

# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_colour$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_colour %>%
    filter(Habitat == habitat) %>%
    dplyr::select(mammal_type, colour_nature, colour_freq)  # Select relevant columns
  
  # Convert to contingency table
  chi_table <- xtabs(colour_freq ~ mammal_type + colour_nature, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results


# For percentage labels

library(ggplot2)
library(dplyr)
library(stringr)
library(scales)



lastplot_nonficus_habitat <- ggplot(Fruit_colour, aes(x = mammal_type, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity") +  # Stacked bar
  facet_wrap(~ Habitat, scales = "free_x") +  # Facet by Asia_zone
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit Colour"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 30, hjust = 0.5, color = "black" ),  # Rotate x-axis text
    axis.title.y = element_text(size = 30 , color = "black"),  # Larger y-axis title
    axis.text.y = element_text(size = 30, color = "black"),  # Larger y-axis text
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 30, color = "black"),  # Larger legend title
    legend.text = element_text(size = 30, color = "black"),  # Larger legend text
    strip.text = element_text(size = 30, face = "bold"),  # Bigger facet titles
    panel.spacing = unit(1, "lines"),  # Space between facets
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black border around facets
  ) +
  scale_fill_manual(values = fruit_colors) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent_format())  # Ensure y-axis shows % correctly

print(lastplot_nonficus_habitat)


ggsave("asiawide_metanalysis_nonficus_habitat_Fruitcolour.jpg", 
       lastplot, 
       width = 15, height = 8, dpi = 300, bg = "white")


#for ficus
# Assign colors to `colour_nature`
fruit_colors <- c(
  "Light" = "#A65628",
  "Bright" = "#E41A1C",
  "Dark" = "#377EB8"
)


plant_trait_ficus_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(updated_genus %in% c("Ficus", "ficus")) %>%
  filter(!is.na(Habitat))

interaction_count <- plant_trait_ficus_habitat %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


interaction_count <- plant_trait_ficus_habitat %>%
  distinct(mammal_species, Habitat) %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# Data preparation
Fruit_colour <- plant_trait_ficus_habitat %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(Habitat)) %>% 
  filter(!is.na(colour_nature)) %>% # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(mammal_type, Habitat, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type, Habitat) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(colour_perc))  # Arrange by descending proportion

# Load required library
library(dplyr)

# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_colour$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_colour %>%
    filter(Habitat == habitat) %>%
    select(mammal_type, colour_nature, colour_freq)  # Select relevant columns
  
  # Convert to contingency table
  chi_table <- xtabs(colour_freq ~ mammal_type + colour_nature, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results


# For percentage labels

library(ggplot2)
library(dplyr)
library(stringr)
library(scales)



lastplot_ficus_habitat <- ggplot(Fruit_colour, aes(x = mammal_type, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity") +  # Stacked bar
  facet_wrap(~ Habitat, scales = "free_x") +  # Facet by Asia_zone
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit Colour"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 30, hjust = 0.5, color = "black"),  # Rotate x-axis text
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.text.y = element_text(size = 30, color = "black"),  # Larger y-axis text
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 30, color = "black"),  # Larger legend title
    legend.text = element_text(size = 30, color = "black"),  # Larger legend text
    strip.text = element_text(size = 30, face = "bold"),  # Bigger facet titles
    panel.spacing = unit(1, "lines"),  # Space between facets
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black border around facets
  ) +
  scale_fill_manual(values = fruit_colors) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent_format())  # Ensure y-axis shows % correctly

print(lastplot_ficus_habitat)


ggsave("asiawide_metanalysis_ficus_habitat_Fruitcolour.jpg", 
       lastplot, 
       width = 15, height = 8, dpi = 300, bg = "white")


#make all the fruit size graphs in one panel
library(ggplot2)
library(patchwork)

# Make sure ALL your plots have same legend & theme but suppress legend there
lastplot <- lastplot + theme(legend.position = "none")
lastplot_nonficus <- lastplot_nonficus + theme(legend.position = "none")
lastplot_ficus_habitat <- lastplot_ficus_habitat + theme(legend.position = "none")
lastplot_nonficus_habitat <- lastplot_nonficus_habitat + theme(legend.position = "none")

# Add labels
lastplot <- lastplot + labs(tag = "A)")
lastplot_nonficus <- lastplot_nonficus + labs(tag = "B)")
lastplot_ficus_habitat <- lastplot_ficus_habitat + labs(tag = "C)")
lastplot_nonficus_habitat <- lastplot_nonficus_habitat + labs(tag = "D)")

# Combine with patchwork (this pulls one legend automatically!)
combined_plot <- (lastplot | lastplot_ficus_habitat) /
  (lastplot_nonficus | lastplot_nonficus_habitat) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 30, face = "bold", color = "black"))  # Optional styling

combined_plot


# Save to file with wider width
ggsave("combined_plot_fruit colour_shared_legend.png", plot = combined_plot,
       width = 42, height = 20, dpi = 300)


#######Fleshy/Dry fruits##########
#For nonficus
# 1. Prepare the data (distinct species, filter for NA families, calculate frequency)
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")


Plant_trait_nonficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(fruit_nature))

# Create a contingency table (count occurrences)
chi_table <- table(Plant_trait_nonficus$mammal_type, Plant_trait_nonficus$fruit_nature)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)


Plant_family_analysis <- Plant_trait_nonficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_nature)) %>%
  group_by(mammal_type, fruit_nature) %>%
  summarise(genus_freq = n(), .groups = 'drop')


# 3. Calculate the relative proportion of each genus within each mammal_type
Plant_family_analysis <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  mutate(genus_perc = genus_freq / sum(genus_freq))  # proportion (0 to 1)



custom_palette <- c(RColorBrewer::brewer.pal(8, "Dark2"), 
                    viridis::viridis_pal(option = "D")(20)[1:20])

lastplot = ggplot(Plant_family_analysis, aes(x = mammal_type, y = genus_perc, fill = fruit_nature)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit type"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 30, hjust = 0.5),
    axis.title.y = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 30),
    legend.text = element_text(size = 30)
  ) +
  scale_fill_manual(values = custom_palette) +  # Apply the custom color palette
  scale_y_continuous(labels = scales::percent)


lastplot

ggsave("Fruit type_nonficus.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")




#for different habitat
plant_trait_nonficus_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(habit)) %>%
  filter(!is.na(Habitat))


interaction_count <- plant_trait_nonficus_habitat %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


interaction_count <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, Habitat) %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)







# Data preparation
Fruit_type <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(fruit_nature)) %>%  # Filter out NA values
  mutate(fruit_nature = str_trim(fruit_nature)) %>%  # Trim white spaces
  group_by(mammal_type, Habitat, fruit_nature) %>%
  summarise(type_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type, Habitat) %>%
  mutate(type_perc = type_freq / sum(type_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(type_perc))  # Arrange by descending proportion


# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_type$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_type %>%
    filter(Habitat == habitat) %>%
    select(mammal_type, fruit_nature, type_freq)  # Use type_freq, NOT type_perc
  
  # Convert to contingency table
  chi_table <- xtabs(type_freq ~ mammal_type + fruit_nature, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results




# For percentage labels

library(ggplot2)
library(dplyr)
library(stringr)
library(scales)




lastplot <- ggplot(Fruit_type, aes(x = mammal_type, y = type_perc, fill = fruit_nature)) +
  geom_bar(stat = "identity") +  # Stacked bar
  facet_wrap(~ Habitat, scales = "free_x") +  # Facet by Asia_zone
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit type"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 30, hjust = 1),  # Rotate x-axis text
    axis.title.y = element_text(size = 30),  # Larger y-axis title
    axis.text.y = element_text(size = 30),  # Larger y-axis text
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 30),  # Larger legend title
    legend.text = element_text(size = 30),  # Larger legend text
    strip.text = element_text(size = 30, face = "bold"),  # Bigger facet titles
    panel.spacing = unit(1, "lines"),  # Space between facets
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black border around facets
  ) +
  scale_fill_manual(values = custom_palette) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent_format())  # Ensure y-axis shows % correctly

print(lastplot)


ggsave("asiawide_metanalysis_nonficus_habitat_Fruittype.jpg", 
       lastplot, 
       width = 15, height = 8, dpi = 300, bg = "white")

########for arillate not-arillate###########
#For nonficus
# 1. Prepare the data (distinct species, filter for NA families, calculate frequency)
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")


Plant_trait_nonficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(arillate_notarillate))


Plant_family_analysis <- Plant_trait_nonficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(arillate_notarillate)) %>%
  group_by(mammal_type, arillate_notarillate) %>%
  summarise(genus_freq = n(), .groups = 'drop')


# Create a contingency table (count occurrences)
chi_table <- table(Plant_trait_nonficus$mammal_type, Plant_trait_nonficus$arillate_notarillate)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)


# 3. Calculate the relative proportion of each genus within each mammal_type
Plant_family_analysis <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  mutate(genus_perc = genus_freq / sum(genus_freq))  # proportion (0 to 1)



custom_palette <- c(RColorBrewer::brewer.pal(8, "Dark2"), 
                    viridis::viridis_pal(option = "D")(20)[1:20])

lastplot = ggplot(Plant_family_analysis, aes(x = mammal_type, y = genus_perc, fill = arillate_notarillate)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Aril presence"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14, hjust = 1),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  scale_fill_manual(values = custom_palette) +  # Apply the custom color palette
  scale_y_continuous(labels = scales::percent)


lastplot

ggsave("arillate_nonficus.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")







#different habitat
plant_trait_nonficus_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(arillate_notarillate)) %>%
  filter(!is.na(Habitat))


interaction_count <- plant_trait_nonficus_habitat %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


interaction_count <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, Habitat) %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)







# Data preparation
Fruit_type <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(arillate_notarillate)) %>%  # Filter out NA values
  mutate(arillate_notarillate = str_trim(arillate_notarillate)) %>%  # Trim white spaces
  group_by(mammal_type, Habitat, arillate_notarillate) %>%
  summarise(type_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type, Habitat) %>%
  mutate(type_perc = type_freq / sum(type_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(type_perc))  # Arrange by descending proportion


# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_type$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_type %>%
    filter(Habitat == habitat) %>%
    select(mammal_type, arillate_notarillate, type_freq)  # Use type_freq, NOT type_perc
  
  # Convert to contingency table
  chi_table <- xtabs(type_freq ~ mammal_type + arillate_notarillate, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results




# For percentage labels

library(ggplot2)
library(dplyr)
library(stringr)
library(scales)




lastplot <- ggplot(Fruit_type, aes(x = mammal_type, y = type_perc, fill = arillate_notarillate)) +
  geom_bar(stat = "identity") +  # Stacked bar
  facet_wrap(~ Habitat, scales = "free_x") +  # Facet by Asia_zone
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "fruit arillation"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 14, hjust = 1),  # Rotate x-axis text
    axis.title.y = element_text(size = 16),  # Larger y-axis title
    axis.text.y = element_text(size = 14),  # Larger y-axis text
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 16),  # Larger legend title
    legend.text = element_text(size = 14),  # Larger legend text
    strip.text = element_text(size = 18, face = "bold"),  # Bigger facet titles
    panel.spacing = unit(1, "lines"),  # Space between facets
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black border around facets
  ) +
  scale_fill_manual(values = custom_palette) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent_format())  # Ensure y-axis shows % correctly

print(lastplot)


ggsave("asiawide_metanalysis_nonficus_habitat_arillatenotarillate.jpg", 
       lastplot, 
       width = 15, height = 8, dpi = 300, bg = "white")




###########for habit#########
#For nonficus
# 1. Prepare the data (distinct species, filter for NA families, calculate frequency)


plant_trait = read.csv ("Mammal review_for analysis_csv.csv")


Plant_trait_nonficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(habit))


Plant_family_analysis <- Plant_trait_nonficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(habit)) %>%
  group_by(mammal_type, habit) %>%
  summarise(genus_freq = n(), .groups = 'drop')


# Create a contingency table (count occurrences)
chi_table <- table(Plant_trait_nonficus$mammal_type, Plant_trait_nonficus$Habit)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)



interaction_count <- Plant_trait_nonficus %>%
  distinct(mammal_species) %>%
  group_by(mammal_species) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# 3. Calculate the relative proportion of each genus within each mammal_type
Plant_family_analysis <- Plant_family_analysis %>%
  group_by(mammal_type) %>%
  mutate(genus_perc = genus_freq / sum(genus_freq))  # proportion (0 to 1)



custom_palette <- c(RColorBrewer::brewer.pal(8, "Dark2"), 
                    viridis::viridis_pal(option = "D")(20)[1:20])

lastplot = ggplot(Plant_family_analysis, aes(x = mammal_type, y = genus_perc, fill = habit)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Habit"
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 30, hjust = 1, color = "black"),
    axis.title.y = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 30, color = "black"),
    legend.text = element_text(size = 30, color = "black")
  ) +
  scale_fill_manual(values = custom_palette) +  # Apply the custom color palette
  scale_y_continuous(labels = scales::percent)


lastplot

ggsave("habit_nonficus.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")


#for different habitat
plant_trait_nonficus_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(habit)) %>%
  filter(!is.na(Habitat))


interaction_count <- plant_trait_nonficus_habitat %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


interaction_count <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, Habitat) %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)







# Data preparation
Fruit_type <- plant_trait_nonficus_habitat %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(habit)) %>%  # Filter out NA values
  mutate(habit = str_trim(habit)) %>%  # Trim white spaces
  group_by(mammal_type, Habitat, habit) %>%
  summarise(type_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(mammal_type, Habitat) %>%
  mutate(type_perc = type_freq / sum(type_freq)) %>%  # Calculate proportion
  arrange(mammal_type, desc(type_perc))  # Arrange by descending proportion


# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_type$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_type %>%
    filter(Habitat == habitat) %>%
    select(mammal_type, habit, type_freq)  # Use type_freq, NOT type_perc
  
  # Convert to contingency table
  chi_table <- xtabs(type_freq ~ mammal_type + habit, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results




# For percentage labels

library(ggplot2)
library(dplyr)
library(stringr)
library(scales)




lastplot_nonficus_habitat <- ggplot(Fruit_type, aes(x = mammal_type, y = type_perc, fill = habit)) +
  geom_bar(stat = "identity") +  # Stacked bar
  facet_wrap(~ Habitat, scales = "free_x") +  # Facet by Asia_zone
  theme_classic() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Habit"
  ) +
  theme(
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_text(size = 30, hjust = 0.5, color = "black"),  # Rotate x-axis text
    axis.title.y = element_text(size = 30, color = "black"),  # Larger y-axis title
    axis.text.y = element_text(size = 30, color = "black"),  # Larger y-axis text
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),  # Larger and bold plot title
    legend.title = element_text(size = 30, color = "black"),  # Larger legend title
    legend.text = element_text(size = 30, color = "black"),  # Larger legend text
    strip.text = element_text(size = 30, face = "bold"),  # Bigger facet titles
    panel.spacing = unit(1, "lines"),  # Space between facets
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)  # Add black border around facets
  ) +
  scale_fill_manual(values = custom_palette) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent_format())  # Ensure y-axis shows % correctly

print(lastplot_nonficus_habitat)


ggsave("asiawide_metanalysis_nonficus_habitat_habit.jpg", 
       lastplot, 
       width = 15, height = 8, dpi = 300, bg = "white")


library(ggplot2)
library(patchwork)

# Remove existing titles and legends
lastplot <- lastplot + theme(legend.position = "none") + labs(title = NULL)
lastplot_nonficus_habitat <- lastplot_nonficus_habitat + theme(legend.position = "none") + labs(title = NULL)

# Add custom tags using the 'tag' argument inside each plot
lastplot <- lastplot + labs(tag = "A)")
lastplot_nonficus_habitat <- lastplot_nonficus_habitat + labs(tag = "B)")

# Combine with shared legend at bottom
combined_plot <- (lastplot | lastplot_nonficus_habitat) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 30, face = "bold", color = "black"))  # Optional styling

# View it
combined_plot



# Save to file with wider width
ggsave("combined_plot_fruit habit_shared_legend.png", plot = combined_plot,
       width = 42, height = 20, dpi = 300)








#########Fruit size#########
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")
boot_function <- function(c) {
  data = c
  mn = numeric(1000)
  for (i in 1:1000) {
    temp = sample(data, replace = TRUE)
    mn[i] = mean(temp, na.rm = TRUE)
  }
  lci = quantile(mn, 0.025, na.rm = TRUE)
  boot_mean = median(mn, na.rm = TRUE)  # Renamed from "mean" to avoid conflicts
  rci = quantile(mn, 0.975, na.rm = TRUE)
  results = c(lci, boot_mean, rci)
  return(results)
}

library(dplyr)
library(stringr)
library(purrr)



plant_trait_ficus <- plant_trait %>%
  dplyr::filter(fruit_nature %in% c("Fig", "fig"))



Fruit_length <- plant_trait_ficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_length_l) & !is.na(fruit_length_h)) %>%
  mutate(fruit_length = (fruit_length_l + fruit_length_h) / 2)

Fruit_length_boot <- Fruit_length %>%
  group_by(mammal_species, mammal_type) %>%
  summarise(
    boot_results = list(boot_function(fruit_length)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitlength = map_dbl(boot_results, 1),
    Mean_fruitlength = map_dbl(boot_results, 2),
    Hci_fruitlength = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)

Fruit_length_final_ficus <- Fruit_length_boot %>%
  group_by(mammal_type) %>%
  summarise(
    Mean_fruitlength_f = mean(Mean_fruitlength, na.rm = TRUE),
    SD_fruitlength_f = sd(Fruit_length_boot$Mean_fruitlength, na.rm = TRUE),
    SE_fruitlength_f = SD_fruitlength_f / sqrt(n()),
    lci_fruitlength_f = Mean_fruitlength_f - 1.96 * SE_fruitlength_f,
    Hci_fruitlength_f = Mean_fruitlength_f + 1.96 * SE_fruitlength_f
  )


library(ggplot2)  

plot<- ggplot(Fruit_length_final_ficus, aes(x = mammal_type, y = Mean_fruitlength_f)) +
  geom_point(size = 4, color = "black") +  
  geom_errorbar(aes(ymin = lci_fruitlength_f, ymax = Hci_fruitlength_f), width = 0.2) +  
  theme_classic() +  # Removes background and grid lines
  labs(x = "Mammal Type", y = "Mean Fruit length (±95% CI)") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 17)
  )

ggsave("asiawide_metanalysis_length_ficus.jpg", 
       plot, 
       width = 10, height = 8, dpi = 300, bg = "white")

#for nonficus
boot_function <- function(c) {
  data = c
  mn = numeric(1000)
  for (i in 1:1000) {
    temp = sample(data, replace = TRUE)
    mn[i] = mean(temp, na.rm = TRUE)
  }
  lci = quantile(mn, 0.025, na.rm = TRUE)
  boot_mean = median(mn, na.rm = TRUE)  # Renamed from "mean" to avoid conflicts
  rci = quantile(mn, 0.975, na.rm = TRUE)
  results = c(lci, boot_mean, rci)
  return(results)
}


plant_trait_nonficus <- plant_trait %>%
  dplyr::filter(!fruit_nature %in% c("Fig", "fig"))

Fruit_length <- plant_trait_nonficus %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_length_l) & !is.na(fruit_length_h)) %>%
  mutate(fruit_length = (fruit_length_l + fruit_length_h) / 2)

Fruit_length_boot <- Fruit_length %>%
  group_by(mammal_species, mammal_type) %>%
  summarise(
    boot_results = list(boot_function(fruit_length)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitlength = map_dbl(boot_results, 1),
    Mean_fruitlength = map_dbl(boot_results, 2),
    Hci_fruitlength = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)

Fruit_length_final <- Fruit_length_boot %>%
  group_by(mammal_type) %>%
  summarise(
    Mean_fruitlength_nf = mean(Mean_fruitlength, na.rm = TRUE),
    SD_fruitlength_nf = sd(Fruit_length_boot$Mean_fruitlength, na.rm = TRUE),
    SE_fruitlength_nf = SD_fruitlength_nf / sqrt(n()),
    lci_fruitlength_nf = Mean_fruitlength_nf - 1.96 * SE_fruitlength_nf,
    Hci_fruitlength_nf = Mean_fruitlength_nf + 1.96 * SE_fruitlength_nf
  )




plot<- ggplot(Fruit_length_final, aes(x = mammal_type, y = Mean_fruitlength_nf)) +
  geom_point(size = 4, color = "black") +  
  geom_errorbar(aes(ymin = lci_fruitlength_nf, ymax = Hci_fruitlength_nf), width = 0.2) +  
  theme_classic() +  # Removes background and grid lines
  labs(x = "Mammal Type", y = "Mean Fruit length (±95% CI)") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 17)
  )

ggsave("asiawide_metanalysis_length_nonficus.jpg", 
       plot, 
       width = 10, height = 8, dpi = 300, bg = "white")




Fruit_length_combined <- left_join(Fruit_length_final_ficus, Fruit_length_final, by = "mammal_type")
library(tidyr)
library(dplyr)

Fruit_length_long <- Fruit_length_combined %>%
  pivot_longer(
    cols = c(Mean_fruitlength_f, Mean_fruitlength_nf, 
             lci_fruitlength_f, lci_fruitlength_nf, 
             Hci_fruitlength_f, Hci_fruitlength_nf), 
    names_to = c("Metric", "Dataset"), 
    names_pattern = "(.*)_(f|nf)"
  ) %>%
  mutate(Dataset = recode(Dataset, "f" = "Ficus", "nf" = "Non Ficus")) %>%
  pivot_wider(names_from = Metric, values_from = value)

Fruit_length_long <- Fruit_length_long %>%
  mutate(mammal_type = recode(mammal_type, 
                              "Carnivore" = "Carnivores", 
                              "Herbivore" = "Herbivores"))


library(ggplot2)

plot_whole<- ggplot(Fruit_length_long, aes(x = mammal_type, y = Mean_fruitlength, color = Dataset)) +
  # Points
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  # Error bars
  geom_errorbar(aes(ymin = lci_fruitlength, ymax = Hci_fruitlength), 
                width = 0.2, position = position_dodge(width = 0.6)) +
  
  # Customize colors for colorblind accessibility
  scale_color_manual(values = c("Ficus" = "#D55E00", "Non Ficus" = "#0072B2")) +  
  
  theme_classic() + 
  labs(x = "Mammal Type", y = "Mean Fruit length (±95% CI)", color = NULL) +  # Removed color label
  
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 30, color = "black"),  # Horizontal x-axis text
    axis.text.y = element_text(size = 30, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, color = "black"),
    panel.grid = element_blank(),
    
    # Increase font size of color labels in legend
    legend.text = element_text(size = 30)  
  )


ggsave("asiawide_metanalysis_Fruitlength_all.jpg", 
       plot, 
       width = 10, height = 8, dpi = 300, bg = "white")




#fruit length for habitat zone
#for nonficus

# Ensure distinct plant traits by species, type, and zone


plant_trait_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE)

# Keep only non-Ficus species within specific zones
plant_trait_nonficus_habitat <- plant_trait_habitat %>%
  filter(!fruit_nature %in% c("Fig", "fig"))

# Calculate mean fruit length
Fruit_length <- plant_trait_nonficus_habitat %>%
  filter(!is.na(fruit_length_l) & !is.na(fruit_length_h)) %>%
  mutate(fruit_length = (fruit_length_l + fruit_length_h) / 2) %>%
  filter(!is.na(Habitat))

interaction_count <- Fruit_length %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

interaction_count <- Fruit_length %>%
  distinct(mammal_species,Habitat) %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

# Apply bootstrapping function
Fruit_length_boot <- Fruit_length %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  summarise(
    boot_results = list(boot_function(fruit_length)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitlength = map_dbl(boot_results, 1),
    Mean_fruitlength = map_dbl(boot_results, 2),
    Hci_fruitlength = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)


interaction_count <- Fruit_length_boot %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)
# Final aggregation for each mammal type in each habitat
Fruit_length_final <- Fruit_length_boot %>%
  group_by(mammal_type, Habitat) %>%
  summarise(
    Mean_fruitlength_nf = mean(Mean_fruitlength, na.rm = TRUE),
    SD_fruitlength_nf = sd(Mean_fruitlength, na.rm = TRUE),
    SE_fruitlength_nf = SD_fruitlength_nf / sqrt(n()),
    lci_fruitlength_nf = Mean_fruitlength_nf - 1.96 * SE_fruitlength_nf,
    Hci_fruitlength_nf = Mean_fruitlength_nf + 1.96 * SE_fruitlength_nf,
    .groups = "drop"  # Ensures no lingering grouping issues
  )



#for ficus
# Ensure distinct plant traits by species, type, and habitat
plant_trait_zone <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE)

# Keep only non-Ficus species within specific zones
plant_trait_nonficus_habitat <- plant_trait_zone %>%
  filter(updated_genus %in% c("Ficus", "ficus")) 


# Calculate mean fruit length
Fruit_length <- plant_trait_nonficus_habitat %>%
  filter(!is.na(fruit_length_l) & !is.na(fruit_length_h)) %>%
  mutate(fruit_length = (fruit_length_l + fruit_length_h) / 2) %>%
  filter(!is.na(Habitat))

updated_species <- Fruit_length %>% filter(Habitat %in% c( "Tropical dry")) %>% filter(!is.na(fruit_length))

interaction_count <- Fruit_length %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# Apply bootstrapping function
Fruit_length_boot <- Fruit_length %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  summarise(
    boot_results = list(boot_function(fruit_length)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitlength = map_dbl(boot_results, 1),
    Mean_fruitlength = map_dbl(boot_results, 2),
    Hci_fruitlength = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)

interaction_count <- Fruit_length_boot %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

# Final aggregation for each mammal type in each zone
Fruit_length_final_ficus <- Fruit_length_boot %>%
  group_by(mammal_type, Habitat) %>%
  summarise(
    Mean_fruitlength_f = mean(Mean_fruitlength, na.rm = TRUE),
    SD_fruitlength_f = sd(Mean_fruitlength, na.rm = TRUE),
    SE_fruitlength_f = SD_fruitlength_f / sqrt(n()),
    lci_fruitlength_f = Mean_fruitlength_f - 1.96 * SE_fruitlength_f,
    Hci_fruitlength_f = Mean_fruitlength_f + 1.96 * SE_fruitlength_f,
    .groups = "drop"  # Ensures no lingering grouping issues
  )

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # For combining plots if needed

# Join datasets while keeping Asia_zone
Fruit_length_combined <- left_join(Fruit_length_final_ficus, Fruit_length_final, 
                                   by = c("mammal_type", "Habitat"))

# Convert to long format
Fruit_length_long <- Fruit_length_combined %>%
  pivot_longer(
    cols = c(Mean_fruitlength_f, Mean_fruitlength_nf, 
             lci_fruitlength_f, lci_fruitlength_nf, 
             Hci_fruitlength_f, Hci_fruitlength_nf), 
    names_to = c("Metric", "Dataset"), 
    names_pattern = "(.*)_(f|nf)"
  ) %>%
  mutate(Dataset = recode(Dataset, "f" = "Ficus", "nf" = "Non Ficus")) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  mutate(mammal_type = recode(mammal_type, 
                              "Carnivore" = "Carnivores", 
                              "Herbivore" = "Herbivores"))

library(ggplot2)

library(ggplot2)

plot_habitat <- ggplot(Fruit_length_long, aes(x = mammal_type, y = Mean_fruitlength, color = Dataset)) +
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lci_fruitlength, ymax = Hci_fruitlength), 
                width = 0.2, position = position_dodge(width = 0.6)) +
  
  # Custom color-blind friendly palette
  scale_color_manual(values = c("Ficus" = "#D55E00", "Non Ficus" = "#0072B2")) +  
  
  theme_classic() + 
  labs(x = "Mammal Type", y = "Mean Fruit Length (±95% CI)", color = NULL) +
  
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 30, color = "black" ),
    axis.text.y = element_text(size = 30, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, color = "black"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 30, color = "black"),
    
    # Add border to each facet panel
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),
    
    # Add border to facet title
    strip.background = element_rect(color = "black", fill = "grey90", size = 1.5),
    
    # Increase facet title font size
    strip.text = element_text(size = 30, face = "bold")
  ) +
  
  facet_wrap(~Habitat, ncol = 3)  # Creates separate plots for each zone


print(plot_habitat)

# Save the combined plot
ggsave("Fruitlength_Allhabitats.jpg", plot, width = 15, height = 10, dpi = 300, bg = "white")



#fruit width#
#ficus


fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )


fruit_diameter <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_species, mammal_type) %>% summarise(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)

Fruit_width_final_ficus <- Fruit_width_boot %>%
  group_by(mammal_type) %>%
  summarise(
    Mean_fruitwidth_f = mean(Mean_fruitwidth, na.rm = TRUE),
    SD_fruitwidth_f = sd(Fruit_width_boot$Mean_fruitwidth, na.rm = TRUE),
    SE_fruitwidth_f = SD_fruitwidth_f / sqrt(n()),
    lci_fruitwidth_f = Mean_fruitwidth_f - 1.96 * SE_fruitwidth_f,
    Hci_fruitwidth_f = Mean_fruitwidth_f + 1.96 * SE_fruitwidth_f
  )



plot<- ggplot(Fruit_width_final_ficus, aes(x = mammal_type, y = Mean_fruitwidth_f)) +
  geom_point(size = 4, color = "black") +  
  geom_errorbar(aes(ymin = lci_fruitwidth_f, ymax = Hci_fruitwidth_f), width = 0.2) +  
  theme_classic() +  # Removes background and grid lines
  labs(x = "Mammal Type", y = "Mean Fruit Diameter (±95% CI)") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 17)
  )



print(plot)



ggsave("asiawide_metanalysis_Fruitwidth_ficus.jpg", 
       plot, 
       width = 10, height = 8, dpi = 300, bg = "white")


#for non ficus


fruit_diameter <- plant_trait_nonficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )


fruit_diameter <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_species, mammal_type) %>% summarise(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)

Fruit_width_final <- Fruit_width_boot %>%
  group_by(mammal_type) %>%
  summarise(
    Mean_fruitwidth_nf = mean(Mean_fruitwidth, na.rm = TRUE),
    SD_fruitwidth_nf = sd(Fruit_width_boot$Mean_fruitwidth, na.rm = TRUE),
    SE_fruitwidth_nf = SD_fruitwidth_nf / sqrt(n()),
    lci_fruitwidth_nf = Mean_fruitwidth_nf - 1.96 * SE_fruitwidth_nf,
    Hci_fruitwidth_nf = Mean_fruitwidth_nf + 1.96 * SE_fruitwidth_nf
  )



plot<- ggplot(Fruit_width_final, aes(x = mammal_type, y = Mean_fruitwidth_nf)) +
  geom_point(size = 4, color = "black") +  
  geom_errorbar(aes(ymin = lci_fruitwidth_nf, ymax = Hci_fruitwidth_nf), width = 0.2) +  
  theme_classic() +  # Removes background and grid lines
  labs(x = "Mammal Type", y = "Mean Fruit Diameter (±95% CI)") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 17)
  )



print(plot)



ggsave("asiawide_metanalysis_Fruitwidth_nonficus.jpg", 
       plot, 
       width = 10, height = 8, dpi = 300, bg = "white")


library(dplyr)

Fruit_width_combined <- left_join(Fruit_width_final_ficus, Fruit_width_final, by = "mammal_type")
library(tidyr)
library(dplyr)

Fruit_width_long <- Fruit_width_combined %>%
  pivot_longer(
    cols = c(Mean_fruitwidth_f, Mean_fruitwidth_nf, 
             lci_fruitwidth_f, lci_fruitwidth_nf, 
             Hci_fruitwidth_f, Hci_fruitwidth_nf), 
    names_to = c("Metric", "Dataset"), 
    names_pattern = "(.*)_(f|nf)"
  ) %>%
  mutate(Dataset = recode(Dataset, "f" = "Ficus", "nf" = "Non Ficus")) %>%
  pivot_wider(names_from = Metric, values_from = value)

Fruit_width_long <- Fruit_width_long %>%
  mutate(mammal_type = recode(mammal_type, 
                              "Carnivore" = "Carnivores", 
                              "Herbivore" = "Herbivores"))


library(ggplot2)

plot_whole_diameter<- ggplot(Fruit_width_long, aes(x = mammal_type, y = Mean_fruitwidth, color = Dataset)) +
  # Points
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  # Error bars
  geom_errorbar(aes(ymin = lci_fruitwidth, ymax = Hci_fruitwidth), 
                width = 0.2, position = position_dodge(width = 0.6)) +
  
  # Customize colors for colorblind accessibility
  scale_color_manual(values = c("Ficus" = "#D55E00", "Non Ficus" = "#0072B2")) +  
  
  theme_classic() + 
  labs(x = "Mammal Type", y = "Mean Fruit diameter (±95% CI)", color = NULL) +  # Removed color label
  
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 30, color = "black"),  # Horizontal x-axis text
    axis.text.y = element_text(size = 30, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, color = "black"),
    panel.grid = element_blank(),
    
    # Increase font size of color labels in legend
    legend.text = element_text(size = 30, color = "black")  
  )


ggsave("asiawide_metanalysis_Fruitwidth_all.jpg", 
       plot, 
       width = 10, height = 8, dpi = 300, bg = "white")




#for difeerent habitat
#for nonficus
library(tidyverse)
# Ensure distinct plant traits by species, type, and zone
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")



plant_trait_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE)

# Keep only non-Ficus species within specific zones
plant_trait_nonficus_habitat <- plant_trait_habitat %>%
  filter(!updated_genus %in% c("Ficus", "ficus"))

#for non ficus
fruit_diameter <- plant_trait_nonficus_habitat %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  ) %>% filter(!is.na(Habitat))



interaction_count <- fruit_diameter %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

# Apply bootstrapping function
Fruit_diameter_boot <- fruit_diameter %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  summarise(
    boot_results = list(boot_function(fruit_diameter)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitdiameter = map_dbl(boot_results, 1),
    Mean_fruitdiameter = map_dbl(boot_results, 2),
    Hci_fruitdiameter = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)

interaction_count <- Fruit_diameter_boot %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

# Final aggregation for each mammal type in each zone
Fruit_diameter_final <- Fruit_diameter_boot %>%
  group_by(mammal_type, Habitat) %>%
  summarise(
    Mean_fruitdiameter_nf = mean(Mean_fruitdiameter, na.rm = TRUE),
    SD_fruitdiameter_nf = sd(Mean_fruitdiameter, na.rm = TRUE),
    SE_fruitdiameter_nf = SD_fruitdiameter_nf / sqrt(n()),
    lci_fruitdiameter_nf = Mean_fruitdiameter_nf - 1.96 * SE_fruitdiameter_nf,
    Hci_fruitdiameter_nf = Mean_fruitdiameter_nf + 1.96 * SE_fruitdiameter_nf,
    .groups = "drop"  # Ensures no lingering grouping issues
  )



#for ficus
# Ensure distinct plant traits by species, type, and zone
plant_trait_zone <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE)

# Keep only Ficus species within specific zones
plant_trait_ficus_habitat <- plant_trait_zone %>%
  filter(updated_genus %in% c("Ficus", "ficus"))

#for ficus
fruit_diameter <- plant_trait_ficus_habitat %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  ) %>% filter(!is.na(Habitat))

interaction_count <- fruit_diameter %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# Apply bootstrapping function
Fruit_diameter_boot <- fruit_diameter %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  summarise(
    boot_results = list(boot_function(fruit_diameter)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitdiameter = map_dbl(boot_results, 1),
    Mean_fruitdiameter = map_dbl(boot_results, 2),
    Hci_fruitdiameter = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)


interaction_count <- Fruit_diameter_boot %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# Final aggregation for each mammal type in each zone
Fruit_diameter_final_ficus <- Fruit_diameter_boot %>%
  group_by(mammal_type, Habitat) %>%
  summarise(
    Mean_fruitdiameter_f = mean(Mean_fruitdiameter, na.rm = TRUE),
    SD_fruitdiameter_f = sd(Mean_fruitdiameter, na.rm = TRUE),
    SE_fruitdiameter_f = SD_fruitdiameter_f / sqrt(n()),
    lci_fruitdiameter_f = Mean_fruitdiameter_f - 1.96 * SE_fruitdiameter_f,
    Hci_fruitdiameter_f = Mean_fruitdiameter_f + 1.96 * SE_fruitdiameter_f,
    .groups = "drop"  # Ensures no lingering grouping issues
  )



library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # For combining plots if needed

# Join datasets while keeping Asia_zone
Fruit_diameter_combined <- left_join(Fruit_diameter_final_ficus, Fruit_diameter_final, 
                                     by = c("mammal_type", "Habitat"))

# Convert to long format
Fruit_diameter_long <- Fruit_diameter_combined %>%
  pivot_longer(
    cols = c(Mean_fruitdiameter_f, Mean_fruitdiameter_nf, 
             lci_fruitdiameter_f, lci_fruitdiameter_nf, 
             Hci_fruitdiameter_f, Hci_fruitdiameter_nf), 
    names_to = c("Metric", "Dataset"), 
    names_pattern = "(.*)_(f|nf)"
  ) %>%
  mutate(Dataset = recode(Dataset, "f" = "Ficus", "nf" = "Non Ficus")) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  mutate(mammal_type = recode(mammal_type, 
                              "Carnivore" = "Carnivores", 
                              "Herbivore" = "Herbivores"))

library(ggplot2)

library(ggplot2)

plot_habitat_diameter <- ggplot(Fruit_diameter_long, aes(x = mammal_type, y = Mean_fruitdiameter, color = Dataset)) +
  geom_point(size = 4, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lci_fruitdiameter, ymax = Hci_fruitdiameter), 
                width = 0.2, position = position_dodge(width = 0.6)) +
  
  # Custom color-blind friendly palette
  scale_color_manual(values = c("Ficus" = "#D55E00", "Non Ficus" = "#0072B2")) +  
  
  theme_classic() + 
  labs(x = "Mammal Type", y = "Mean Fruit diameter (±95% CI)", color = NULL) +
  
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 30, color = "black"),
    plot.title = element_text(size = 30, color = "black"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 30, color = "black"),
    
    # Add border to each facet panel
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),
    
    # Add border to facet title
    strip.background = element_rect(color = "black", fill = "grey90", size = 1.5),
    
    # Increase facet title font size
    strip.text = element_text(size = 30, color = "black", face = "bold")
  ) +
  
  facet_wrap(~Habitat, ncol = 3)  # Creates separate plots for each zone


print(plot_habitat_diameter)

# Save the combined plot
ggsave("Fruitdiameter_Allhabitat.jpg", plot, width = 15, height = 10, dpi = 300, bg = "white")


#make all the fruit size graphs in one panel
library(ggplot2)
library(patchwork)

# Make sure ALL your plots have same legend & theme but suppress legend there
plot_whole <- plot_whole + theme(legend.position = "none")
plot_habitat <- plot_habitat + theme(legend.position = "none")
plot_whole_diameter <- plot_whole_diameter + theme(legend.position = "none")
plot_habitat_diameter <- plot_habitat_diameter + theme(legend.position = "none")

# Add labels
plot_whole <- plot_whole + ggtitle("A)")
plot_habitat <- plot_habitat + ggtitle("B)")
plot_whole_diameter <- plot_whole_diameter + ggtitle("C)")
plot_habitat_diameter <- plot_habitat_diameter + ggtitle("D)")

# Combine with patchwork (this pulls one legend automatically!)
combined_plot <- (plot_whole | plot_habitat) /
  (plot_whole_diameter | plot_habitat_diameter) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")




# Save to file with wider width
ggsave("combined_plot_fruit size_shared_legend.png", plot = combined_plot,
       width = 45, height = 15, dpi = 300)


########Number of seeds per fruit#####

# Ensure distinct plant traits by species, type, and zone
plant_trait_zone <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE)

# Keep only non-Ficus species
plant_trait_nonficus_zone <- plant_trait_zone %>%
  filter(!updated_genus %in% c("Ficus", "ficus")) %>%
  filter(!is.na(no_of_seed_per_fruit))  # Proper NA filtering

# Apply bootstrapping function
Fruit_number_boot <- plant_trait_nonficus_zone %>%
  group_by(mammal_species, mammal_type) %>%
  summarise(
    boot_results = list(boot_function(no_of_seed_per_fruit)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitno = map_dbl(boot_results, 1),
    Mean_fruitno = map_dbl(boot_results, 2),
    Hci_fruitno = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)


# Final aggregation for each mammal type
Fruit_number <- Fruit_number_boot %>%
  group_by(mammal_type) %>%
  summarise(
    mean_fruitnum = mean(Mean_fruitno, na.rm = TRUE),
    SD_fruitno = sd(Mean_fruitno, na.rm = TRUE),  # Use Mean_fruitno
    SE_fruitno = SD_fruitno / sqrt(n()),
    lci_fruitno = mean_fruitnum - 1.96 * SE_fruitno,
    Hci_fruitno = mean_fruitnum + 1.96 * SE_fruitno,
    .groups = "drop"  # Ensures no lingering grouping issues
  )



plot<- ggplot(Fruit_number, aes(x = mammal_type, y = mean_fruitnum)) +
  geom_point(size = 4, color = "black") +  
  geom_errorbar(aes(ymin = lci_fruitno, ymax = Hci_fruitno), width = 0.2) +  
  theme_classic() +  # Removes background and grid lines
  labs(x = "Mammal Type", y = "Mean seed numbers/fruit (±95% CI)") +
  theme(
    axis.text.x = element_text(hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 17)
  )


ggsave("number of seed per fruit.jpg", 
       plot, 
       width = 15, height = 8, dpi = 300, bg = "white")



#for habitat 
# Ensure distinct plant traits by species, type, and zone
plant_trait_habitat <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE)

# Keep only non-Ficus species and specific Asia zones
plant_trait_nonficus_habitat <- plant_trait_habitat %>%
  filter(
    !updated_genus %in% c("Ficus", "ficus"),
    !is.na(no_of_seed_per_fruit),
    !is.na(Habitat)# Proper NA filtering
  )


# Apply bootstrapping function
Fruit_number_boot <- plant_trait_nonficus_habitat %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  summarise(
    boot_results = list(boot_function(no_of_seed_per_fruit)), .groups = "drop"
  ) %>%
  mutate(
    lci_fruitno = map_dbl(boot_results, 1),
    Mean_fruitno = map_dbl(boot_results, 2),
    Hci_fruitno = map_dbl(boot_results, 3)
  ) %>%
  select(-boot_results)

# Final aggregation for each mammal type
Fruit_number <- Fruit_number_boot %>%
  group_by(mammal_type, Habitat) %>%
  summarise(
    mean_fruitnum = mean(Mean_fruitno, na.rm = TRUE),
    SD_fruitno = sd(Mean_fruitno, na.rm = TRUE),  # Use Mean_fruitno
    SE_fruitno = SD_fruitno / sqrt(n()),
    lci_fruitno = mean_fruitnum - 1.96 * SE_fruitno,
    Hci_fruitno = mean_fruitnum + 1.96 * SE_fruitno,
    .groups = "drop"  # Ensures no lingering grouping issues
  )


Fruit_number<- Fruit_number %>% mutate(mammal_type = recode(mammal_type, 
                                                            "Carnivore" = "Carnivores", 
                                                            "Herbivore" = "Herbivores"))


library(ggplot2)

library(ggplot2)

plot <- ggplot(Fruit_number, aes(x = mammal_type, y = mean_fruitnum)) +
  
  # Points and error bars
  geom_point(size = 4, color = "black", position = position_dodge(width = 0.6)) +  
  geom_errorbar(aes(ymin = lci_fruitno, ymax = Hci_fruitno), 
                width = 0.2, color = "black", position = position_dodge(width = 0.6)) +
  
  # Theme and labels
  theme_bw() +  # Use theme_bw() instead of theme_classic()
  labs(x = "Mammal Type", y = "Mean Number of Seeds per Fruit (±95% CI)") +
  
  # Custom styling
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold"),  
    panel.grid = element_blank(),
    
    # Remove legend (no color groups anymore)
    legend.position = "none",
    
    # Add border between facet panels
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    
    # Bigger facet titles
    strip.text = element_text(size = 18, face = "bold")  
  ) +
  
  # Facet by Habitat for side-by-side plots
  facet_wrap(~Habitat, ncol = 3)

# Display the plot
print(plot)

# Save the plot
ggsave("Fruit_number_Asia_habitat.jpg", 
       plot, 
       width = 12, height = 8, dpi = 300, bg = "white")




######## hill diversity calculation for plant species #######
library(hillR)
library(vegan)
library(iNEXT)
library(dplyr)
library(tidyr)
Plant_analysis <- plant_trait %>%
  distinct(updated_species, author_id, .keep_all = TRUE) %>%
  filter(!is.na(updated_species)) %>%
  group_by(mammal_type, updated_species) %>%
  summarise(species_analysis = n(), .groups = 'drop')

# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis %>%
  pivot_wider(names_from = mammal_type, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Ensure the dataset is numeric (excluding author_id if present)
plant_analysis_wide_numeric <- plant_analysis_wide %>%
  dplyr::select(-1) %>%  # Remove non-numeric column
  mutate(across(everything(), as.numeric))

# Extract each assemblage column as separate vectors
Herbivore <- plant_analysis_wide_numeric$Herbivore
Carnivore <- plant_analysis_wide_numeric$Carnivore
Primates <- plant_analysis_wide_numeric$Primates

# Create a list of assemblages
plant_analysis_list <- list(Herbivore = Herbivore, Carnivore = Carnivore, Primates = Primates)

# Check structure of the list
str(plant_analysis_list)

# Run the iNEXT analysis
df <- iNEXT(plant_analysis_list, q = 0, datatype = "abundance", se = TRUE)

# Create the plot
plot <- ggiNEXT(
  df,
  type = 1,
  se = TRUE,
  facet.var = "None",
  color.var = "Assemblage"
) + 
  xlab("Number of records") + 
  ylab("plant species diversity") 


ggsave("hill diversity_metananlysis_plant species.png",   # File name and format
       plot,                                  # The ggplot object to save
       width = 18,                            # Width of the plot in inches
       height = 8,                            # Height of the plot in inches
       units = "in",                          # Units for width and height
       dpi = 300,
       bg = "white")




####check sample coverage##
plot= ggiNEXT(df, type=3 , se=TRUE, facet.var="None", color.var="Assemblage") + xlab("Number of records") + 
  ylab("plant species richness")

ggsave("Sample coverage_plant species.png",   # File name and format
       plot,                                  # The ggplot object to save
       width = 18,                            # Width of the plot in inches
       height = 8,                            # Height of the plot in inches
       units = "in",                          # Units for width and height
       dpi = 300)







######## hill diversity calculation for plant genus #######

library(hillR)
library(vegan)


Plant_analysis <- plant_trait %>%
  distinct(updated_species, author_id, .keep_all = TRUE) %>%
  filter(!is.na(updated_genus)) %>%
  group_by(mammal_type, updated_genus) %>%
  summarise(species_analysis = n(), .groups = 'drop')

# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis %>%
  pivot_wider(names_from = mammal_type, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Ensure the dataset is numeric (excluding author_id if present)
plant_analysis_wide_numeric <- plant_analysis_wide %>%
  dplyr::select(-1) %>%  # Remove non-numeric column
  mutate(across(everything(), as.numeric))

# Extract each assemblage column as separate vectors
Herbivore <- plant_analysis_wide_numeric$Herbivore
Carnivore <- plant_analysis_wide_numeric$Carnivore
Primates <- plant_analysis_wide_numeric$Primates

# Create a list of assemblages
plant_analysis_list <- list(Herbivore = Herbivore, Carnivore = Carnivore, Primates = Primates)

# Check structure of the list
str(plant_analysis_list)

library(iNEXT)

# Run the iNEXT analysis
df <- iNEXT(plant_analysis_list, q = 0, datatype = "abundance", se = TRUE)



# Create the plot
plot <- ggiNEXT(
  df,
  type = 1,
  se = TRUE,
  facet.var = "None",
  color.var = "Assemblage"
) + 
  xlab("Number of records") + 
  ylab("plant genus diversity") 

ggsave("hill diversity_metananlysis_plant genus.png",   # File name and format
       plot,                                  # The ggplot object to save
       width = 18,                            # Width of the plot in inches
       height = 8,                            # Height of the plot in inches
       units = "in",                          # Units for width and height
       dpi = 300,
       bg = "white")





####check sample coverage##
plot= ggiNEXT(df, type=3 , se=TRUE, facet.var="None", color.var="Assemblage") + xlab("Number of records") + 
  ylab("plant genus richness")

ggsave("Sample coverage_plant genus.png",   # File name and format
       plot,                                  # The ggplot object to save
       width = 18,                            # Width of the plot in inches
       height = 8,                            # Height of the plot in inches
       units = "in",                          # Units for width and height
       dpi = 300)




######## hill diversity calculation for plant family #######

library(hillR)
library(vegan)
Plant_analysis <- plant_trait %>%
  distinct(updated_species, author_id, .keep_all = TRUE) %>%
  filter(!is.na(family)) %>%
  group_by(mammal_type, family) %>%
  summarise(species_analysis = n(), .groups = 'drop')


# Pivot the data and fill missing values with 0
plant_analysis_wide <- Plant_analysis %>%
  pivot_wider(names_from = mammal_type, values_from = species_analysis, values_fill = list(species_analysis = 0))


plant_analysis_wide <- plant_analysis_wide %>% dplyr::select(-1)

library(iNEXT)

plant_analysis_wide <- plant_analysis_wide %>% mutate_all(as.numeric)

Carnivore <- plant_analysis_wide$Carnivore

Herbivore <- plant_analysis_wide$Herbivore

Primates <- plant_analysis_wide$Primates


plant_analysis_wide2= list(Carnivore, `Herbivore`,Primates)

names(plant_analysis_wide2)= colnames(plant_analysis_wide)
str(plant_analysis_wide2)

plant_analysis_df <- bind_rows(plant_analysis_wide2, .id = "Assemblage")

df<- iNEXT(plant_analysis_wide2, q=0, datatype="abundance", se = TRUE)



plot <- ggiNEXT(
  df,
  type = 1,
  se = TRUE,
  facet.var = "None",
  color.var = "Assemblage"
) + 
  xlab("Number of records") + 
  ylab("plant family diversity") 

# Print the plot
print(plot)


ggsave("hill diversity_metaanalysis_Plant family.png",   # File name and format
       plot,                                  # The ggplot object to save
       width = 18,                            # Width of the plot in inches
       height = 8,                            # Height of the plot in inches
       units = "in",                          # Units for width and height
       dpi = 300)  

plot= ggiNEXT(df, type=3 , se=TRUE, facet.var="None", color.var="Assemblage") + xlab("Number of Studies") + 
  ylab("Plant family diversity")

ggsave("Sample coverage_plant family.png",   # File name and format
       plot,                                  # The ggplot object to save
       width = 18,                            # Width of the plot in inches
       height = 8,                            # Height of the plot in inches
       units = "in",                          # Units for width and height
       dpi = 300)  




########vein diagram for similarity among mammals#######
#install.packages("ggVennDiagram")
#how many species genera and family

unique_species= plant_trait  %>% distinct(updated_species)
unique_species_group= plant_trait  %>% distinct(mammal_type, updated_species)   %>% group_by(mammal_type) %>% reframe(freq= n())

unique_genera= plant_trait  %>% distinct(updated_genus)
unique_genus_group= plant_trait  %>% distinct(mammal_type, updated_genus)   %>% group_by(mammal_type) %>% reframe(freq= n())

unique_family= plant_trait  %>% distinct(family)
unique_family_group= plant_trait  %>% distinct(mammal_type, family)   %>% group_by(mammal_type) %>% reframe(freq= n())







#for ficus

plant_trait_ficus <- plant_trait_final %>%
  filter(updated_genus %in% c("Ficus", "ficus"))

library(ggVennDiagram)
library(dplyr)

#for permanova
# Data preparation
Plant_analysis_similarity <- plant_trait_ficus %>%
  filter(!is.na(updated_genus)) %>%
  distinct(mammal_scientific_name  , mammal_type, updated_species) %>%
  group_by(mammal_scientific_name  , mammal_type, updated_species) %>%
  summarise(species_analysis = n(), .groups = 'drop')

Plant_analysis_similarity$updated_species <- iconv(Plant_analysis_similarity$updated_species, from = "latin1", to = "UTF-8")


# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis_similarity %>%
  pivot_wider(names_from = updated_species, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Load libraries
library(vegan)
library(tidyverse)



# Ensure the dataframe is ungrouped
plant_analysis_wide <- plant_analysis_wide %>% ungroup()

# Extract numeric columns for NMDS
numeric_data <- plant_analysis_wide %>% dplyr::select(-c(1, 2),)

# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(as.matrix(numeric_data), method = "bray")

# Run PERMANOVA
permanova_result <- adonis2(bray_curtis ~ mammal_type, data = plant_analysis_wide, permutations = 999)

# Print results
print(permanova_result)


# Create a named list of plant species for each mammal_type
plant_species_list <- plant_trait_ficus %>%
  group_by(mammal_species, mammal_type) %>%
  summarize(plants = list(updated_species), .groups = "drop") %>%
  group_by(mammal_type) %>%
  summarize(plants = list(unlist(plants)), .groups = "drop") %>%
  deframe()

# Get unique plant species across all mammal types
unique_plants <- unique(unlist(plant_species_list))

# Print the unique species
print(unique_plants)


# Get unique plant species for each mammal type
unique_plants_per_mammal_type <- lapply(plant_species_list, unique)

# Print the unique species for each mammal type
print(unique_plants_per_mammal_type)


# Generate Venn diagram with ONLY percentages
venn_plot<- ggVennDiagram(plant_species_list, label = "percent") +  # Show only percentage
  scale_fill_gradient(low = "white", high = "#56B4E9") +  # Colorblind-friendly
  theme_void() + 
  labs(title = "Plant Species Overlap Across Mammal Types") +
  theme(plot.title = element_text(hjust = 0.5))




ggsave("venn_diagram_ficus species_Asia.png", venn_plot, width = 8, height = 6, dpi = 300, bg = "white")



# Data preparation
Plant_analysis_similarity <- plant_trait_ficus %>%
  filter(!is.na(updated_species)) %>%
  distinct(mammal_type, updated_species) %>%  # Fixing unique selection
  group_by(mammal_type, updated_species) %>%
  summarise(species_analysis = n(), .groups = 'drop')



# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis_similarity %>%
  pivot_wider(names_from = updated_species, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Load libraries
library(vegan)
library(tidyverse)



# Ensure the dataframe is ungrouped
plant_analysis_wide <- plant_analysis_wide %>% ungroup()

library(dplyr)
library(stringr)

library(dplyr)
library(stringr)

# Replace non-printable characters and trim spaces
colnames(plant_analysis_wide) <- str_squish(str_replace_all(colnames(plant_analysis_wide), "[^[:alnum:]_ ]", " "))

# Make names unique automatically
colnames(plant_analysis_wide) <- make.names(colnames(plant_analysis_wide), unique = TRUE)



# Extract numeric columns for NMDS
numeric_data <- plant_analysis_wide %>% dplyr::select(,-1)

##heatmap bray-curtis
# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(as.matrix(numeric_data), method = "bray")

grouping_info <- plant_analysis_wide[, "mammal_type"]


# Convert the dissimilarity object to a matrix
bray_curtis_matrix <- as.matrix(bray_curtis)

# Ensure row and column names are present for clarity
rownames(bray_curtis_matrix) <- grouping_info$mammal_type
colnames(bray_curtis_matrix) <- grouping_info$mammal_type

# Print the matrix
print(bray_curtis_matrix)


library(pheatmap)
library(RColorBrewer)

color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# Generate the heatmap
heatmap_plot <- pheatmap(bray_curtis_matrix, 
                         clustering_distance_rows = as.dist(bray_curtis_matrix),  # Precomputed distance for rows
                         clustering_distance_cols = as.dist(bray_curtis_matrix),  # Precomputed distance for columns
                         display_numbers = TRUE,                # Display numbers in each cell (optional)
                         fontsize_number = 14,                  # Adjust font size for numbers
                         fontface_number = "bold",              # Make the numbers bold
                         main = "Sorensen's Dissimilarity Heatmap of mammals_plant species", 
                         color = color_palette)                 # Title for the heatmap

# Save the plot as a PNG file
output_file <- "heatmap_bray_curtis_ficus species.png"
ggsave(output_file, plot = heatmap_plot$gtable, width = 10, height = 8)  # Save the heatmap as PNG





#for plant genus


# Create a named list of plant species for each mammal_type
plant_trait_genus <- plant_trait %>%
  distinct(mammal_species, updated_genus, .keep_all = TRUE)

plant_trait_nonficus_genus <- plant_trait_genus %>%
  filter(!updated_genus %in% c("Ficus", "ficus"))


#for permanova
# Data preparation
Plant_analysis_similarity <- plant_trait_nonficus_genus %>%
  filter(!is.na(updated_genus)) %>%
  distinct(mammal_scientific_name  , mammal_type, updated_genus) %>%
  group_by(mammal_scientific_name  , mammal_type, updated_genus) %>%
  summarise(species_analysis = n(), .groups = 'drop')

Plant_analysis_similarity$updated_genus <- iconv(Plant_analysis_similarity$updated_genus, from = "latin1", to = "UTF-8")


# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis_similarity %>%
  pivot_wider(names_from = updated_genus, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Load libraries
library(vegan)
library(tidyverse)



# Ensure the dataframe is ungrouped
plant_analysis_wide <- plant_analysis_wide %>% ungroup()

# Extract numeric columns for NMDS
numeric_data <- plant_analysis_wide %>% dplyr::select(-c(1, 2),)

# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(as.matrix(numeric_data), method = "bray")

# Run PERMANOVA
permanova_result <- adonis2(bray_curtis ~ mammal_type, data = plant_analysis_wide, permutations = 999)

# Print results
print(permanova_result)




plant_species_list <- plant_trait_nonficus_genus %>%
  group_by(mammal_species, mammal_type) %>%
  summarize(plants = list(updated_genus), .groups = "drop") %>%
  group_by(mammal_type) %>%
  summarize(plants = list(unlist(plants)), .groups = "drop") %>%
  deframe()

# Get unique plant species across all mammal types
unique_plants <- unique(unlist(plant_species_list))

# Print the unique species
print(unique_plants)


# Get unique plant species for each mammal type
unique_plants_per_mammal_type <- lapply(plant_species_list, unique)

# Print the unique species for each mammal type
print(unique_plants_per_mammal_type)



# Generate Venn diagram with ONLY percentages
venn_plot <- ggVennDiagram(plant_species_list, label = "percent") +  # Show only percentage
  scale_fill_gradient(low = "white", high = "#56B4E9") +  # Colorblind-friendly
  theme_void() + 
  labs(title = "Plant genus Overlap Across Mammal Types") +
  theme(plot.title = element_text(hjust = 0.5))


library(ggvenn)


ggsave("venn_diagram_nonficus genus_Asia.png", venn_plot, width = 8, height = 6, dpi = 300, bg = "white")


# Data preparation
Plant_analysis_similarity <- plant_trait_nonficus_genus %>%
  filter(!is.na(updated_genus)) %>%
  distinct(mammal_type, updated_genus) %>%  # Fixing unique selection
  group_by(mammal_type, updated_genus) %>%
  summarise(species_analysis = n(), .groups = 'drop')



# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis_similarity %>%
  pivot_wider(names_from = updated_genus, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Load libraries
library(vegan)
library(tidyverse)



# Ensure the dataframe is ungrouped
plant_analysis_wide <- plant_analysis_wide %>% ungroup()



# Extract numeric columns for NMDS
numeric_data <- plant_analysis_wide %>% dplyr::select(,-1)

##heatmap bray-curtis
# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(as.matrix(numeric_data), method = "bray")

grouping_info <- plant_analysis_wide[, "mammal_type"]


# Convert the dissimilarity object to a matrix
bray_curtis_matrix <- as.matrix(bray_curtis)

# Ensure row and column names are present for clarity
rownames(bray_curtis_matrix) <- grouping_info$mammal_type
colnames(bray_curtis_matrix) <- grouping_info$mammal_type

# Print the matrix
print(bray_curtis_matrix)


library(pheatmap)
library(RColorBrewer)

color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# Generate the heatmap
heatmap_plot <- pheatmap(bray_curtis_matrix, 
                         clustering_distance_rows = as.dist(bray_curtis_matrix),  # Precomputed distance for rows
                         clustering_distance_cols = as.dist(bray_curtis_matrix),  # Precomputed distance for columns
                         display_numbers = TRUE,                # Display numbers in each cell (optional)
                         fontsize_number = 14,                  # Adjust font size for numbers
                         fontface_number = "bold",              # Make the numbers bold
                         main = "Sorensen's Dissimilarity Heatmap of mammals_plant genus", 
                         color = color_palette)                 # Title for the heatmap

# Save the plot as a PNG file
output_file <- "heatmap_bray_curtis_nonficus genus.png"
ggsave(output_file, plot = heatmap_plot$gtable, width = 10, height = 8)  # Save the heatmap as PNG




#for plant family

# Create a named list of plant species for each mammal_type
plant_trait_family <- plant_trait %>%
  distinct(mammal_species, family, .keep_all = TRUE)

plant_trait_nonficus_family <- plant_trait_family %>%
  filter(!updated_genus %in% c("Ficus", "ficus"))


#for permanova
# Data preparation
Plant_analysis_similarity <- plant_trait_nonficus_family %>%
  filter(!is.na(family)) %>%
  distinct(mammal_scientific_name  , mammal_type, family) %>%
  group_by(mammal_scientific_name  , mammal_type, family) %>%
  summarise(species_analysis = n(), .groups = 'drop')

Plant_analysis_similarity$family <- iconv(Plant_analysis_similarity$family, from = "latin1", to = "UTF-8")


# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis_similarity %>%
  pivot_wider(names_from = family, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Load libraries
library(vegan)
library(tidyverse)



# Ensure the dataframe is ungrouped
plant_analysis_wide <- plant_analysis_wide %>% ungroup()

# Extract numeric columns for NMDS
numeric_data <- plant_analysis_wide %>% dplyr::select(-c(1, 2),)

# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(as.matrix(numeric_data), method = "bray")

# Run PERMANOVA
permanova_result <- adonis2(bray_curtis ~ mammal_type, data = plant_analysis_wide, permutations = 999)

# Print results
print(permanova_result)



# Create a named list of plant species for each mammal_type
plant_species_list <- plant_trait_nonficus_family %>%
  group_by(mammal_species, mammal_type) %>%
  summarize(plants = list(family), .groups = "drop") %>%
  group_by(mammal_type) %>%
  summarize(plants = list(unlist(plants)), .groups = "drop") %>%
  deframe()
# Get unique plant species across all mammal types
unique_plants <- unique(unlist(plant_species_list))

# Print the unique species
print(unique_plants)


# Get unique plant species for each mammal type
unique_plants_per_mammal_type <- lapply(plant_species_list, unique)

# Print the unique species for each mammal type
print(unique_plants_per_mammal_type)





# Generate Venn diagram with ONLY percentages
venn_plot<- ggVennDiagram(plant_species_list, label = "percent") +  # Show only percentage
  scale_fill_gradient(low = "white", high = "#56B4E9") +  # Colorblind-friendly
  theme_void() + 
  labs(title = "Plant family Overlap Across Mammal Types") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("venn_diagram_plant family_Asia.png", venn_plot, width = 8, height = 6, dpi = 300, bg = "white")


# Data preparation
Plant_analysis_similarity <- plant_trait_nonficus_family %>%
  filter(!is.na(family)) %>%
  distinct(mammal_type, family) %>%  # Fixing unique selection
  group_by(mammal_type, family) %>%
  summarise(species_analysis = n(), .groups = 'drop')



# Pivot the data to wide format and fill missing values with 0
plant_analysis_wide <- Plant_analysis_similarity %>%
  pivot_wider(names_from = family, values_from = species_analysis, values_fill = list(species_analysis = 0))

# Load libraries
library(vegan)
library(tidyverse)



# Ensure the dataframe is ungrouped
plant_analysis_wide <- plant_analysis_wide %>% ungroup()



# Extract numeric columns for NMDS
numeric_data <- plant_analysis_wide %>% dplyr::select(,-1)

##heatmap bray-curtis
# Calculate Bray-Curtis dissimilarity
bray_curtis <- vegdist(as.matrix(numeric_data), method = "bray")

grouping_info <- plant_analysis_wide[, "mammal_type"]


# Convert the dissimilarity object to a matrix
bray_curtis_matrix <- as.matrix(bray_curtis)

# Ensure row and column names are present for clarity
rownames(bray_curtis_matrix) <- grouping_info$mammal_type
colnames(bray_curtis_matrix) <- grouping_info$mammal_type

# Print the matrix
print(bray_curtis_matrix)


library(pheatmap)
library(RColorBrewer)

color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# Generate the heatmap
heatmap_plot <- pheatmap(bray_curtis_matrix, 
                         clustering_distance_rows = as.dist(bray_curtis_matrix),  # Precomputed distance for rows
                         clustering_distance_cols = as.dist(bray_curtis_matrix),  # Precomputed distance for columns
                         display_numbers = TRUE,                # Display numbers in each cell (optional)
                         fontsize_number = 14,                  # Adjust font size for numbers
                         fontface_number = "bold",              # Make the numbers bold
                         main = "Sorensen's Dissimilarity Heatmap of mammals_plant family", 
                         color = color_palette)                 # Title for the heatmap

# Save the plot as a PNG file
output_file <- "heatmap_bray_curtis_nonficus family.png"
ggsave(output_file, plot = heatmap_plot$gtable, width = 10, height = 8)  # Save the heatmap as PNG




#for different habitat and plant genus
plant_trait_genus_habitat <- plant_trait %>%
  distinct(mammal_scientific_name, updated_genus, Habitat, .keep_all = TRUE)

plant_trait_nonficus_genus_habitat <- plant_trait_genus_habitat %>%
  filter(!updated_genus %in% c("Ficus", "ficus"))

plant_trait_nonficus_genus_habitat <- plant_trait_nonficus_genus_habitat %>%
  filter(!Habitat %in% NA)

unique_genus_per_habitat <- plant_trait_nonficus_genus_habitat %>%
  group_by(Habitat, mammal_type) %>%
  summarize(unique_genus = list(unique(updated_genus)), .groups = "drop")

print(unique_genus_per_habitat)

unique_genus_per_zone <- plant_trait_nonficus_genus_habitat %>%
  group_by(Habitat) %>%
  summarize(unique_genus = list(unique(updated_genus)), .groups = "drop")

print(unique_genus_per_zone)

library(ggVennDiagram)
library(viridis)
library(patchwork)

create_venn_plot <- function(zone_data, zone_name) {
  # Create a nested list of plant species for each mammal_type in this zone
  plant_species_list <- zone_data %>%
    group_by(mammal_type, mammal_species) %>%
    summarize(plants = list(updated_genus), .groups = "drop") %>%
    group_by(mammal_type) %>%
    summarize(plants = list(unlist(plants)), .groups = "drop") %>%
    deframe()
  
  # Generate Venn diagram for this zone with percentage labels and standardized color scale
  ggVennDiagram(plant_species_list, label = "percent") +  
    scale_fill_gradient(low = "white", high = "#56B4E9", limits = c(0, 180)) +  # Standardized from 0% to 100%
    theme_void() + 
    labs(title = zone_name) +
    theme(plot.title = element_text(hjust = 0.5))
}


# Generate Venn diagrams for each Asia_zone
venn_plots <- plant_trait_nonficus_genus_habitat %>%
  group_split(Habitat) %>%
  map2(.x = ., .y = unique(plant_trait_nonficus_genus_habitat$Habitat), .f = create_venn_plot)

# Combine all plots into one layout using patchwork
final_plot <- wrap_plots(venn_plots) + plot_layout(ncol = 3)


# Save the faceted Venn diagram
ggsave("venn_diagram_habitat_nonficus genus.png", final_plot, width = 15, height = 8, dpi = 300, bg = "white")


library(vegan)
library(tidyr)
library(dplyr)
library(pheatmap)
library(RColorBrewer)

# Create a color palette
color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# List to store heatmaps
heatmap_list <- list()

# Get unique Habitat zones
habitat_zones <- unique(plant_trait_nonficus_genus_habitat$Habitat)

# Loop through each Habitat
for (zone in habitat_zones) {
  
  # Filter data for the specific zone
  zone_data <- plant_trait_nonficus_genus_habitat %>% filter(Habitat == zone)
  
  # Data preparation
  plant_analysis_similarity <- zone_data %>%
    filter(!is.na(updated_genus)) %>%
    distinct(mammal_type, updated_genus) %>%
    group_by(mammal_type, updated_genus) %>%
    summarise(species_analysis = n(), .groups = 'drop')
  
  # Pivot to wide format
  plant_analysis_wide <- plant_analysis_similarity %>%
    pivot_wider(names_from = updated_genus, values_from = species_analysis, values_fill = list(species_analysis = 0)) %>%
    ungroup()
  
  # Extract numeric data and ensure no NA values
  numeric_data <- plant_analysis_wide %>% select(-1) %>% drop_na()
  
  # Remove rows with all zeros (to avoid errors in vegdist)
  numeric_data <- numeric_data[rowSums(numeric_data) > 0, ]
  
  # Convert to matrix
  numeric_matrix <- as.matrix(numeric_data)
  
  # Compute Bray-Curtis dissimilarity
  if (nrow(numeric_matrix) > 1) {  # Only compute if there are multiple rows
    bray_curtis <- vegdist(numeric_matrix, method = "bray")
    bray_curtis_matrix <- as.matrix(bray_curtis)
    
    # Set row and column names
    rownames(bray_curtis_matrix) <- plant_analysis_wide$mammal_type
    colnames(bray_curtis_matrix) <- plant_analysis_wide$mammal_type
    
    # Generate the heatmap
    heatmap_plot <- pheatmap(bray_curtis_matrix, 
                             clustering_distance_rows = as.dist(bray_curtis_matrix),
                             clustering_distance_cols = as.dist(bray_curtis_matrix),
                             display_numbers = TRUE, 
                             fontsize_number = 14, 
                             fontface_number = "bold",
                             main = paste("Sorenson's Dissimilarity Heatmap -", zone),
                             color = color_palette)
    
    # Store heatmap in list
    heatmap_list[[zone]] <- heatmap_plot$gtable
  } else {
    heatmap_list[[zone]] <- paste("Not enough data for", zone)
  }
}

library(gridExtra)

# Arrange multiple heatmaps in a grid
grid.arrange(grobs = heatmap_list, ncol = 3)


png("heatmap_bray_curtis_Habitat_nonficus_genus.png", width = 15, height = 10, units = "in", res = 300)
grid.arrange(grobs = heatmap_list, ncol = 3)  # Arrange plots in a grid
dev.off()

library(vegan)
library(tidyr)
library(dplyr)

# List to store PERMANOVA results
permanova_results <- list()

# Get unique Habitat zones
habitat_zones <- unique(plant_trait_nonficus_genus_habitat$Habitat)

# Loop through each Habitat
for (zone in habitat_zones) {
  
  # Filter data for the specific zone
  zone_data <- plant_trait_nonficus_genus_habitat %>% filter(Habitat == zone)
  
  # Data preparation
  plant_analysis_similarity <- zone_data %>%
    filter(!is.na(updated_genus)) %>%
    distinct(mammal_species, mammal_type, updated_genus) %>%
    group_by(mammal_species, mammal_type, updated_genus) %>%
    summarise(species_analysis = n(), .groups = 'drop')
  
  # Pivot the data
  plant_analysis_wide <- plant_analysis_similarity %>%
    pivot_wider(names_from = updated_genus, values_from = species_analysis, values_fill = list(species_analysis = 0)) %>%
    ungroup()
  
  # Extract numeric data
  numeric_data <- plant_analysis_wide %>% dplyr::select(-mammal_species, -mammal_type) %>% drop_na()
  
  # Remove rows with all zeros (to avoid errors in vegdist)
  numeric_data <- numeric_data[rowSums(numeric_data) > 0, ]
  
  # Convert to matrix
  numeric_matrix <- as.matrix(numeric_data)
  
  # Compute Bray-Curtis dissimilarity and PERMANOVA only if there is enough data
  if (nrow(numeric_matrix) > 1) {
    bray_curtis <- vegdist(numeric_matrix, method = "bray")
    
    # Run PERMANOVA
    permanova_result <- adonis2(bray_curtis ~ plant_analysis_wide$mammal_type, permutations = 999)
    
    # Store results in list
    permanova_results[[zone]] <- permanova_result
  } else {
    permanova_results[[zone]] <- paste("Not enough data for PERMANOVA in", zone)
  }
}

# Print all results
permanova_results




#plant family and habitat across Asia
plant_trait_family_habitat <- plant_trait %>%
  distinct(mammal_scientific_name, family, Habitat, .keep_all = TRUE)

plant_trait_nonficus_family_habitat <- plant_trait_family_habitat %>%
  filter(!updated_genus %in% c("Ficus", "ficus"))

plant_trait_nonficus_family_habitat <- plant_trait_nonficus_family_habitat %>%
  filter(!Habitat %in% NA)

unique_genus_per_habitat <- plant_trait_nonficus_family_habitat %>%
  group_by(Habitat, mammal_type) %>%
  summarize(unique_genus = list(unique(family)), .groups = "drop")

print(unique_genus_per_habitat)

unique_genus_per_zone <- plant_trait_nonficus_family_habitat %>%
  group_by(Habitat) %>%
  summarize(unique_genus = list(unique(family)), .groups = "drop")

print(unique_genus_per_zone)

library(ggVennDiagram)
library(viridis)
library(patchwork)

create_venn_plot <- function(zone_data, zone_name) {
  # Create a nested list of plant species for each mammal_type in this zone
  plant_species_list <- zone_data %>%
    group_by(mammal_type, mammal_species) %>%
    summarize(plants = list(family), .groups = "drop") %>%
    group_by(mammal_type) %>%
    summarize(plants = list(unlist(plants)), .groups = "drop") %>%
    deframe()
  
  # Generate Venn diagram for this zone with percentage labels and standardized color scale
  ggVennDiagram(plant_species_list, label = "percent") +  
    scale_fill_gradient(low = "white", high = "#56B4E9", limits = c(0, 100)) +  # Standardized from 0% to 100%
    theme_void() + 
    labs(title = zone_name) +
    theme(plot.title = element_text(hjust = 0.5))
}


# Generate Venn diagrams for each Asia_zone
venn_plots <- plant_trait_nonficus_family_habitat %>%
  group_split(Habitat) %>%
  map2(.x = ., .y = unique(plant_trait_nonficus_family_habitat$Habitat), .f = create_venn_plot)

# Combine all plots into one layout using patchwork
final_plot <- wrap_plots(venn_plots) + plot_layout(ncol = 3)


# Save the faceted Venn diagram
ggsave("venn_diagram_habitat_nonficus family.png", final_plot, width = 15, height = 8, dpi = 300, bg = "white")


library(vegan)
library(tidyr)
library(dplyr)
library(pheatmap)
library(RColorBrewer)

# Create a color palette
color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# List to store heatmaps
heatmap_list <- list()

# Get unique Habitat zones
habitat_zones <- unique(plant_trait_nonficus_family_habitat$Habitat)

# Loop through each Habitat
for (zone in habitat_zones) {
  
  # Filter data for the specific zone
  zone_data <- plant_trait_nonficus_family_habitat %>% filter(Habitat == zone)
  
  # Data preparation
  plant_analysis_similarity <- zone_data %>%
    filter(!is.na(family)) %>%
    distinct(mammal_type, family) %>%
    group_by(mammal_type, family) %>%
    summarise(species_analysis = n(), .groups = 'drop')
  
  # Pivot to wide format
  plant_analysis_wide <- plant_analysis_similarity %>%
    pivot_wider(names_from = family, values_from = species_analysis, values_fill = list(species_analysis = 0)) %>%
    ungroup()
  
  # Extract numeric data and ensure no NA values
  numeric_data <- plant_analysis_wide %>% dplyr::select(-1) %>% drop_na()
  
  # Remove rows with all zeros (to avoid errors in vegdist)
  numeric_data <- numeric_data[rowSums(numeric_data) > 0, ]
  
  # Convert to matrix
  numeric_matrix <- as.matrix(numeric_data)
  
  # Compute Bray-Curtis dissimilarity
  if (nrow(numeric_matrix) > 1) {  # Only compute if there are multiple rows
    bray_curtis <- vegdist(numeric_matrix, method = "bray")
    bray_curtis_matrix <- as.matrix(bray_curtis)
    
    # Set row and column names
    rownames(bray_curtis_matrix) <- plant_analysis_wide$mammal_type
    colnames(bray_curtis_matrix) <- plant_analysis_wide$mammal_type
    
    # Generate the heatmap
    heatmap_plot <- pheatmap(bray_curtis_matrix, 
                             clustering_distance_rows = as.dist(bray_curtis_matrix),
                             clustering_distance_cols = as.dist(bray_curtis_matrix),
                             display_numbers = TRUE, 
                             fontsize_number = 14, 
                             fontface_number = "bold",
                             main = paste("Sorenson's Dissimilarity Heatmap -", zone),
                             color = color_palette)
    
    # Store heatmap in list
    heatmap_list[[zone]] <- heatmap_plot$gtable
  } else {
    heatmap_list[[zone]] <- paste("Not enough data for", zone)
  }
}


# Arrange multiple heatmaps in a grid
grid.arrange(grobs = heatmap_list, ncol = 3)


png("heatmap_bray_curtis_Habitat_nonficus_genus.png", width = 15, height = 10, units = "in", res = 300)
grid.arrange(grobs = heatmap_list, ncol = 3)  # Arrange plots in a grid
dev.off()

library(vegan)
library(tidyr)
library(dplyr)

# List to store PERMANOVA results
permanova_results <- list()

# Get unique Habitat zones
habitat_zones <- unique(plant_trait_nonficus_family_habitat$Habitat)

# Loop through each Habitat
for (zone in habitat_zones) {
  
  # Filter data for the specific zone
  zone_data <- plant_trait_nonficus_family_habitat %>% filter(Habitat == zone)
  
  # Data preparation
  plant_analysis_similarity <- zone_data %>%
    filter(!is.na(family)) %>%
    distinct(mammal_species, mammal_type, family) %>%
    group_by(mammal_species, mammal_type, family) %>%
    summarise(species_analysis = n(), .groups = 'drop')
  
  # Pivot the data
  plant_analysis_wide <- plant_analysis_similarity %>%
    pivot_wider(names_from = family, values_from = species_analysis, values_fill = list(species_analysis = 0)) %>%
    ungroup()
  
  # Extract numeric data
  numeric_data <- plant_analysis_wide %>% dplyr::select(-mammal_species, -mammal_type) %>% drop_na()
  
  # Remove rows with all zeros (to avoid errors in vegdist)
  numeric_data <- numeric_data[rowSums(numeric_data) > 0, ]
  
  # Convert to matrix
  numeric_matrix <- as.matrix(numeric_data)
  
  # Compute Bray-Curtis dissimilarity and PERMANOVA only if there is enough data
  if (nrow(numeric_matrix) > 1) {
    bray_curtis <- vegdist(numeric_matrix, method = "bray")
    
    # Run PERMANOVA
    permanova_result <- adonis2(bray_curtis ~ plant_analysis_wide$mammal_type, permutations = 999)
    
    # Store results in list
    permanova_results[[zone]] <- permanova_result
  } else {
    permanova_results[[zone]] <- paste("Not enough data for PERMANOVA in", zone)
  }
}

# Print all results
permanova_results


#ficus species & habitats
plant_trait_habitat <- plant_trait %>%
  distinct(mammal_scientific_name, updated_species, Habitat, .keep_all = TRUE)

plant_trait_ficus_habitat <- plant_trait_habitat %>%
  filter(updated_genus %in% c("Ficus", "ficus"))

plant_trait_ficus_habitat <- plant_trait_ficus_habitat %>%
  filter(!Habitat %in% NA)

unique_genus_per_habitat <- plant_trait_ficus_habitat %>%
  group_by(Habitat, mammal_type) %>%
  summarize(updated_genus = list(unique(updated_species)), .groups = "drop")

print(unique_genus_per_habitat)

unique_genus_per_zone <- plant_trait_ficus_habitat %>%
  group_by(Habitat) %>%
  summarize(updated_genus = list(unique(updated_species)), .groups = "drop")

print(unique_genus_per_zone)

library(ggVennDiagram)
library(viridis)
library(patchwork)

create_venn_plot <- function(zone_data, zone_name) {
  # Create a nested list of plant species for each mammal_type in this zone
  plant_species_list <- zone_data %>%
    group_by(mammal_type, mammal_species) %>%
    summarize(plants = list(updated_species), .groups = "drop") %>%
    group_by(mammal_type) %>%
    summarize(plants = list(unlist(plants)), .groups = "drop") %>%
    deframe()
  
  # Generate Venn diagram for this zone with percentage labels and standardized color scale
  ggVennDiagram(plant_species_list, label = "percent") +  
    scale_fill_gradient(low = "white", high = "#56B4E9", limits = c(0, 100)) +  # Standardized from 0% to 100%
    theme_void() + 
    labs(title = zone_name) +
    theme(plot.title = element_text(hjust = 0.5))
}


# Generate Venn diagrams for each Asia_zone
venn_plots <- plant_trait_ficus_habitat %>%
  group_split(Habitat) %>%
  map2(.x = ., .y = unique(plant_trait_ficus_habitat$Habitat), .f = create_venn_plot)

# Combine all plots into one layout using patchwork
final_plot <- wrap_plots(venn_plots) + plot_layout(ncol = 3)


# Save the faceted Venn diagram
ggsave("venn_diagram_habitat_ficus_species.png", final_plot, width = 15, height = 8, dpi = 300, bg = "white")


library(vegan)
library(tidyr)
library(dplyr)
library(pheatmap)
library(RColorBrewer)

# Create a color palette
color_palette <- colorRampPalette(brewer.pal(9, "Set1"))(100)

# List to store heatmaps
heatmap_list <- list()

# Get unique Habitat zones
habitat_zones <- unique(plant_trait_ficus_habitat$Habitat)

# Loop through each Habitat
for (zone in habitat_zones) {
  
  # Filter data for the specific zone
  zone_data <- plant_trait_ficus_habitat %>% filter(Habitat == zone)
  
  # Data preparation
  plant_analysis_similarity <- zone_data %>%
    filter(!is.na(updated_species)) %>%
    distinct(mammal_type, updated_species) %>%
    group_by(mammal_type, updated_species) %>%
    summarise(species_analysis = n(), .groups = 'drop')
  
  # Pivot to wide format
  plant_analysis_wide <- plant_analysis_similarity %>%
    pivot_wider(names_from = updated_species, values_from = species_analysis, values_fill = list(species_analysis = 0)) %>%
    ungroup()
  
  # Extract numeric data and ensure no NA values
  numeric_data <- plant_analysis_wide %>% dplyr::select(-1) %>% drop_na()
  
  # Remove rows with all zeros (to avoid errors in vegdist)
  numeric_data <- numeric_data[rowSums(numeric_data) > 0, ]
  
  # Convert to matrix
  numeric_matrix <- as.matrix(numeric_data)
  
  # Compute Bray-Curtis dissimilarity
  if (nrow(numeric_matrix) > 1) {  # Only compute if there are multiple rows
    bray_curtis <- vegdist(numeric_matrix, method = "bray")
    bray_curtis_matrix <- as.matrix(bray_curtis)
    
    # Set row and column names
    rownames(bray_curtis_matrix) <- plant_analysis_wide$mammal_type
    colnames(bray_curtis_matrix) <- plant_analysis_wide$mammal_type
    
    # Generate the heatmap
    heatmap_plot <- pheatmap(bray_curtis_matrix, 
                             clustering_distance_rows = as.dist(bray_curtis_matrix),
                             clustering_distance_cols = as.dist(bray_curtis_matrix),
                             display_numbers = TRUE, 
                             fontsize_number = 14, 
                             fontface_number = "bold",
                             main = paste("Sorenson's Dissimilarity Heatmap -", zone),
                             color = color_palette)
    
    # Store heatmap in list
    heatmap_list[[zone]] <- heatmap_plot$gtable
  } else {
    heatmap_list[[zone]] <- paste("Not enough data for", zone)
  }
}


# Arrange multiple heatmaps in a grid
grid.arrange(grobs = heatmap_list, ncol = 3)


png("heatmap_bray_curtis_Habitat_ficus_species.png", width = 15, height = 10, units = "in", res = 300)
grid.arrange(grobs = heatmap_list, ncol = 3)  # Arrange plots in a grid
dev.off()

library(vegan)
library(tidyr)
library(dplyr)

# List to store PERMANOVA results
permanova_results <- list()

# Get unique Habitat zones
habitat_zones <- unique(plant_trait_ficus_habitat$Habitat)

# Loop through each Habitat
for (zone in habitat_zones) {
  
  # Filter data for the specific zone
  zone_data <- plant_trait_ficus_habitat %>% filter(Habitat == zone)
  
  # Data preparation
  plant_analysis_similarity <- zone_data %>%
    filter(!is.na(updated_species)) %>%
    distinct(mammal_species, mammal_type, updated_species) %>%
    group_by(mammal_species, mammal_type, updated_species) %>%
    summarise(species_analysis = n(), .groups = 'drop')
  
  # Pivot the data
  plant_analysis_wide <- plant_analysis_similarity %>%
    pivot_wider(names_from = updated_species, values_from = species_analysis, values_fill = list(species_analysis = 0)) %>%
    ungroup()
  
  # Extract numeric data
  numeric_data <- plant_analysis_wide %>% dplyr::select(-mammal_species, -mammal_type) %>% drop_na()
  
  # Remove rows with all zeros (to avoid errors in vegdist)
  numeric_data <- numeric_data[rowSums(numeric_data) > 0, ]
  
  # Convert to matrix
  numeric_matrix <- as.matrix(numeric_data)
  
  # Compute Bray-Curtis dissimilarity and PERMANOVA only if there is enough data
  if (nrow(numeric_matrix) > 1) {
    bray_curtis <- vegdist(numeric_matrix, method = "bray")
    
    # Run PERMANOVA
    permanova_result <- adonis2(bray_curtis ~ plant_analysis_wide$mammal_type, permutations = 999)
    
    # Store results in list
    permanova_results[[zone]] <- permanova_result
  } else {
    permanova_results[[zone]] <- paste("Not enough data for PERMANOVA in", zone)
  }
}

# Print all results
permanova_results



########Mammal trait and fruit trait correlation######

boot_function <- function(c) {
  data = c
  mn = numeric(1000)
  for (i in 1:1000) {
    temp = sample(data, replace = TRUE)
    mn[i] = mean(temp, na.rm = TRUE)
  }
  lci = quantile(mn, 0.025, na.rm = TRUE)
  boot_mean = median(mn, na.rm = TRUE)  # Renamed from "mean" to avoid conflicts
  rci = quantile(mn, 0.975, na.rm = TRUE)
  results = c(lci, boot_mean, rci)
  return(results)
}




library(dplyr)
library(ggplot2)
library(vegan)

#for ficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))



fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )


fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_scientific_name, mammal_type) %>% reframe(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)


Mammal_trait = read.csv ("Mammal_traits.csv")
colnames(Mammal_trait)[colnames(Mammal_trait) == "scientific_name"] <- "mammal_scientific_name"
# Ensure correct encoding
Mammal_trait$mammal_scientific_name <- iconv(Mammal_trait$mammal_scientific_name, from = "latin1", to = "UTF-8")



# Ensure correct encoding
Fruit_width_boot$mammal_scientific_name <- iconv(Fruit_width_boot$mammal_scientific_name, from = "latin1", to = "UTF-8")



merged_data <- left_join(Fruit_width_boot, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))

#perform phylogenetic signaliing through phytools
# Install and load required packages
#install.packages("ape")
#install.packages("phytools")
library(ape)
library(phytools)

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[mammal_data$mammal_scientific_name != "Bear_sp", ]


# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)



# Load required packages
#install.packages ("caper")
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization

# 1️⃣ Function to prune a tree, keeping only species in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# 2️⃣ Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = mammal_data$mammal_scientific_name)

mammal_data_pruned <- data.frame(
  fruit_diameter = setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name),
  body_size = setNames(mammal_data$body_size, mammal_data$mammal_scientific_name),
  mammal_type = setNames(mammal_data$mammal_type, mammal_data$mammal_scientific_name)  # Add mammal type
)

# Add species names as a column in the pruned dataset
mammal_data_pruned$species <- rownames(mammal_data_pruned)





mammal_data_pruned$log_body_size <- log(mammal_data_pruned$body_size)


comp_data <- comparative.data(
  phy = pruned_trees[[1]],  
  data = mammal_data_pruned, 
  names.col = "species",  # Ensure this column exists in `mammal_data_pruned`
  vcv = TRUE, 
  na.omit = TRUE
)



# 5️⃣ Run the PGLS Regression
pgls_model <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")

# 6️⃣ Print summary results
summary(pgls_model)

# 7️⃣ Extract residuals for visualization
mammal_data_pruned$residuals <- residuals(pgls_model)

# Plot residuals instead of raw values
ggplot(mammal_data_pruned, aes(x = log_body_size, y = residuals, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.8) +  # Data points
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Zero baseline
  scale_color_viridis_d(option = "D") +  # Use colorblind-friendly palette
  labs(
    x = "Log Mammal Body Size", 
    y = "Residuals (PGLS)", 
    color = "Mammal Type"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.key.size = unit(1.5, "cm")
  )

ggsave("Ficus_residual_plot_phylo_Asia.png", width = 12, height = 8, dpi = 300)

library(ggplot2)

# Extract coefficients from PGLS model
pgls_intercept <- coef(pgls_model)[1]
pgls_slope <- coef(pgls_model)[2]

# Create a dataset for the regression line within the observed range
new_data <- data.frame(
  log_body_size = seq(min(mammal_data_pruned$log_body_size), max(mammal_data_pruned$log_body_size), length.out = 100)
)
new_data$fruit_diameter <- pgls_intercept + pgls_slope * new_data$log_body_size  # y = mx + b

# Plot data with regression line
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3) +
  geom_line(data = new_data, aes(x = log_body_size, y = fruit_diameter), color = "black", size = 1.2) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log Mammal Body Size", y = "Mean Fruit Diameter (cm)", color = "Mammal Type") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.key.size = unit(1.5, "cm")
  )


ggsave("Ficus_regression_plot_phylo_Asia.png", width = 12, height = 8, dpi = 300)







# for each of the mammal groups
library(ape)        # Phylogenetic trees
library(caper)      # PGLS regression
library(ggplot2)    # Visualization
library(dplyr)      # Data manipulation
library(viridis)
library(caper)


# 1️⃣ Run separate PGLS for each mammal type & store coefficients
pgls_models <- list()
model_coeffs <- data.frame(mammal_type = character(), intercept = numeric(), slope = numeric())

mammal_types <- unique(mammal_data_pruned$mammal_type)

for (m_type in mammal_types) {
  subset_data <- mammal_data_pruned %>% filter(mammal_type == m_type)
  
  comp_data <- comparative.data(
    phy = pruned_trees[[1]],  
    data = subset_data, 
    names.col = "species",  
    vcv = TRUE, 
    na.omit = TRUE
  )
  
  pgls_models[[m_type]] <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")
  
  # Store coefficients
  model_summary <- summary(pgls_models[[m_type]])
  model_coeffs <- rbind(model_coeffs, data.frame(
    mammal_type = m_type,
    intercept = model_summary$coefficients[1, 1],  # Intercept
    slope = model_summary$coefficients[2, 1]      # Slope
  ))
}

for (m_type in names(pgls_models)) {
  cat("\n*** PGLS Summary for:", m_type, "***\n")
  print(summary(pgls_models[[m_type]]))
}


library(dplyr)

interaction <- mammal_data_pruned %>%
  group_by(mammal_type) %>%
  summarise(interaction = n())


library(ggplot2)
library(dplyr)

# Create a new dataset for the regression lines
regression_lines <- data.frame()

for (m_type in unique(mammal_data_pruned$mammal_type)) {
  subset_data <- mammal_data_pruned %>% filter(mammal_type == m_type)
  
  # Get the min & max of observed log body size for this mammal type
  x_min <- min(subset_data$log_body_size, na.rm = TRUE)
  x_max <- max(subset_data$log_body_size, na.rm = TRUE)
  
  # Generate a sequence of x values within this range
  new_x <- seq(x_min, x_max, length.out = 100)
  
  # Get corresponding y values using the PGLS coefficients
  coeffs <- model_coeffs %>% filter(mammal_type == m_type)
  new_y <- coeffs$intercept + coeffs$slope * new_x
  
  # Store the regression line data
  regression_lines <- rbind(regression_lines, data.frame(
    mammal_type = m_type,
    log_body_size = new_x,
    fruit_diameter = new_y
  ))
}

# 🔥 Plot with regression lines within observed ranges
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(data = regression_lines, aes(x = log_body_size, y = fruit_diameter, color = mammal_type), size = 1.2) +  # PGLS regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Mean Fruit Diameter (cm)", 
       color = "Mammal Type") +
  theme_classic() +
  facet_wrap(~mammal_type, scales = "free_x") +  # Separate panels for each type
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "blank",
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),  # Add borders around facets
    strip.text = element_text(size = 18, face = "bold")  # Bigger & Bold Facet Captions
  )



ggsave("Ficus_mammal_group_phylo_Asia.png", width = 12, height = 8, dpi = 300)








###for nonficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))



fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )


fruit_diameter <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_scientific_name, mammal_type) %>% reframe(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)


Mammal_trait = read.csv ("Mammal_traits.csv")
colnames(Mammal_trait)[colnames(Mammal_trait) == "scientific_name"] <- "mammal_scientific_name"
# Ensure correct encoding
Mammal_trait$mammal_scientific_name <- iconv(Mammal_trait$mammal_scientific_name, from = "latin1", to = "UTF-8")



# Ensure correct encoding
Fruit_width_boot$mammal_scientific_name <- iconv(Fruit_width_boot$mammal_scientific_name, from = "latin1", to = "UTF-8")



merged_data <- left_join(Fruit_width_boot, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))

#perform phylogenetic signaliing through phytools
# Install and load required packages
#install.packages("ape")
#install.packages("phytools")
library(ape)
library(phytools)

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Gazella_arabica"), ]



# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)




# Load required packages
#install.packages ("caper")
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization

# 1️⃣ Function to prune a tree, keeping only species in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# 2️⃣ Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = mammal_data$mammal_scientific_name)

mammal_data_pruned <- data.frame(
  fruit_diameter = setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name),
  body_size = setNames(mammal_data$body_size, mammal_data$mammal_scientific_name),
  mammal_type = setNames(mammal_data$mammal_type, mammal_data$mammal_scientific_name)  # Add mammal type
)

rownames(mammal_data_pruned) <- mammal_data$mammal_scientific_name


# Add species names as a column in the pruned dataset
mammal_data_pruned$species <- rownames(mammal_data_pruned)





mammal_data_pruned$log_body_size <- log(mammal_data_pruned$body_size)


comp_data <- comparative.data(
  phy = pruned_trees[[1]],  
  data = mammal_data_pruned, 
  names.col = "species",  # Ensure this column exists in `mammal_data_pruned`
  vcv = TRUE, 
  na.omit = TRUE
)



# 5️⃣ Run the PGLS Regression
pgls_model <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")

# 6️⃣ Print summary results
summary(pgls_model)



library(ggplot2)

# Extract coefficients from PGLS model
pgls_intercept <- coef(pgls_model)[1]
pgls_slope <- coef(pgls_model)[2]

# Create a dataset for the regression line within the observed range
new_data <- data.frame(
  log_body_size = seq(min(mammal_data_pruned$log_body_size), max(mammal_data_pruned$log_body_size), length.out = 100)
)
new_data$fruit_diameter <- pgls_intercept + pgls_slope * new_data$log_body_size  # y = mx + b

# Plot data with regression line
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3) +
  geom_line(data = new_data, aes(x = log_body_size, y = fruit_diameter), color = "black", size = 1.2) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log Mammal Body Size", y = "Mean Fruit Diameter (cm)", color = "Mammal Type") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.key.size = unit(1.5, "cm")
  )




ggsave("NonFicus_regression_plot_phylo_Asia.png", width = 12, height = 8, dpi = 300)

# for each of the mammal groups
library(ape)        # Phylogenetic trees
library(caper)      # PGLS regression
library(ggplot2)    # Visualization
library(dplyr)      # Data manipulation
library(viridis)
library(caper)


# 1️⃣ Run separate PGLS for each mammal type & store coefficients
pgls_models <- list()
model_coeffs <- data.frame(mammal_type = character(), intercept = numeric(), slope = numeric())

mammal_types <- unique(mammal_data_pruned$mammal_type)

for (m_type in mammal_types) {
  subset_data <- mammal_data_pruned %>% filter(mammal_type == m_type)
  
  comp_data <- comparative.data(
    phy = pruned_trees[[1]],  
    data = subset_data, 
    names.col = "species",  
    vcv = TRUE, 
    na.omit = TRUE
  )
  
  pgls_models[[m_type]] <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")
  
  # Store coefficients
  model_summary <- summary(pgls_models[[m_type]])
  model_coeffs <- rbind(model_coeffs, data.frame(
    mammal_type = m_type,
    intercept = model_summary$coefficients[1, 1],  # Intercept
    slope = model_summary$coefficients[2, 1]      # Slope
  ))
}

for (m_type in names(pgls_models)) {
  cat("\n*** PGLS Summary for:", m_type, "***\n")
  print(summary(pgls_models[[m_type]]))
}

interaction <- mammal_data_pruned %>%
  group_by(mammal_type) %>%
  summarise(interaction = n())


library(ggplot2)
library(dplyr)

# Create a new dataset for the regression lines
regression_lines <- data.frame()

for (m_type in unique(mammal_data_pruned$mammal_type)) {
  subset_data <- mammal_data_pruned %>% filter(mammal_type == m_type)
  
  # Get the min & max of observed log body size for this mammal type
  x_min <- min(subset_data$log_body_size, na.rm = TRUE)
  x_max <- max(subset_data$log_body_size, na.rm = TRUE)
  
  # Generate a sequence of x values within this range
  new_x <- seq(x_min, x_max, length.out = 100)
  
  # Get corresponding y values using the PGLS coefficients
  coeffs <- model_coeffs %>% filter(mammal_type == m_type)
  new_y <- coeffs$intercept + coeffs$slope * new_x
  
  # Store the regression line data
  regression_lines <- rbind(regression_lines, data.frame(
    mammal_type = m_type,
    log_body_size = new_x,
    fruit_diameter = new_y
  ))
}

# 🔥 Plot with regression lines within observed ranges
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(data = regression_lines, aes(x = log_body_size, y = fruit_diameter, color = mammal_type), size = 1.2) +  # PGLS regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Mean Fruit Diameter (cm)", 
       color = "Mammal Type") +
  theme_classic() +
  facet_wrap(~mammal_type, scales = "free_x") +  # Separate panels for each type
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "blank",
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),  # Add borders around facets
    strip.text = element_text(size = 18, face = "bold")  # Bigger & Bold Facet Captions
  )



ggsave("NonFicus_mammal_group_phylo_Asia.png", width = 12, height = 8, dpi = 300)





#for max fruit size and non ficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))



fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter_h), fruit_breadth_h, fruit_diameter_h)
  )


fruit_diameter <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


fruit_diameter_max <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter)) %>%
  group_by(mammal_species, mammal_type) %>%
  slice_max(fruit_diameter, with_ties = FALSE)  # Selects the row with the max fruit diameter



Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(fruit_diameter_max, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))

# Load required packages
#install.packages ("caper")
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization

#perform phylogenetic signaliing through phytools
# Install and load required packages
#install.packages("ape")
#install.packages("phytools")
library(ape)
library(phytools)

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Gazella_arabica"), ]

mammal_data <- mammal_data %>%  distinct(mammal_scientific_name, .keep_all = TRUE)

# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)







# 1️⃣ Function to prune a tree, keeping only species in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# 2️⃣ Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = mammal_data$mammal_scientific_name)

mammal_data_pruned <- data.frame(
  fruit_diameter = setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name),
  body_size = setNames(mammal_data$body_size, mammal_data$mammal_scientific_name),
  mammal_type = setNames(mammal_data$mammal_type, mammal_data$mammal_scientific_name)  # Add mammal type
)

rownames(mammal_data_pruned) <- mammal_data$mammal_scientific_name


# Add species names as a column in the pruned dataset
mammal_data_pruned$species <- rownames(mammal_data_pruned)





mammal_data_pruned$log_body_size <- log(mammal_data_pruned$body_size)


comp_data <- comparative.data(
  phy = pruned_trees[[1]],  
  data = mammal_data_pruned, 
  names.col = "species",  # Ensure this column exists in `mammal_data_pruned`
  vcv = TRUE, 
  na.omit = TRUE
)



# 5️⃣ Run the PGLS Regression
pgls_model <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")

# 6️⃣ Print summary results
summary(pgls_model)



# Extract coefficients from PGLS model
pgls_intercept <- coef(pgls_model)[1]
pgls_slope <- coef(pgls_model)[2]

# Create a dataset for the regression line within the observed range
new_data <- data.frame(
  log_body_size = seq(min(mammal_data_pruned$log_body_size), max(mammal_data_pruned$log_body_size), length.out = 100)
)
new_data$fruit_diameter <- pgls_intercept + pgls_slope * new_data$log_body_size  # y = mx + b

# Plot data with regression line
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3) +
  geom_line(data = new_data, aes(x = log_body_size, y = fruit_diameter), color = "black", size = 1.2) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Log Mammal Body Size", y = "Maximum Fruit Diameter (cm)", color = "Mammal Type") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.key.size = unit(1.5, "cm")
  )



ggsave("Maximum fruit diameter_NonFicus_regression_plot_phylo.png", width = 12, height = 8, dpi = 300)

# for each of the mammal groups
library(ape)        # Phylogenetic trees
library(caper)      # PGLS regression
library(ggplot2)    # Visualization
library(dplyr)      # Data manipulation
library(viridis)
library(caper)


# 1️⃣ Run separate PGLS for each mammal type & store coefficients
pgls_models <- list()
model_coeffs <- data.frame(mammal_type = character(), intercept = numeric(), slope = numeric())

mammal_types <- unique(mammal_data_pruned$mammal_type)

for (m_type in mammal_types) {
  subset_data <- mammal_data_pruned %>% filter(mammal_type == m_type)
  
  comp_data <- comparative.data(
    phy = pruned_trees[[1]],  
    data = subset_data, 
    names.col = "species",  
    vcv = TRUE, 
    na.omit = TRUE
  )
  
  pgls_models[[m_type]] <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")
  
  # Store coefficients
  model_summary <- summary(pgls_models[[m_type]])
  model_coeffs <- rbind(model_coeffs, data.frame(
    mammal_type = m_type,
    intercept = model_summary$coefficients[1, 1],  # Intercept
    slope = model_summary$coefficients[2, 1]      # Slope
  ))
}

for (m_type in names(pgls_models)) {
  cat("\n*** PGLS Summary for:", m_type, "***\n")
  print(summary(pgls_models[[m_type]]))
}


regression_lines <- mammal_data_pruned %>%
  group_by(mammal_type) %>%
  summarize(
    log_body_size = seq(min(log_body_size), max(log_body_size), length.out = 100),
    .groups = "drop"
  ) %>%
  left_join(model_coeffs, by = "mammal_type") %>%
  mutate(fruit_diameter = intercept + slope * log_body_size)





# 🔥 Plot with regression lines within observed ranges
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines, 
    aes(x = log_body_size, y = fruit_diameter, color = mammal_type), 
    size = 1.2
  ) +  # PGLS regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(
    x = "Log Body Size", 
    y = "Maximum Fruit Diameter (cm)", 
    color = "Mammal Type"
  ) +
  theme_classic() +
  facet_wrap(~mammal_type, scales = "free_x") +  # Separate panels for each type
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "none",  # ✅ use "none", not "blank"
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),  # Facet borders
    strip.text = element_text(size = 18, face = "bold")  # Bigger & Bold Facet Captions
  )



ggsave("Maximum fruit diameter_NonFicus_mammal_group_phylo.png", width = 12, height = 8, dpi = 300)







#fruit size and mammal size simple regression
library(mgcv)

merged_data$log_body_size <- log(merged_data$body_size)

# Perform linear regression
model_size <- lm(fruit_diameter ~ log_body_size, data = merged_data)

summary(model_size)


library(ggplot2)
library(viridis)  # For colorblind-friendly palettes

# Create the plot
p <- ggplot(merged_data, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3) +  # Increase point size for visibility
  geom_smooth(method = "lm", se = FALSE, color = "#0072B2") +  # Accessible blue regression line
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Mammal Size", 
       y = "Maximum Fruit Diameter (cm)", 
       color = "Mammal Type") +  # Legend title
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 20),  # Bigger x-axis label
    axis.title.y = element_text(size = 20),  # Bigger y-axis label
    axis.text.x = element_text(size = 16),  # Bigger x-axis tick labels
    axis.text.y = element_text(size = 16),  # Bigger y-axis tick labels
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16),  # Bigger legend title
    legend.key.size = unit(1.5, "cm")  # Increase legend key size
  )

# Display the plot
print(p)

# Save the plot with high resolution
ggsave("Maximum fruit_size_vs_mammal_size_nonficus.png", plot = p, width = 12, height = 5, dpi = 300)





#for Maximum fruit size and ficus
#for max fruit size
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))



fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter_h), fruit_breadth_h, fruit_diameter_h)
  )


fruit_diameter <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


fruit_diameter_max <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter)) %>%
  group_by(mammal_species, mammal_type) %>%
  slice_max(fruit_diameter, with_ties = FALSE)  # Selects the row with the max fruit diameter



Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(fruit_diameter_max, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))

# Load required packages
#install.packages ("caper")
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization

#perform phylogenetic signaliing through phytools
# Install and load required packages
#install.packages("ape")
#install.packages("phytools")
library(ape)
library(phytools)

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp"), ]

mammal_data <- mammal_data %>%  distinct(mammal_scientific_name, .keep_all = TRUE)

# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)







# 1️⃣ Function to prune a tree, keeping only species in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# 2️⃣ Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = mammal_data$mammal_scientific_name)

mammal_data_pruned <- data.frame(
  fruit_diameter = setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name),
  body_size = setNames(mammal_data$body_size, mammal_data$mammal_scientific_name),
  mammal_type = setNames(mammal_data$mammal_type, mammal_data$mammal_scientific_name)  # Add mammal type
)

rownames(mammal_data_pruned) <- mammal_data$mammal_scientific_name


# Add species names as a column in the pruned dataset
mammal_data_pruned$species <- rownames(mammal_data_pruned)





mammal_data_pruned$log_body_size <- log(mammal_data_pruned$body_size)


comp_data <- comparative.data(
  phy = pruned_trees[[1]],  
  data = mammal_data_pruned, 
  names.col = "species",  # Ensure this column exists in `mammal_data_pruned`
  vcv = TRUE, 
  na.omit = TRUE
)



# 5️⃣ Run the PGLS Regression
pgls_model <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")

# 6️⃣ Print summary results
summary(pgls_model)




# Extract PGLS coefficients
pgls_intercept <- coef(pgls_model)[1]
pgls_slope <- coef(pgls_model)[2]


# Create a dataset for the regression line within the observed range
new_data <- data.frame(
  log_body_size = seq(min(mammal_data_pruned$log_body_size), max(mammal_data_pruned$log_body_size), length.out = 100)
)
new_data$fruit_diameter <- pgls_intercept + pgls_slope * new_data$log_body_size  # y = mx + b



ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(
    data = new_data,
    aes(x = log_body_size, y = fruit_diameter),
    color = "black",
    size = 1.2
  ) +
  scale_color_viridis_d(option = "D") +
  labs(
    x = "Log Mammal Body Size",
    y = "Maximum Fruit Diameter (cm)",
    color = "Mammal Type"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.key.size = unit(1.2, "cm")
  )

ggsave("Maximum fruit diameter_Ficus_regression_plot_phylo.png", width = 12, height = 8, dpi = 300)

# for each of the mammal groups
library(ape)        # Phylogenetic trees
library(caper)      # PGLS regression
library(ggplot2)    # Visualization
library(dplyr)      # Data manipulation
library(viridis)
library(caper)


# 1️⃣ Run separate PGLS for each mammal type & store coefficients
pgls_models <- list()
model_coeffs <- data.frame(mammal_type = character(), intercept = numeric(), slope = numeric())

mammal_types <- unique(mammal_data_pruned$mammal_type)

for (m_type in mammal_types) {
  subset_data <- mammal_data_pruned %>% filter(mammal_type == m_type)
  
  comp_data <- comparative.data(
    phy = pruned_trees[[1]],  
    data = subset_data, 
    names.col = "species",  
    vcv = TRUE, 
    na.omit = TRUE
  )
  
  pgls_models[[m_type]] <- pgls(fruit_diameter ~ log_body_size, data = comp_data, lambda = "ML")
  
  # Store coefficients
  model_summary <- summary(pgls_models[[m_type]])
  model_coeffs <- rbind(model_coeffs, data.frame(
    mammal_type = m_type,
    intercept = model_summary$coefficients[1, 1],  # Intercept
    slope = model_summary$coefficients[2, 1]      # Slope
  ))
}

for (m_type in names(pgls_models)) {
  cat("\n*** PGLS Summary for:", m_type, "***\n")
  print(summary(pgls_models[[m_type]]))
}

regression_lines <- mammal_data_pruned %>%
  group_by(mammal_type) %>%
  summarize(
    log_body_size = seq(min(log_body_size), max(log_body_size), length.out = 100),
    .groups = "drop"
  ) %>%
  left_join(model_coeffs, by = "mammal_type") %>%
  mutate(fruit_diameter = intercept + slope * log_body_size)





# 2️⃣ Plot with regression lines using abline()
ggplot(mammal_data_pruned, aes(x = log_body_size, y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines, 
    aes(x = log_body_size, y = fruit_diameter, color = mammal_type),
    size = 1.2
  )  +  # PGLS regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Maximum Fruit Diameter (cm)", 
       color = "Mammal Type") +
  theme_classic() +
  facet_wrap(~mammal_type, scales = "free_x") +  # Separate panels for each type
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "blank",
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),  # Add borders around facets
    strip.text = element_text(size = 18, face = "bold")  # 🔥 Bigger & Bold Facet Captions
  )


ggsave("Maximum fruit diameter_Ficus_mammal_group_phylo.png", width = 12, height = 8, dpi = 300)



#for habitat nonficus & mean fruit size

plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_scientific_name, mammal_type, Habitat) %>% reframe(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)


Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(Fruit_width_boot, Mammal_trait, by = c("mammal_scientific_name"))


merged_data <- merged_data %>% filter(!is.na(body_size))

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Gazella_arabica "), ]

mammal_data <- mammal_data[!is.na(mammal_data$Habitat), ]


# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Habitat == "Temperate & subtropical")
asia_zone_2 <- merged_data %>% filter(Habitat == "Tropical dry")
asia_zone_3 <- merged_data %>% filter(Habitat == "Tropical wet")

interaction <- merged_data %>%
  group_by(Habitat) %>%
  summarise(interaction = n())

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)

# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

# Ensure the species names match the tree tip labels
asia_zone_1 <- asia_zone_1[asia_zone_1$mammal_scientific_name %in% pruned_tree_zone_1$tip.label, ]
asia_zone_2 <- asia_zone_2[asia_zone_2$mammal_scientific_name %in% pruned_tree_zone_2$tip.label, ]
asia_zone_3 <- asia_zone_3[asia_zone_3$mammal_scientific_name %in% pruned_tree_zone_3$tip.label, ]


# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Temperate & subtropical", "Tropical dry", "Tropical wet"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Habitat = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)

merged_data <- merged_data[!is.na(merged_data$Habitat), ]

regression_lines <- merged_data %>%
  filter(!is.na(Habitat)) %>%
  group_by(Habitat) %>%
  summarise(
    x_range = list(seq(min(log(body_size)), max(log(body_size)), length.out = 100)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Habitat") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)





# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = Mean_fruitwidth, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  ) +  # Regression lines (correctly mapped)
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Mean Fruit diameter (cm)", 
       color = "Mammal Type"  # Updated legend title
  ) +
  theme_classic() +
  facet_wrap(~Habitat, scales = "free_x") +  # Separate panels for each Habitat
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_meanfruitwidth__nonficus_habitat_phylo__plot.png", width = 15, height = 8, dpi = 300)




#for ficus & habitat mean fruit diameter
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_scientific_name, mammal_type, Habitat) %>% reframe(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)


Mammal_trait = read.csv ("Mammal_traits.csv")



merged_data <- left_join(Fruit_width_boot, Mammal_trait, by = c("mammal_scientific_name"))


merged_data <- merged_data %>% filter(!is.na(body_size))

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp"), ]

mammal_data <- mammal_data[!is.na(mammal_data$Habitat), ]


# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)

interaction <- merged_data %>%
  group_by(Habitat) %>%
  summarise(interaction = n())


# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Habitat == "Temperate & subtropical")
asia_zone_2 <- merged_data %>% filter(Habitat == "Tropical dry")
asia_zone_3 <- merged_data %>% filter(Habitat == "Tropical wet")

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)

# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

# Ensure the species names match the tree tip labels
asia_zone_1 <- asia_zone_1[asia_zone_1$mammal_scientific_name %in% pruned_tree_zone_1$tip.label, ]
asia_zone_2 <- asia_zone_2[asia_zone_2$mammal_scientific_name %in% pruned_tree_zone_2$tip.label, ]
asia_zone_3 <- asia_zone_3[asia_zone_3$mammal_scientific_name %in% pruned_tree_zone_3$tip.label, ]


# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Temperate & subtropical", "Tropical dry", "Tropical wet"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Habitat = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)

merged_data <- merged_data[!is.na(merged_data$Habitat), ]

regression_lines <- merged_data %>%
  filter(!is.na(Habitat)) %>%
  group_by(Habitat) %>%
  summarise(
    x_range = list(seq(min(log(body_size)), max(log(body_size)), length.out = 100)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Habitat") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)




# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = Mean_fruitwidth, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  )  +  # Regression lines (correctly mapped)
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Mean Fruit Diameter (cm)", 
       color = "Mammal Type"  # Updated legend title
  ) +
  theme_classic() +
  facet_wrap(~Habitat, scales = "free_x") +  # Free x-axis for each Habitat facet
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_meanfruitwidth__ficus_habitat_phylo__plot.png", width = 15, height = 8, dpi = 300)



#for habitat & ficus maximum fruit diameter
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


fruit_diameter_max <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter)) %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  slice_max(fruit_diameter, with_ties = FALSE)  # Selects the row with the max fruit diameter



interaction_count <- fruit_diameter %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

mammal_species_count <- fruit_diameter %>%
  group_by(Habitat) %>%
  summarize(unique_mammal_species = n_distinct(mammal_species), .groups = "drop")

print(mammal_species_count)


Mammal_trait = read.csv ("Mammal_traits.csv")


merged_data <- left_join(fruit_diameter_max, Mammal_trait, by = c("mammal_scientific_name"))


merged_data <- merged_data %>% filter(!is.na(body_size))

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp"), ]

mammal_data <- mammal_data[!is.na(mammal_data$Habitat), ]


# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Habitat == "Temperate & subtropical")
asia_zone_2 <- merged_data %>% filter(Habitat == "Tropical dry")
asia_zone_3 <- merged_data %>% filter(Habitat == "Tropical wet")
# Remove duplicates, keeping only the first occurrence
asia_zone_3 <- asia_zone_3 %>% distinct(mammal_scientific_name, .keep_all = TRUE) # if there is a duplicate 

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)

# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

# Ensure the species names match the tree tip labels
asia_zone_1 <- asia_zone_1[asia_zone_1$mammal_scientific_name %in% pruned_tree_zone_1$tip.label, ]
asia_zone_2 <- asia_zone_2[asia_zone_2$mammal_scientific_name %in% pruned_tree_zone_2$tip.label, ]
asia_zone_3 <- asia_zone_3[asia_zone_3$mammal_scientific_name %in% pruned_tree_zone_3$tip.label, ]


# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Temperate & subtropical", "Tropical dry", "Tropical wet"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Habitat = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)

merged_data <- merged_data[!is.na(merged_data$Habitat), ]


regression_lines <- merged_data %>%
  filter(!is.na(Habitat)) %>%
  group_by(Habitat) %>%
  summarise(
    x_range = list(seq(min(log(body_size)), max(log(body_size)), length.out = 100)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Habitat") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)


# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  ) +  # Regression lines (correctly mapped)
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Maximum Fruit diameter (cm)", 
       color = "Mammal Type"  # Updated legend title
  ) +
  theme_classic() +
  facet_wrap(~Habitat, scales = "free_x") +  # Separate panels for each Habitat
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_maximumfruitwidth__ficus_habitat_phylo__plot.png", width = 15, height = 8, dpi = 300)






##for maximum fruit width and nonficus fruit for habitat
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


fruit_diameter_max <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter)) %>%
  group_by(mammal_species, mammal_type, Habitat) %>%
  slice_max(fruit_diameter, with_ties = FALSE)  # Selects the row with the max fruit diameter



interaction_count <- fruit_diameter %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

mammal_species_count <- fruit_diameter %>%
  group_by(Habitat) %>%
  summarize(unique_mammal_species = n_distinct(mammal_species), .groups = "drop")

print(mammal_species_count)


Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(fruit_diameter_max, Mammal_trait, by = c("mammal_scientific_name"))


merged_data <- merged_data %>% filter(!is.na(body_size))

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Gazella_arabica"), ]

mammal_data <- mammal_data[!is.na(mammal_data$Habitat), ]


# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Habitat == "Temperate & subtropical")
asia_zone_2 <- merged_data %>% filter(Habitat == "Tropical dry")
asia_zone_3 <- merged_data %>% filter(Habitat == "Tropical wet")
# Remove duplicates, keeping only the first occurrence
asia_zone_3 <- asia_zone_3 %>% distinct(mammal_scientific_name, .keep_all = TRUE) # if there is a duplicate 

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)

# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

# Ensure the species names match the tree tip labels
asia_zone_1 <- asia_zone_1[asia_zone_1$mammal_scientific_name %in% pruned_tree_zone_1$tip.label, ]
asia_zone_2 <- asia_zone_2[asia_zone_2$mammal_scientific_name %in% pruned_tree_zone_2$tip.label, ]
asia_zone_3 <- asia_zone_3[asia_zone_3$mammal_scientific_name %in% pruned_tree_zone_3$tip.label, ]


# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Temperate & subtropical", "Tropical dry", "Tropical wet"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Habitat = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)

merged_data <- merged_data[!is.na(merged_data$Habitat), ]

regression_lines <- merged_data %>%
  filter(!is.na(Habitat)) %>%
  group_by(Habitat) %>%
  summarise(
    x_range = list(seq(min(log(body_size)), max(log(body_size)), length.out = 100)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Habitat") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)

# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  ) +  # Regression lines (correctly mapped)
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Maximum Fruit diameter (cm)", 
       color = "Mammal Type"  # Updated legend title
  ) +
  theme_classic() +
  facet_wrap(~Habitat, scales = "free_x") +  # Separate panels for each Habitat
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_maximumfruitwidth__nonficus_habitat_phylo__plot.png", width = 15, height = 8, dpi = 300)






#for activity patterns and colour of the fruit 
# Assign colors to `colour_nature`
fruit_colors <- c(
  "Light" = "#A65628",
  "Bright"= "#E41A1C",
  "Dark" = "#377EB8"
)

##for ficus 
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))


Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(plant_trait_ficus, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))
merged_data <- merged_data %>% filter(!is.na(colour_nature))

library(stringr)



# Create a contingency table (count occurrences)
chi_table <- table(merged_data$activity_pattern, merged_data$colour_nature)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)




# Data preparation
Fruit_colour <- merged_data %>%
  #distinct(updated_species, author_id, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(activity_pattern)) %>%  # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(activity_pattern, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(activity_pattern) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(activity_pattern, desc(colour_perc))  # Arrange by descending proportion

# Create the stacked bar graph
lastplot <- ggplot(Fruit_colour, aes(x = activity_pattern, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit Colour"
  ) +
  scale_fill_manual(values = fruit_colors) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    text = element_text(size = 30, color = "black"),  # General text size
    axis.text.x = element_text(size = 30, angle = 45, hjust = 1, color = "black"),  # Match x-axis text style
    axis.title.x = element_blank(),  # Also removes x-axis title
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )


# Display the plot
print(lastplot)


ggsave("activity pattern_metanalysis_Fruitcolour_ficus.jpg", 
       lastplot, 
       width = 10, height = 8, dpi = 300, bg = "white")


#for nonficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_species, mammal_type, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))


Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(plant_trait_ficus, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))
merged_data <- merged_data %>% filter(!is.na(colour_nature))


# Create a contingency table (count occurrences)
chi_table <- table(merged_data$activity_pattern, merged_data$colour_nature)

# Print table
print(chi_table)

# Run Chi-square test
chi_test <- chisq.test(chi_table)

# Print results
print(chi_test)



# Data preparation
Fruit_colour <- merged_data %>%
  #distinct(updated_species, author_id, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(activity_pattern)) %>%  # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(activity_pattern, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(activity_pattern) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(activity_pattern, desc(colour_perc))  # Arrange by descending proportion

# Create the stacked bar graph
lastplot_nonficus <- ggplot(Fruit_colour, aes(x = activity_pattern, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    y = "Relative Proportion of Fruit Consumption",
    fill = "Fruit Colour"
  ) +
  scale_fill_manual(values = fruit_colors) +
  scale_y_continuous(labels = scales::percent) +
  theme(
    text = element_text(size = 30, color = "black"),  # General text size
    axis.text.x = element_text(size = 30, angle = 45, hjust = 1, color = "black"),  # Match x-axis text style
    axis.title.x = element_blank(),  # Also removes x-axis title
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

# Display the plot
print(lastplot_nonficus)


ggsave("activity pattern_metanalysis_Fruitcolour_nonficus.jpg", 
       lastplot_nonficus, 
       width = 10, height = 8, dpi = 300, bg = "white")





#for activity pattern colour nature habitat wise, Non ficus 
#non ficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic)) 


Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(plant_trait_ficus, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))
merged_data <- merged_data %>% filter(!is.na(colour_nature))
merged_data <- merged_data %>% filter(!is.na(Habitat))

interaction_count <- merged_data %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)


# Data preparation
Fruit_colour <- merged_data %>%
  #distinct(updated_species, author_id, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(activity_pattern)) %>%  # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(activity_pattern, Habitat, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(activity_pattern, Habitat) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(activity_pattern, desc(colour_perc))  # Arrange by descending proportion



# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_colour$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_colour %>%
    filter(Habitat == habitat) %>%
    dplyr::select(activity_pattern, colour_nature, colour_freq)  # Select relevant columns
  
  # Convert to contingency table
  chi_table <- xtabs(colour_freq ~ activity_pattern + colour_nature, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results


library(ggplot2)

# Create the plot
fruit_plot <- ggplot(Fruit_colour, aes(x = activity_pattern, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  facet_wrap(~Habitat) +  # Facet wrap by Asia zone
  scale_fill_manual(values = fruit_colors) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent) +  # Convert to percentage format
  labs(x = NULL, y = "Relative Proportion of Fruit Consumption", fill = "Fruit Colour") +  # **Remove x-axis label**
  theme_minimal() +
  theme(
    text = element_text(size = 30, color = "black"),  # General text size
    axis.text.x = element_text(size = 30, angle= 45, hjust = 1, color = "black"),  # **Increase x-axis text size**
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(size = 30, face = "bold")  # **Increase facet title size**
  )

# Save the plot
ggsave("Acitivity patter fruit colour_habitat_nonficus.png", plot = fruit_plot, width = 15, height = 6, dpi = 300, bg = "white")

#for ficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Habitat, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic)) 


Mammal_trait = read.csv ("Mammal_traits.csv")

merged_data <- left_join(plant_trait_ficus, Mammal_trait, by = "mammal_scientific_name")

merged_data <- merged_data %>% filter(!is.na(body_size))
merged_data <- merged_data %>% filter(!is.na(colour_nature))
merged_data <- merged_data %>% filter(!is.na(Habitat))

interaction_count <- merged_data %>%
  group_by(Habitat) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)




# Data preparation
Fruit_colour <- merged_data %>%
  #distinct(updated_species, author_id, .keep_all = TRUE) %>%  # Keep distinct rows
  filter(!is.na(activity_pattern)) %>%  # Filter out NA values
  mutate(colour_nature = str_trim(colour_nature)) %>%  # Trim white spaces
  group_by(activity_pattern, Habitat, colour_nature) %>%
  summarise(colour_freq = n(), .groups = 'drop') %>%  # Summarize data
  group_by(activity_pattern, Habitat) %>%
  mutate(colour_perc = colour_freq / sum(colour_freq)) %>%  # Calculate proportion
  arrange(activity_pattern, desc(colour_perc))  # Arrange by descending proportion

# Prepare a list to store results
chi_results <- list()

# Loop through each Habitat and perform Chi-square test
for (habitat in unique(Fruit_colour$Habitat)) {
  # Filter data for the specific habitat
  habitat_data <- Fruit_colour %>%
    filter(Habitat == habitat) %>%
    dplyr::select(activity_pattern, colour_nature, colour_freq)  # Select relevant columns
  
  # Convert to contingency table
  chi_table <- xtabs(colour_freq ~ activity_pattern + colour_nature, data = habitat_data)
  
  # Perform Chi-square test (only if there are enough data points)
  if (sum(chi_table) > 0) {
    chi_test <- chisq.test(chi_table)
    chi_results[[habitat]] <- chi_test
  } else {
    chi_results[[habitat]] <- "Not enough data for Chi-square test"
  }
}

# Print results for each Habitat
chi_results






library(ggplot2)

# Create the plot
fruit_plot_ficus <- ggplot(Fruit_colour, aes(x = activity_pattern, y = colour_perc, fill = colour_nature)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  facet_wrap(~Habitat) +  # Facet wrap by Asia zone
  scale_fill_manual(values = fruit_colors) +  # Assign manual colors
  scale_y_continuous(labels = scales::percent) +  # Convert to percentage format
  labs(x = NULL, y = "Relative Proportion of Fruit Consumption", fill = "Fruit Colour") +  # **Remove x-axis label**
  theme_minimal() +
  theme(
    text = element_text(size = 30, color = "black"),  # General text size
    axis.text.x = element_text(size = 30, angle= 45, hjust = 1, color = "black"),  # **Increase x-axis text size**
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(size = 30, face = "bold")  # **Increase facet title size**
  )

# Save the plot
ggsave("Acitivity patter fruit colour_habitat_ficus.png", plot = fruit_plot, width = 15, height = 6, dpi = 300, bg = "white")

library(ggplot2)
library(patchwork)

# Make sure ALL your plots have same legend & theme but suppress legend there
lastplot <- lastplot + theme(legend.position = "none")
lastplot_nonficus <- lastplot_nonficus + theme(legend.position = "none")
fruit_plot_ficus <- fruit_plot_ficus + theme(legend.position = "none")
fruit_plot <- fruit_plot + theme(legend.position = "none")

# Add labels
lastplot <- lastplot + labs(tag = "A)")
lastplot_nonficus <- lastplot_nonficus + labs(tag = "B)")
fruit_plot_ficus <- fruit_plot_ficus + labs(tag = "C)")
fruit_plot <- fruit_plot + labs(tag = "D)")



combined_plot <- (lastplot | fruit_plot_ficus) /
  (lastplot_nonficus | fruit_plot) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 30,color = "black" ),
    legend.title = element_text(size = 30, color = "black"),
    plot.tag = element_text(size = 30, face = "bold")
  )

combined_plot



# Save to file with wider width
ggsave("combined_plot_Activity pattern_shared_legend.png", plot = combined_plot,
       width = 42, height = 20, dpi = 300)







###for fruit handling and fruit size###
#for maximum fruit diameter and non ficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


fruit_diameter_max <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter)) %>%
  group_by(mammal_species, mammal_type, Fruit_handle) %>%
  slice_max(fruit_diameter, with_ties = FALSE)  # Selects the row with the max fruit diameter



interaction_count <- fruit_diameter %>%
  group_by(Fruit_handle) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

mammal_species_count <- fruit_diameter %>%
  group_by(Fruit_handle) %>%
  summarize(unique_mammal_species = n_distinct(mammal_species), .groups = "drop")

print(mammal_species_count)


Mammal_trait = read.csv ("Mammal_traits.csv")


merged_data <- left_join(fruit_diameter_max, Mammal_trait, by = c("mammal_scientific_name"))


merged_data <- merged_data %>% filter(!is.na(body_size))

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Gazella_arabica"), ]

mammal_data <- mammal_data[!is.na(mammal_data$Habitat), ]


# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Fruit_handle == "Using opposable thumbs")
asia_zone_2 <- merged_data %>% filter(Fruit_handle == "Using mouthparts")
asia_zone_3 <- merged_data %>% filter(Fruit_handle == "Using hands")
# Remove duplicates, keeping only the first occurrence
asia_zone_3 <- asia_zone_3 %>% distinct(mammal_scientific_name, .keep_all = TRUE) # if there is a duplicate 

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)



# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

# Ensure the species names match the tree tip labels
library(dplyr)


# Filter based on pruned tree and remove duplicates by keeping the max fruit size
asia_zone_1 <- asia_zone_1 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_1$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(fruit_diameter, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_2 <- asia_zone_2 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_2$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(fruit_diameter, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_3 <- asia_zone_3 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_3$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(fruit_diameter, n = 1, with_ties = FALSE) %>%
  ungroup()


asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Using opposable thumbs", "Using mouthparts", "Using hands"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Fruit_handle = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)

regression_lines <- merged_data %>%
  filter(!is.na(Fruit_handle)) %>%
  group_by(Fruit_handle) %>%
  summarise(
    x_range = list(seq(min(log(body_size)), max(log(body_size)), length.out = 100)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Fruit_handle") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)



# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  )  +  # Regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Maximum Fruit Diameter (cm)", 
       color = "Mammal Type") +  # Updated legend title
  theme_classic() +
  facet_wrap(~Fruit_handle, scales = "free_x") +  # Allow x-axis to vary by facet
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_maximumfruitwidth__nonficus_Fruit handling_phylo__plot.png", width = 15, height = 8, dpi = 300)



#for maximum fruit diameter and ficus
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


fruit_diameter_max <- fruit_diameter %>%
  distinct(mammal_species, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter)) %>%
  group_by(mammal_species, mammal_type, Fruit_handle) %>%
  slice_max(fruit_diameter, with_ties = FALSE)  # Selects the row with the max fruit diameter



interaction_count <- fruit_diameter %>%
  group_by(Fruit_handle) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

mammal_species_count <- fruit_diameter %>%
  group_by(Fruit_handle) %>%
  summarize(unique_mammal_species = n_distinct(mammal_species), .groups = "drop")

print(mammal_species_count)


Mammal_trait = read.csv ("Mammal_traits.csv")


merged_data <- left_join(fruit_diameter_max, Mammal_trait, by = c("mammal_scientific_name"))


merged_data <- merged_data %>% filter(!is.na(body_size))

# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp"), ]



# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$fruit_diameter, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Fruit_handle == "Using opposable thumbs")
asia_zone_2 <- merged_data %>% filter(Fruit_handle == "Using mouthparts")
asia_zone_3 <- merged_data %>% filter(Fruit_handle == "Using hands")
# Remove duplicates, keeping only the first occurrence
asia_zone_3 <- asia_zone_3 %>% distinct(mammal_scientific_name, .keep_all = TRUE) # if there is a duplicate 

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)



# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, fruit_diameter)

# Ensure the species names match the tree tip labels
library(dplyr)


# Filter based on pruned tree and remove duplicates by keeping the max fruit size
asia_zone_1 <- asia_zone_1 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_1$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(fruit_diameter, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_2 <- asia_zone_2 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_2$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(fruit_diameter, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_3 <- asia_zone_3 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_3$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(fruit_diameter, n = 1, with_ties = FALSE) %>%
  ungroup()


asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(fruit_diameter ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Using opposable thumbs", "Using mouthparts", "Using hands"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Fruit_handle = Asia_zone)


regression_lines <- merged_data %>%
  filter(!is.na(Fruit_handle)) %>%
  group_by(Fruit_handle) %>%
  summarise(
    x_range = list(seq(min(log(body_size)), max(log(body_size)), length.out = 100)),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Fruit_handle") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)




# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)


# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = fruit_diameter, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  ) +  # Regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Maximum Fruit Diameter (cm)", 
       color = "Mammal Type") +  # Updated legend title
  theme_classic() +
  facet_wrap(~Fruit_handle, scales = "free_x") +  # Allow x-axis to vary by facet
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_maximumfruitwidth__ficus_Fruit handling_phylo__plot.png", width = 15, height = 8, dpi = 300)

#for non ficus and mean fruit width

boot_function <- function(c) {
  data = c
  mn = numeric(1000)
  for (i in 1:1000) {
    temp = sample(data, replace = TRUE)
    mn[i] = mean(temp, na.rm = TRUE)
  }
  lci = quantile(mn, 0.025, na.rm = TRUE)
  boot_mean = median(mn, na.rm = TRUE)  # Renamed from "mean" to avoid conflicts
  rci = quantile(mn, 0.975, na.rm = TRUE)
  results = c(lci, boot_mean, rci)
  return(results)
}





plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_scientific_name, mammal_type, Fruit_handle) %>% reframe(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)



interaction_count <- Fruit_width_boot %>%
  group_by(Fruit_handle) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

mammal_species_count <- Fruit_width_boot %>%
  group_by(Fruit_handle) %>%
  summarize(unique_mammal_species = n_distinct(mammal_scientific_name), .groups = "drop")

print(mammal_species_count)


Mammal_trait = read.csv ("Mammal_traits.csv")


merged_data <- left_join(Fruit_width_boot, Mammal_trait, by = c("mammal_scientific_name"))


# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Bos_taurus", "Bubalus_bubalis", "Bubalus_koreanus", "Civet_sp", "Deer_sp", "Equus_caballus", "Gazella_arabica", "Mongoose_sp", "Ovis_aries", "Semnopithecus vetulus"), ]



# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Fruit_handle == "Using opposable thumbs")
asia_zone_2 <- merged_data %>% filter(Fruit_handle == "Using mouthparts")
asia_zone_3 <- merged_data %>% filter(Fruit_handle == "Using hands")
# Remove duplicates, keeping only the first occurrence
asia_zone_3 <- asia_zone_3 %>% distinct(mammal_scientific_name, .keep_all = TRUE) # if there is a duplicate 

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)



# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

# Ensure the species names match the tree tip labels
library(dplyr)


# Filter based on pruned tree and remove duplicates by keeping the max fruit size
asia_zone_1 <- asia_zone_1 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_1$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(Mean_fruitwidth, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_2 <- asia_zone_2 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_2$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(Mean_fruitwidth, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_3 <- asia_zone_3 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_3$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(Mean_fruitwidth, n = 1, with_ties = FALSE) %>%
  ungroup()


asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Using opposable thumbs", "Using mouthparts", "Using hands"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Fruit_handle = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)



regression_lines <- merged_data %>%
  filter(!is.na(Fruit_handle)) %>%
  group_by(Fruit_handle) %>%
  summarise(
    x_range = list(seq(
      min(log(body_size), na.rm = TRUE),  # Ignore NAs
      max(log(body_size), na.rm = TRUE), 
      length.out = 100
    )),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Fruit_handle") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)



# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = Mean_fruitwidth, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  ) +  # Regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Mean Fruit Diameter (cm)", 
       color = "Mammal Type") +  # Updated legend title
  theme_classic() +
  facet_wrap(~Fruit_handle, scales = "free_x") +  # Allow x-axis to vary by facet
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_meanfruitwidth__nonficus_Fruit handling_phylo__plot.png", width = 15, height = 8, dpi = 300)



#for ficus and mean fruit width
plant_trait = read.csv ("Mammal review_for analysis_csv.csv")

plant_trait_ficus <- plant_trait %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(fruit_nature %in% c("Fig", "fig")) %>%
  filter(!is.na(native_exotic))

fruit_diameter <- plant_trait_ficus %>% 
  mutate(
    fruit_width = (fruit_breadth_l + fruit_breadth_h) / 2,
    fruit_diameter = (fruit_diameter_l + fruit_diameter_h) / 2  # Fixed typo
  ) %>%
  mutate(
    fruit_diameter = ifelse(is.na(fruit_diameter), fruit_width, fruit_diameter)
  )



fruit_diameter <- fruit_diameter %>%
  distinct(mammal_scientific_name, mammal_type, Fruit_handle, updated_species, .keep_all = TRUE) %>%
  filter(!is.na(fruit_diameter))


Fruit_width_boot= fruit_diameter %>% group_by(mammal_scientific_name, mammal_type, Fruit_handle) %>% reframe(
  lci_fruitwidth = boot_function(fruit_diameter)[1],
  Mean_fruitwidth = boot_function(fruit_diameter)[2],
  Hci_fruitwidth = boot_function(fruit_diameter)[3]
)



interaction_count <- Fruit_width_boot %>%
  group_by(Fruit_handle) %>%
  summarize(interaction_count = n(), .groups = "drop")

print(interaction_count)

mammal_species_count <- Fruit_width_boot %>%
  group_by(Fruit_handle) %>%
  summarize(unique_mammal_species = n_distinct(mammal_scientific_name), .groups = "drop")

print(mammal_species_count)


Mammal_trait = read.csv ("Mammal_traits.csv")


merged_data <- left_join(Fruit_width_boot, Mammal_trait, by = c("mammal_scientific_name"))


# Load phylogenetic tree
tree <- read.nexus("Complete_phylogeny.nex")  # Replace with your file path

#write.csv(tree$tip.label, "mammal_species_in_tree.csv", row.names = FALSE)
#cat("List saved as 'mammal_species_in_tree.csv'\n")



# Load dataset
mammal_data <-  merged_data # Replace with your file path

# Function to extract species names from each tree in multiPhylo object
extract_species_names <- function(tree) {
  species <- tree$tip.label  # Extract tip labels
  species <- as.character(unlist(species))  # Ensure it's a character vector
  return(species)
}

# Apply function to each tree in multiPhylo object
tree_species_list <- lapply(tree, extract_species_names)

# Print first few species from the first tree (for verification)
print(head(tree_species_list[[1]]))


# Ensure species names are character vectors
data_species <- as.character(mammal_data$mammal_scientific_name)


# No need to use `tree$tip.label` directly since we have `tree_species_list`

# Check mismatches for each tree
mismatches <- lapply(tree_species_list, function(tree_species) {
  missing_in_tree <- setdiff(data_species, tree_species)
  missing_in_data <- setdiff(tree_species, data_species)
  
  list(missing_in_tree = missing_in_tree, missing_in_data = missing_in_data)
})

# Print mismatches for the first tree as an example
cat("Species in dataset but missing from tree:\n", paste(mismatches[[1]]$missing_in_tree, collapse = ", "), "\n")



mammal_data <- mammal_data[!mammal_data$mammal_scientific_name %in% c("Bear_sp", "Civet_sp", "Deer_sp"), ]



# Extract response variable (Mean Fruit Diameter)
fruit_diameter <- setNames(mammal_data$Mean_fruitwidth, mammal_data$mammal_scientific_name)

# Extract predictor variable (Mammal Body Size)
body_size <- setNames(mammal_data$body_size, mammal_data$mammal_scientific_name)

# Function to prune a tree, keeping only species present in the dataset
prune_tree <- function(single_tree, species_list) {
  species_to_keep <- intersect(single_tree$tip.label, species_list)
  pruned_tree <- drop.tip(single_tree, setdiff(single_tree$tip.label, species_to_keep))
  return(pruned_tree)
}

# Prune all trees in the multiPhylo object
pruned_trees <- lapply(tree, prune_tree, species_list = data_species)




# Load necessary libraries
library(ape)        # For phylogenetic trees
library(caper)      # For PGLS regression
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation

# split data by habitat 
asia_zone_1 <- merged_data %>% filter(Fruit_handle == "Using opposable thumbs")
asia_zone_2 <- merged_data %>% filter(Fruit_handle == "Using mouthparts")
asia_zone_3 <- merged_data %>% filter(Fruit_handle == "Using hands")
# Remove duplicates, keeping only the first occurrence
asia_zone_3 <- asia_zone_3 %>% distinct(mammal_scientific_name, .keep_all = TRUE) # if there is a duplicate 

# Convert to a standard data frame
asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Prune trees to match the species in each zone
pruned_tree_zone_1 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_1$mammal_scientific_name)
)

pruned_tree_zone_2 <- drop.tip(
  pruned_trees[[1]], 
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_2$mammal_scientific_name)
)

pruned_tree_zone_3 <- drop.tip(
  pruned_trees[[1]],  # Ensure correct index if using multiple trees
  setdiff(pruned_trees[[1]]$tip.label, asia_zone_3$mammal_scientific_name)
)



# Select relevant columns
asia_zone_1 <- asia_zone_1 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_2 <- asia_zone_2 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

asia_zone_3 <- asia_zone_3 %>%
  dplyr::select(mammal_scientific_name, mammal_type, body_size, Mean_fruitwidth)

# Ensure the species names match the tree tip labels
library(dplyr)


# Filter based on pruned tree and remove duplicates by keeping the max fruit size
asia_zone_1 <- asia_zone_1 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_1$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(Mean_fruitwidth, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_2 <- asia_zone_2 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_2$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(Mean_fruitwidth, n = 1, with_ties = FALSE) %>%
  ungroup()

asia_zone_3 <- asia_zone_3 %>%
  filter(mammal_scientific_name %in% pruned_tree_zone_3$tip.label) %>%
  group_by(mammal_scientific_name) %>%
  slice_max(Mean_fruitwidth, n = 1, with_ties = FALSE) %>%
  ungroup()


asia_zone_1 <- as.data.frame(asia_zone_1)
asia_zone_2 <- as.data.frame(asia_zone_2)
asia_zone_3 <- as.data.frame(asia_zone_3)

# Create comparative data objects
comp_data_zone_1 <- comparative.data(
  phy = pruned_tree_zone_1, 
  data = asia_zone_1, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)

comp_data_zone_2 <- comparative.data(
  phy = pruned_tree_zone_2, 
  data = asia_zone_2, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)


comp_data_zone_3 <- comparative.data(
  phy = pruned_tree_zone_3, 
  data = asia_zone_3, 
  names.col = "mammal_scientific_name", 
  vcv = TRUE, 
  na.omit = TRUE
)



# 4️⃣ Run PGLS for each Asia zone
pgls_zone_1 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_1, lambda = "ML")
pgls_zone_2 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_2, lambda = "ML")
pgls_zone_3 <- pgls(Mean_fruitwidth ~ log(body_size), data = comp_data_zone_3, lambda = "ML")

# Print model summaries
summary(pgls_zone_1)
summary(pgls_zone_2)
summary(pgls_zone_3)

# 5️⃣ Extract model coefficients for plotting
model_coeffs <- data.frame(
  Asia_zone = c("Using opposable thumbs", "Using mouthparts", "Using hands"),
  intercept = c(coef(pgls_zone_1)[1], coef(pgls_zone_2)[1], coef(pgls_zone_3)[1]),
  slope = c(coef(pgls_zone_1)[2], coef(pgls_zone_2)[2], coef(pgls_zone_3)[2])
)

model_coeffs <- model_coeffs %>% rename(Fruit_handle = Asia_zone)

# 6️⃣ Create the facet-wrapped PGLS plot
library(ggplot2)
library(viridis)


regression_lines <- merged_data %>%
  filter(!is.na(Fruit_handle)) %>%
  group_by(Fruit_handle) %>%
  summarise(
    x_range = list(seq(
      min(log(body_size), na.rm = TRUE),  # Ignore NAs
      max(log(body_size), na.rm = TRUE), 
      length.out = 100
    )),
    .groups = "drop"
  ) %>%
  tidyr::unnest(cols = c(x_range)) %>%
  rename(log_body_size = x_range) %>%
  left_join(model_coeffs, by = "Fruit_handle") %>%
  mutate(Mean_fruitwidth = intercept + slope * log_body_size)







# 6️⃣ Create the facet-wrapped PGLS plot
ggplot(merged_data, aes(x = log(body_size), y = Mean_fruitwidth, color = mammal_type)) +
  geom_point(size = 3, alpha = 0.7) +  # Data points
  geom_line(
    data = regression_lines,
    aes(x = log_body_size, y = Mean_fruitwidth),
    inherit.aes = FALSE,
    color = "black",
    size = 1.2
  ) +  # Regression lines
  scale_color_viridis_d(option = "D") +  # Colorblind-friendly colors
  labs(x = "Log Body Size", 
       y = "Mean Fruit Diameter (cm)", 
       color = "Mammal Type") +  # Updated legend title
  theme_classic() +
  facet_wrap(~Fruit_handle, scales = "free_x") +  # Allow x-axis to vary by facet
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),  # Larger facet labels
    legend.position = "right",  # Moves legend to the side
    legend.text = element_text(size = 14),  # Bigger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Bigger legend title
    panel.border = element_rect(color = "black", fill = NA, size = 1.2)  # Adds box around each facet
  )



# Save the plot
ggsave("Asia_mammal_meanfruitwidth__ficus_Fruit handling_phylo__plot.png", width = 15, height = 8, dpi = 300)












