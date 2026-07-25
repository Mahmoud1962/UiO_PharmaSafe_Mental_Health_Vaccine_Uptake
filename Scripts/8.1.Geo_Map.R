################################################################################
# Script: 8.1.Geo_Map.R
#
# Purpose:
#   Plot the geographical differences in influenza and COVID-19 
#   Vaccine uptake among pregnant and older adult women.
################################################################################
library(dplyr)
library(csmaps)
library(ggplot2)
library(dplyr)

# Step 1: Calculate rape percentages from your individual data
uptake_pct <- preg_covid_risk_imp_iptw[[1]] %>%
  group_by(county_2021) %>%
  summarize(
    n_total = n(),
    n_vacc = sum(vaccinated == 1, na.rm = TRUE),
    uptake = (n_vacc/ n_total) * 100
  ) 
# Load the county map data (adjust year based on what's available)
norway_counties <- csmaps::nor_county_map_b2020_default_dt
# If 2024 isn't available, try 2020: nor_county_map_b2020_default_dt

# Look at the structure
str(norway_counties)
head(norway_counties)

# If your county names in the map data differ, adjust accordingly

# Check actual county names in the dataset
unique(norway_counties$location_code)
norway_counties$location_code[norway_counties$location_code == "county_nor03"] <- "Eastern"
norway_counties$location_code[norway_counties$location_code == "county_nor11"] <- "Southern"
norway_counties$location_code[norway_counties$location_code == "county_nor15"] <- "Western"
norway_counties$location_code[norway_counties$location_code == "county_nor18"] <- "Central"
norway_counties$location_code[norway_counties$location_code == "county_nor30"] <- "Eastern"
norway_counties$location_code[norway_counties$location_code == "county_nor34"] <- "Central"
norway_counties$location_code[norway_counties$location_code == "county_nor38"] <- "Eastern"
norway_counties$location_code[norway_counties$location_code == "county_nor42"] <- "Southern"
norway_counties$location_code[norway_counties$location_code == "county_nor46"] <- "Western"
norway_counties$location_code[norway_counties$location_code == "county_nor50"] <- "Central"
norway_counties$location_code[norway_counties$location_code == "county_nor54"] <- "Northern"


# Merge region mapping with the map data
norway_map_with_region <- norway_counties %>%
  left_join(uptake_pct, by = c("location_code" = "county_2021"))

# Step 6: Create the map
Preg_COVID_risk_plot <- ggplot(data = norway_map_with_region) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = uptake),
    color = "black",
    linewidth = 0
  ) +
  scale_fill_gradient(
    low = "lightyellow", 
    high = "darkred",
    name = "Uptake Percentage",
    na.value = "grey80"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "COVID-19 Vaccine Uptake in Pregnancies with risk by Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
#############################
# Step 1: Calculate rape percentages from your individual data
uptake_pct <- preg_covid_norisk_imp_iptw[[1]] %>%
  group_by(county_2021) %>%
  summarize(
    n_total = n(),
    n_vacc = sum(vaccinated == 1, na.rm = TRUE),
    uptake = (n_vacc/ n_total) * 100
  ) 

# Merge region mapping with the map data
norway_map_with_region <- norway_counties %>%
  left_join(uptake_pct, by = c("location_code" = "county_2021"))

# Step 6: Create the map
Preg_COVID_norisk_plot <- ggplot(data = norway_map_with_region) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = uptake),
    color = "black",
    linewidth = 0
  ) +
  scale_fill_gradient(
    low = "lightyellow", 
    high = "darkred",
    name = "Uptake Percentage",
    na.value = "grey80"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "COVID-19 Vaccine Uptake in Pregnancies without risk by Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
#######################
# Step 1: Calculate rape percentages from your individual data
uptake_pct <- preg_flu_norisk_imp_iptw[[1]] %>%
  group_by(county_2017) %>%
  summarize(
    n_total = n(),
    n_vacc = sum(all.seasons == 1, na.rm = TRUE),
    uptake = (n_vacc/ n_total) * 100
  ) 

# Merge region mapping with the map data
norway_map_with_region <- norway_counties %>%
  left_join(uptake_pct, by = c("location_code" = "county_2017"))

# Step 6: Create the map
Preg_Influenza_norisk_plot <- ggplot(data = norway_map_with_region) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = uptake),
    color = "black",
    linewidth = 0
  ) +
  scale_fill_gradient(
    low = "lightyellow", 
    high = "darkred",
    name = "Uptake Percentage",
    na.value = "grey80"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Influenza Vaccine Uptake in Pregnancies without risk by Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
#######################
# Step 1: Calculate rape percentages from your individual data
uptake_pct <- preg_flu_risk_imp_iptw[[1]] %>%
  group_by(county_2017) %>%
  summarize(
    n_total = n(),
    n_vacc = sum(all.seasons == 1, na.rm = TRUE),
    uptake = (n_vacc/ n_total) * 100
  ) 

# Merge region mapping with the map data
norway_map_with_region <- norway_counties %>%
  left_join(uptake_pct, by = c("location_code" = "county_2017"))

# Step 6: Create the map
Preg_Influenza_risk_plot <- ggplot(data = norway_map_with_region) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = uptake),
    color = "black",
    linewidth = 0
  ) +
  scale_fill_gradient(
    low = "lightyellow", 
    high = "darkred",
    name = "Uptake Percentage",
    na.value = "grey80"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Influenza Vaccine Uptake in Pregnancies with risk by Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
#######################
# Step 1: Calculate rape percentages from your individual data
uptake_pct <- old_flu_imp_iptw[[1]] %>%
  group_by(county_2017) %>%
  summarize(
    n_total = n(),
    n_vacc = sum(all.seasons == 1, na.rm = TRUE),
    uptake = (n_vacc/ n_total) * 100
  ) 

# Merge region mapping with the map data
norway_map_with_region <- norway_counties %>%
  left_join(uptake_pct, by = c("location_code" = "county_2017"))

# Step 6: Create the map
Old_Influenza_plot <- ggplot(data = norway_map_with_region) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = uptake),
    color = "black",
    linewidth = 0
  ) +
  scale_fill_gradient(
    low = "lightyellow", 
    high = "darkred",
    name = "Uptake Percentage",
    na.value = "grey80"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "Influenza Vaccine Uptake in Older Adults by Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
#######################
# Step 1: Calculate rape percentages from your individual data
uptake_pct <- old_covid_imp_iptw[[1]] %>%
  group_by(county_2021) %>%
  summarize(
    n_total = n(),
    n_vacc = sum(all.seasons == 1, na.rm = TRUE),
    uptake = (n_vacc/ n_total) * 100
  ) 

# Merge region mapping with the map data
norway_map_with_region <- norway_counties %>%
  left_join(uptake_pct, by = c("location_code" = "county_2021"))

# Step 6: Create the map
Old_COVID_plot <- ggplot(data = norway_map_with_region) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = uptake),
    color = "black",
    linewidth = 0
  ) +
  scale_fill_gradient(
    low = "lightyellow", 
    high = "darkred",
    name = "Uptake Percentage",
    na.value = "grey80"
  ) +
  coord_quickmap() +
  theme_void() +
  labs(
    title = "COVID-19 Vaccine Uptake in Older Adults by Region"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )
######################################################
library(gridExtra)
grid.arrange(Old_COVID_plot, Old_Influenza_plot, Preg_COVID_norisk_plot, Preg_Influenza_norisk_plot, Preg_COVID_risk_plot, Preg_Influenza_risk_plot,
             ncol = 2, nrow = 3)



