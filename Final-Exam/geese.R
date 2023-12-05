## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 1) Geese
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(sp)
library(sf)
library(rgdal)
library(ggspatial)

land <- st_read("/Users/carterkruse/Data Viz Final/data/geese_problem/natural_usa/ne_10m_land/ne_10m_land.shp")
glimpse(land)

lakes <- st_read("/Users/carterkruse/Data Viz Final/data/geese_problem/natural_usa/ne_10m_lakes/ne_10m_lakes.shp")
glimpse(lakes)

rivers <- st_read("/Users/carterkruse/Data Viz Final/data/geese_problem/natural_usa/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
glimpse(rivers)

geese <- read_csv("/Users/carterkruse/Data Viz Final/data/geese_problem/geese.csv")
glimpse(geese)

final_plot <- land %>%
  ggplot() +
  geom_sf(size = 0.15, fill = "#C0C2CE", color = "#525258") +
  geom_sf(data = lakes, fill = "#0084FF", color = "#0084FF") +
  geom_sf(data = rivers, fill = "#0062EC", color = "#0062EC") +
  geom_point(data = geese, aes(x = `location-long`, y = `location-lat`), color = "#C92C20", size = 0.1) +
  coord_sf(xlim = c(-3.5, 89), ylim = c(34.8, 84)) +
  labs(title = "White-Fronted Geese in Europe and Northern Asia \n Observations of Migrating Geese in Red",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 7),
        axis.title = element_text(color = "#000000", size = 6.5),
        axis.text.x = element_text(size = 5.5, vjust = 1),
        axis.text.y = element_text(size = 5.5, hjust = 1))

final_plot

ggsave(filename = "geese.png", plot = final_plot, width = 6, height = 6, scale = 1, units = "in")
ggsave(filename = "geese.pdf", plot = final_plot, width = 6, height = 6, scale = 1, units = "in")

