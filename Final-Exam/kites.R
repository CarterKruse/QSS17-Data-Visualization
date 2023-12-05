## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 11) Kites
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

library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires) # install_github("ropensci/rnaturalearthhires")

library(ggtext)

kites <- read_csv("/Users/carterkruse/Data Viz Final/data/black_kites/black_kites")
glimpse(kites)

kite_10 <- kites %>%
  filter(`tag-local-identifier` == "kite10")
glimpse(kite_10)

world <- ne_countries(scale = 10, returnclass = "sf")
glimpse(world)

base_plot <- world %>%
  ggplot() +
  geom_sf(fill = "#E5E5E5", color = "#000000", size = 0.2) +
  geom_point(data = kite_10, aes(x = `location-long`, y = `location-lat`), color = "#0000FF", size = 0.05) +
  coord_sf(xlim = c(-6.25, -5), ylim = c(35.75, 36.25)) +
  labs(title = "European Black Kites \"Summer\" in the Strait of Gibraltar",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 7),
        axis.title = element_text(color = "#000000", size = 6),
        axis.text.x = element_text(size = 5, vjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1))

base_plot

final_plot <- base_plot + 
  geom_richtext(x = -5.68, y = 36.2, label = "SPAIN",
                family = "sans", fontface = "italic", size = 2,
                color = "#E5E5E5", fill = "#E5E5E5", text.colour = "#7A7A7A",
                label.padding = unit(c(0, 0, 0, 0), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")) +
  geom_richtext(x = -5.6, y = 35.8, label = "MOROCCO",
                family = "sans", fontface = "italic", size = 2,
                color = "#E5E5E5", fill = "#E5E5E5", text.colour = "#7A7A7A",
                label.padding = unit(c(0, 0, 0, 0), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")) +
  geom_richtext(x = -5.74, y = 35.9, label = "Strait of Gibraltar",
                family = "sans", fontface = "italic", size = 2,
                color = "#FFFFFF", fill = "#FFFFFF", text.colour = "#7A7A7A",
                label.padding = unit(c(0, 0, 0, 0), "lines"),
                label.margin = unit(c(0, 0, 0, 0), "lines"),
                label.r = unit(0, "lines")) +
  annotate("rect", xmin = -5.45, xmax = -5.05, ymin = 35.95, ymax = 36.05, alpha = 0.2) +
  annotate("text", x = -5.25, y = 36.02, label = "Black Kites are migratory and spend",
           color = "#707070", fontface = "italic", size = 2, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = -5.25, y = 36, label = "the winter in Africa south of the Sahara.",
           color = "#707070", fontface = "italic", size = 2, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = -5.25, y = 35.98, label = "See Kite 10's summer flight in blue.",
           color = "#707070", fontface = "italic", size = 2, hjust = 0.5, vjust = 0.5)

final_plot

ggsave(filename = "kites.png", plot = final_plot, width = 8, height = 5, scale = 1, units = "in")
ggsave(filename = "kites.pdf", plot = final_plot, width = 8, height = 5, scale = 1, units = "in")

