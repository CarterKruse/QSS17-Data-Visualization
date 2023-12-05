## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 2) Songbirds
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

library(lubridate)

library(gganimate)
library(magick)
library(gifski)

world <- map_data("world") %>%
  filter(region != "Antarctica")
glimpse(world)

points <- st_read("/Users/carterkruse/Data Viz Final/data/songbirds/points.shp")
glimpse(points)

songbirds <- points %>%
  filter(tag_ident == "4210-002" | tag_ident == "4210-004") %>%
  arrange(timestamp)

glimpse(songbirds)

base_map <- world %>%
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "#DFDFDF", color = "#868686", size = 0.1) +
  geom_point(data = songbirds, aes(x = long, y = lat, color = tag_ident),
             size = 0.05) +
  coord_sf(xlim = c(-35, -140), ylim = c(-70, 80)) +
  scale_color_manual(values = c("#F29309", "#0E0E7E")) +
  scale_y_continuous(limits = c(-70, 80), breaks = c(-50, 0, 50)) +
  labs(title = "Two Blackpoll Warblers Migrating in the Americas",
       subtitle = "{closest_state}") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 9),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 7, hjust = 1))

base_map

final_animation <- base_map +
  transition_states(states = timestamp) +
  shadow_wake(wake_length = 0.9,
              size = NULL,
              falloff = "linear")

animate(final_animation, duration = 20, fps = 10, height = 900, width = 600)
anim_save("songbirds.gif")

