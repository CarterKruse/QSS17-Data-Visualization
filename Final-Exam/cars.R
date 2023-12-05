## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 10) Cars
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(stringr)

library(rvest)
library(ggtext)
library(ggrepel)

cars <- read_html("https://en.wikipedia.org/wiki/Production_car_speed_record")

cars_table <- cars %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE)

cars_data <- data.frame(cars_table[[1]]); cars_data

final_cars_data <- cars_data %>%
  mutate(speed = as.numeric(str_sub(sub("h.*", "", Top.speed), start = 1, end = -5))) %>%
  select(Year, Make.and.model, speed)

final_cars_data

final_plot <- final_cars_data %>%
  ggplot(aes(x = Year, y = speed)) +
  geom_point(size = 0.6, color = "#5763FF") +
  geom_smooth(span = 4, color = "#FD2E0D", size = 0.6, alpha = 0.5) +
  geom_text_repel(aes(label = Make.and.model), size = 2,
                  force = 4, nudge_x = 0, nudge_y = 0, seed = 10,
                  max.overlaps = Inf) +
  labs(title = "Street Legal Car Speed Records", x = "Year", y = "Speed in Kilometers Per Hour (KPH)",
       caption = "Sources: https://en.wikipedia.org/wiki/Production_car_speed_record") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 6),
        axis.title = element_text(color = "#000000", size = 6.5),
        axis.text.x = element_text(size = 5, vjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1),
        plot.caption = element_text(color = "#000000", size = 6))

final_plot

ggsave(filename = "cars.png", plot = final_plot, width = 8, height = 5, scale = 1, units = "in")
ggsave(filename = "cars.pdf", plot = final_plot, width = 8, height = 5, scale = 1, units = "in")

final_plot_updated <- final_cars_data %>%
  ggplot(aes(x = Year, y = speed)) +
  geom_point(size = 0.6, color = "#5763FF") +
  geom_smooth(span = 4, color = "#FD2E0D", size = 0.6, alpha = 0.5) +
  geom_label_repel(aes(label = Make.and.model), size = 2,
                   force = 18, nudge_x = 0, nudge_y = 0, seed = 10,
                   max.overlaps = Inf, min.segment.length = 0) +
  labs(title = "Street Legal Car Speed Records", x = "Year", y = "Speed in Kilometers Per Hour (KPH)",
       caption = "Sources: https://en.wikipedia.org/wiki/Production_car_speed_record") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 6),
        axis.title = element_text(color = "#000000", size = 6.5),
        axis.text.x = element_text(size = 5, vjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1),
        plot.caption = element_text(color = "#000000", size = 6))

final_plot_updated

ggsave(filename = "cars_updated.png", plot = final_plot_updated, width = 8, height = 5, scale = 1, units = "in")
ggsave(filename = "cars_updated.pdf", plot = final_plot_updated, width = 8, height = 5, scale = 1, units = "in")

