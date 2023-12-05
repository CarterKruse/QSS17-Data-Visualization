## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 6) Crimes
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

crimes <- read_csv("/Users/carterkruse/Data Viz Final/data/us_crimes_dat/us_crimes_histdat_f21.csv")
glimpse(crimes)

final_plot <- crimes %>%
  mutate(arrests_per_thousand = arrests / (population / 1000)) %>%
  filter(arrests_per_thousand < 500) %>% # Error in data.
  ggplot() +
  ylim(0, 450) +
  geom_line(aes(x = year, y = arrests_per_thousand, color = city)) +
  scale_color_manual(values = c(rep("#E8E8E8", times = 22), "#FF8785")) +
  labs(title = "U.S. Arrests, 1860-1920", subtitle = "23 Major Historial Cities", 
       x = "Year", y = "Total Arrests per 1000 People") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 7),
        axis.title = element_text(color = "#000000", size = 6.5),
        axis.text.x = element_text(size = 5, vjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1),
        legend.position = "none") +
  annotate("text", x = 1871, y = 310, label = "Washington, D.C.", color = "#000000", 
           size = 2, hjust = 0, vjust = 0)

final_plot

ggsave(filename = "crimes.png", plot = final_plot, width = 8, height = 5.5, scale = 1, units = "in")
ggsave(filename = "crimes.pdf", plot = final_plot, width = 8, height = 5.5, scale = 1, units = "in")

