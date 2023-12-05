## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 8) Gini Index
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(readxl)
library(forcats)

library(gridExtra)
library(grid)

wiid <- read_excel("/Users/carterkruse/Data Viz Final/data/WIID_31MAY2021_0/WIID_31MAY2021.xlsx")
glimpse(wiid)

plot1 <- wiid %>%
  filter(region_un == "Asia", year == 2010) %>%
  mutate(avg_gini = mean(gini, na.rm = TRUE)) %>%
  group_by(country) %>%
  summarize(gini_difference = mean(gini, na.rm = TRUE) - avg_gini) %>%
  distinct() %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(country, gini_difference), y = gini_difference),
           fill = "#6779C0") +
  coord_flip() +
  labs(title = "Asian Gini Index Scores") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        title = element_text(color = "#000000", size = 7),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 6, vjust = 1),
        axis.text.y = element_text(size = 6, hjust = 1),
        axis.ticks = element_line(color = "#000000"),
        axis.ticks.length = unit(0.8, "mm"))

plot1

plot2 <- wiid %>%
  filter(region_un == "Americas", year == 2010) %>%
  mutate(avg_gini = mean(gini, na.rm = TRUE)) %>%
  group_by(country) %>%
  summarize(gini_difference = mean(gini, na.rm = TRUE) - avg_gini) %>%
  distinct() %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(country, gini_difference), y = gini_difference),
           fill = "#236D0B") +
  coord_flip() +
  labs(title = "Americas Gini Index Scores", caption = "Source: World Income Inequality Database") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        title = element_text(color = "#000000", size = 7),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 6, vjust = 1),
        axis.text.y = element_text(size = 6, hjust = 1),
        axis.ticks = element_line(color = "#000000"),
        axis.ticks.length = unit(0.8, "mm"))

plot2

final_plot <- arrangeGrob(plot1, plot2, ncol = 2, 
                          top = textGrob("Centered Gini Index Scores by Region in 2010", gp = gpar(fontsize = 8)),
                          widths = c(900, 900), heights = 800)

final_plot

ggsave(filename = "gini_index.png", plot = final_plot, width = 9, height = 5, scale = 1, units = "in")
ggsave(filename = "gini_index.pdf", plot = final_plot, width = 9, height = 5, scale = 1, units = "in")

