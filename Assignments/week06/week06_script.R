## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 6: Advanced ggplot2
##
## Name: Carter Kruse
## Date: October 24th, 2022

## To deal with missing data points, we will remove the 'NA' missing data points
## inside the statistical functions like 'mean', 'median', 'min' and 'max' with the
## 'na.rm = TRUE' argument.

## In this assignment, we will fix all labels (x-axis, y-axis, titles, legend)
## and use hex codes to make colors.

library(tidyverse)
library(ggridges)
library(ggtext)
library(readxl)
library(devtools)

## Creating a simple theme for plots.

theme_simple <- theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
                      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
                      panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
                      panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
                      # 
                      title = element_text(color = "#000000", size = 10, family = "sans"),
                      axis.title = element_text(color = "#000000", size = 9, family = "sans"),
                      axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
                      axis.text.y = element_text(size = 8, hjust = 1, family = "sans"),
                      # 
                      legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
                      legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
                      legend.title = element_text(color = "#000000", size = 9, family = "sans"),
                      legend.text = element_text(color = "#000000", size = 8, family = "sans"),
                      legend.key.size = unit(5, "mm"))

## Opening the WIDD data set and naming it 'wiid'.

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")

## Looking at the data set.

glimpse(wiid)

## 1) Density Ridgeline Plot

## Creating a density ridgeline plot of Europe by deciles of income (d1 to d10).
## This requires data transformation ('pivot_longer').

## The final product is a set of 10 density curves that overlap to some extent.
## The alpha is 0.7, keeping the colors fairly strong, while allowing a slight view
## of the overlapping curves.

## The categorical variable is on the y-axis, while the continuous variable is
## on the x-axis.

wiid %>%
  filter(region_un == "Europe") %>%
  pivot_longer(names_to = "decile", values_to = "percent", d1:d10) %>%
  mutate(decile_ordered = factor(decile, ordered = TRUE, 
                                 levels = c("d1", "d2", "d3", "d4", "d5", "d6",
                                            "d7", "d8", "d9", "d10"))) %>%
  select(country, year, gini, decile_ordered, percent) %>%
  ggplot(aes(x = percent, y = decile_ordered, fill = decile_ordered)) +
  geom_density_ridges(alpha = 0.7, scale = 2) +
  labs(title = "Deciles Of Income (d1 to d10) in Europe",
       subtitle = "The deciles represent percentages of countries’ population and their income held.", 
       x = "Percentage Of Income", y = "Decile") +
  scale_color_manual(values = c("#440154", "#482878", "#3e4989", "#31688e", "#26828e",
                                "#1f9e89", "#35b779", "#6ece58", "#b5de2b", "#fde725")) +
  theme_simple +
  theme(legend.position = "none")

## 2) Waffle Plot

library(ggwaffle)
library(waffle)

## The waffle plot is a strong competitor to the pie chart.

## Filtering for "Europe" and creating a waffle plot representing the rounded
## percentages of income held by each decile.

## This requires data transformation ('pivot_longer'). We use a factor to make
## sure the deciles are in the right order.

## Standard ColorBrewer Colors - Spectral Palette

base <- wiid %>%
  filter(region_un == "Europe") %>%
  pivot_longer(names_to = "decile", values_to = "percent", d1:d10) %>%
  mutate(decile_ordered = factor(decile, ordered = TRUE, 
                                 levels = c("d1", "d2", "d3", "d4", "d5", "d6",
                                            "d7", "d8", "d9", "d10"))) %>%
  select(country, year, gini, decile_ordered, percent) %>%
  group_by(decile_ordered) %>%
  summarize(avg_percent = mean(percent, na.rm = TRUE)) %>%
  mutate(avg_percent_rounded = round(avg_percent)) %>%
  uncount(avg_percent_rounded) %>%
  .[c(1:100), ] %>% # Removing the last row of the data table, to ensure there are 100 rows (100%).
  mutate(x = rep(1:10, each = 10),
         y = rep(1:10, 10))

base %>%
  ggplot(aes(x, y, fill = decile_ordered)) +
  geom_waffle(size = 1) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Deciles Of Income (d1 to d10) in Europe",
       subtitle = "The deciles represent percentages of countries’ population and their income held.",
       fill = "Decile",
       caption = "The 10th decile is represented in purple.") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
      panel.grid = element_blank(),
      # 
      title = element_text(color = "#000000", size = 10, family = "sans"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      # 
      legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
      legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
      legend.title = element_text(color = "#000000", size = 9, family = "sans"),
      legend.text = element_text(color = "#000000", size = 8, family = "sans"),
      legend.key.size = unit(5, "mm"))

## The default colors are modified, specifically so that the 10th decile (top group)
## stands out a bit more from the other deciles as a group.

base %>%
  ggplot(aes(x, y, fill = decile_ordered)) +
  geom_waffle(size = 1) +
  scale_fill_manual(values = c(rep("#9A9A9A", 9), "#DB3434"))  +
  labs(title = "Deciles Of Income (d1 to d10) in Europe",
       subtitle = "The deciles represent percentages of countries’ population and their income held.",
       fill = "Decile",
       caption = "The 10th decile is highlighted in red.") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

## 3) Violin Plot

## Creating a violin plot that show Africa's Gini Index values by Africa's sub-regions.
## A violin plot is just a symmetric density curve replacing what would normally
## be something like a box plot.

## The coordinates are flipped so that the 'violins' are horizontal.

glimpse(wiid)

## Standard Viridis Colors

wiid %>%
  filter(region_un == "Africa") %>%
  ggplot(aes(x = region_un_sub, y = gini, fill = region_un_sub, color = region_un_sub)) +
  geom_violin(alpha = 0.7) +
  coord_flip() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(title = "Africa's Gini Index Values by UN Sub-Region", x = "UN Sub-Region",
       y = "Gini Index", fill = "UN Sub-Region", color = "UN Sub-Region") +
  theme_simple +
  theme(axis.title.y = element_blank())

## Manual Colors

wiid %>%
  filter(region_un == "Africa") %>%
  ggplot(aes(x = region_un_sub, y = gini, fill = region_un_sub, color = region_un_sub)) +
  geom_violin(alpha = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("#68ADD4", "#BCD9BD", "#FFFB8F", "#FFBD86", "#F65A6D")) +
  scale_color_manual(values = c("#68ADD4", "#BCD9BD", "#FFFB8F", "#FFBD86", "#F65A6D")) +
  labs(title = "Africa's Gini Index Values by UN Sub-Region",
       y = "Gini Index", fill = "UN Sub-Region", color = "UN Sub-Region") +
  theme_simple +
  theme(axis.title.y = element_blank())

## 4) Data Transformation

library(dplyr)

## There is no visualization required. Using the WIID data set, we split it into
## many smaller data sets by UN Sub-Region using 'group_by' followed by 'group_split'
## (the new dplyr version of 'split'), calling the new data list 'wiid_split'.

## The new object is a list of small data sets.

wiid_split <- wiid %>%
  group_by(region_un_sub) %>%
  group_split()

wiid_split

## The 'map' function (from the 'purrr' library) is used in the following process.

library(purrr)

## The final result may be the full data set, or a summarized table.

## Calculating the UN Sub-Region means/average of the Gini index.

avg_gini_sub_region <- function(data) {
  final_data <- data %>%
    mutate(avg_gini = mean(gini, na.rm = TRUE)) %>%
    select(region_un_sub, region_un, avg_gini)
  print(final_data)
}

## Using the 'map' function to calculate the average Gini index across UN Sub-Region.

region_un_sub_tibbles <- wiid_split %>%
  map(avg_gini_sub_region)

## Combining the 'tibbles' (small data frames) into one object, and converting 
## to a data frame, with 'distinct' to ensure a single line per UN Sub-Region.

df_avg_gini_sub_region <- as.data.frame(bind_rows(region_un_sub_tibbles)) %>%
  distinct()

df_avg_gini_sub_region

## Calculating the average Gini index by UN Region.

df_avg_gini_region <- wiid %>%
  group_by(region_un) %>%
  summarize(avg_gini_region = mean(gini, na.rm = TRUE))

df_avg_gini_region

## Calculating the difference between each UN Sub-Region and its appropriate UN Region.

left_join(df_avg_gini_sub_region, df_avg_gini_region, "region_un" = "region_un") %>%
  mutate(gini_difference = avg_gini - avg_gini_region)

## Simplified Final Result

left_join(df_avg_gini_sub_region, df_avg_gini_region, "region_un" = "region_un") %>%
  mutate(gini_difference = avg_gini - avg_gini_region) %>%
  select(region_un_sub, region_un, gini_difference)

