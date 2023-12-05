## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 7) Waffle
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(gganimate)
library(ggwaffle)
library(waffle)

library(readxl)

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")
glimpse(wiid)

canada_data <- wiid %>%
  filter(year == 2010, country == "Canada") %>%
  pivot_longer(names_to = "quintile", values_to = "percent", q1:q5) %>%
  mutate(quintile_ordered = factor(quintile, ordered = TRUE, 
                                   levels = c("q1", "q2", "q3", "q4", "q5"))) %>%
  select(country, quintile_ordered, percent) %>%
  group_by(quintile_ordered) %>%
  summarize(avg_percent = mean(percent, na.rm = TRUE)) %>%
  mutate(avg_percent_rounded = round(avg_percent)) %>%
  uncount(avg_percent_rounded) %>%
  mutate(order = 1:100) %>%
  mutate(x = rep(1:10, each = 10),
         y = rep(1:10, 10))

united_states_data <- wiid %>%
  filter(year == 2010, country == "United States") %>%
  pivot_longer(names_to = "quintile", values_to = "percent", q1:q5) %>%
  mutate(quintile_ordered = factor(quintile, ordered = TRUE, 
                                   levels = c("q1", "q2", "q3", "q4", "q5"))) %>%
  select(country, quintile_ordered, percent) %>%
  group_by(quintile_ordered) %>%
  summarize(avg_percent = mean(percent, na.rm = TRUE)) %>%
  mutate(avg_percent_rounded = round(avg_percent)) %>%
  uncount(avg_percent_rounded) %>%
  mutate(order = 1:100) %>%
  mutate(x = rep(12:21, each = 10),
         y = rep(1:10, 10))

canada_data
united_states_data

final_data <- rbind(canada_data, united_states_data)

final_plot <- final_data %>%
  ggplot(aes(x, y, fill = quintile_ordered)) +
  geom_waffle(size = 1) +
  scale_fill_manual(values = c("#2271C0", "#52B283", "#5FAC3E", "#92D427", "#DFD41A"),
                    labels = c("1st Quintile", "2nd Quintile", "3rd Quintile", "4th Quintile", "5th Quintile")) +
  labs(title = "Income Inequality in 2010: U.S. v. Canada",
       fill = "quintfact") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16)) +
  annotate("text", x = 5.4, y = 11.2, label = "Canada", size = 4.5, hjust = 0.5) +
  annotate("text", x = 16.4, y = 11.2, label = "United States", size = 4.5, hjust = 0.5)

final_plot

final_animation <- final_plot +
  transition_manual(factor(order), cumulative = TRUE)

animate(final_animation, duration = 36, fps = 6, height = 600, width = 1125)
anim_save("waffle.gif")

