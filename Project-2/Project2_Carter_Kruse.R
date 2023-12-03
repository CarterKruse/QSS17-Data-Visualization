## Data Visualization (GOVT16-QSS17) Fall 2022
## Project 2 - COVID Data
## NY Times & University Of Oxford
##
## Name: Carter Kruse
## Date: October 31st, 2022

## Data
## NY Times: https://github.com/nytimes/covid-19-data
## University Of Oxford: https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker

## Packages

library(tidyverse)
library(devtools)

library(lubridate)
library(gganimate)
library(magick)
library(ggrepel)
library(ggtext)

library(sf)
library(USAboundaries)

## Using the 'read_csv' function to import the data.

covid_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
containment_health_index <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/containment_health_index_avg.csv")
economic_support_index <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/economic_support_index.csv")
stringency_index <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/stringency_index_avg.csv")
government_response_index <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/government_response_index_avg.csv")

## Printing the 'typeof' and 'class' of the data frames.

typeof(covid_data)
typeof(containment_health_index)
typeof(economic_support_index)
typeof(stringency_index)
typeof(government_response_index)

class(covid_data)
class(containment_health_index)
class(economic_support_index)
class(stringency_index)
class(government_response_index)

## Using the 'glimpse' function to quickly view the data frames.

glimpse(covid_data)
glimpse(containment_health_index)
glimpse(economic_support_index)
glimpse(stringency_index)
glimpse(government_response_index)

## Identifying the states included in the COVID data set using the 'distinct' function.

covid_data %>%
  distinct(state)

## Creating a new data frame that analyzes the data, appropriate for the visualization.
## Using 'filter' to limit the states included and time frame ('date').
## Using 'select' to simplify the data frame.
## Using 'group_by' to calculate statistics over groups.
## Using 'mutate' to create new data frame variables.
## Using 'arrange' to organize the data frame.
## Using 'case_when' to handle if/else statements.

covid_data_map <- covid_data %>%
  filter(date >= "2020-03-15" & date <= "2022-10-31",
         state == "Maine" | state == "New Hampshire" | state == "Vermont" |
         state == "Massachusetts" | state == "Rhode Island" | state == "Connecticut" |
         state == "New York" | state == "Pennsylvania" | state == "New Jersey") %>%
  select(date, state, cases) %>%
  group_by(state) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  arrange(state, date) %>%
  group_by(state, year, month) %>%
  mutate(max_cases = max(cases)) %>% # Calculating the max cases per month.
  ungroup() %>%
  mutate(new_cases = max_cases - lag(max_cases)) %>% # Calculating the new cases per month.
  group_by(state, year, month) %>%
  mutate(new_cases = max(new_cases),
         new_cases_scaled = case_when(
           new_cases == 0 ~ 0,
           TRUE ~ sqrt(sqrt(new_cases)))) %>% # Implementing a scale factor for appropriate colors.
  ungroup() %>%
  select(date, state, new_cases_scaled)

## Using sub-setting and 'is.na' to replace NA values with 0.

covid_data_map[is.na(covid_data_map)] <- 0

## Using the 'glimpse' function to quickly view the data frame and the 'str' function
## to view the structure.

glimpse(covid_data_map)
str(covid_data_map)

## Using the 'min' and 'max' functions by column to check scaling.

min(covid_data_map$new_cases_scaled)
max(covid_data_map$new_cases_scaled)

## USAboundaries - Importing the 'us_states' data frame and viewing it.

state_data <- us_states()
glimpse(state_data)

## Modifying the data, using 'mutate' to rename a variable and 'select' to simplify
## the data frame.

state_data_map <- state_data %>%
  mutate(state = name) %>%
  select(state, geometry)

glimpse(state_data_map)

## Using the 'right_join' function to join the data sets by 'state'.

map_data <- right_join(state_data_map, covid_data_map, by = "state")
glimpse(map_data)

## Viewing the 'head'/'tail' of the data frame.

head(map_data)
tail(map_data)

## Using '?' to look up the documentation for 'scale_fill_distiller'.

?scale_fill_distiller

## Animated Plot 1 - This plot demonstrates the number of new COVID cases (scaled)
## per month, for the U.S. (specifically the Northeast). The animation occurs over
## a time frame from "2020-03-15" to "2022-10-31".

## To plot the map data, 'geom_sf', along with 'coord_sf' is used.

plot_1_base <- map_data %>%
  arrange(date) %>%
  ggplot() +
  geom_sf(aes(fill = new_cases_scaled), size = 0.2, color = "#000000") +
  coord_sf(xlim = c(-82, -67), ylim = c(38, 48)) +
  scale_fill_gradientn(colors = c(colorRampPalette(c("#0D1C93", "#4DE374"))(9))) +
  labs(title = "New COVID Cases in the Northeast",
       subtitle = "Date: {frame_time}",
       x = "Longitude", y = "Latitude", fill = "New Cases (Scaled)") +
  theme_classic() +
  transition_time(date)

# plot_1_base

# Customizing the theme of the plot.

plot_1_final <- plot_1_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

# plot_1_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.

animation_1 <- animate(plot_1_final, width = 600, height = 400, duration = 20, fps = 15)
# animation_1

## Creating/modifying the various data sets, which contain time series data of
## policy indices.

## The function 'pivot_longer' is used to transform the data from 'wide' to 'long' form.

## Documentation: https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker

containment_health_data <- containment_health_index %>%
  filter(country_name == "United States",
         jurisdiction == "NAT_TOTAL") %>%
  pivot_longer(names_to = "date", values_to = "containment_health", 
               !(country_code | country_name | region_code | region_name | jurisdiction)) %>%
  select(date, containment_health)

glimpse(containment_health_data)

economic_support_data <- economic_support_index %>%
  filter(country_name == "United States",
         jurisdiction == "NAT_TOTAL") %>%
  pivot_longer(names_to = "date", values_to = "economic_support", 
               !(country_code | country_name | region_code | region_name | jurisdiction)) %>%
  select(date, economic_support)

glimpse(economic_support_data)

stringency_data <- stringency_index %>%
  filter(country_name == "United States",
         jurisdiction == "NAT_TOTAL") %>%
  pivot_longer(names_to = "date", values_to = "stringency", 
               !(country_code | country_name | region_code | region_name | jurisdiction)) %>%
  select(date, stringency)

glimpse(stringency_data)

government_response_data <- government_response_index %>%
  filter(country_name == "United States",
         jurisdiction == "NAT_TOTAL") %>%
  pivot_longer(names_to = "date", values_to = "government_response", 
               !(country_code | country_name | region_code | region_name | jurisdiction)) %>%
  select(date, government_response)

glimpse(government_response_data)

## Joining the data into a full data frame, which contains all policy indices.

all_data <- full_join(containment_health_data, 
          full_join(economic_support_data, 
                    full_join(stringency_data, government_response_data)))

glimpse(all_data)

## Removing the first row of the data, which does not contain relevant information.

all_data <- all_data[-1,]
glimpse(all_data)

## Creating a new data frame that uses appropriate dates.
## Using 'mutate' to change the date from a '<chr>' to a '<Date>' and 'filter'
## to limit the time frame ('date').

index_data <- all_data %>%
  mutate(date = dmy(date)) %>%
  filter(date >= "2020-03-15" & date <= "2022-10-31") %>%
  pivot_longer(names_to = "policy", values_to = "index_value", 
               containment_health:government_response)

glimpse(index_data)

## Animated Plot 2 - This plot demonstrates the change in policy with respect to
## COVID (as indicated by the various indices), over time, across the U.S. The
## animation occurs over a time frame from "2020-03-15" to "2022-10-31".

## Using 'scale_y_continuous' to scale the axis and 'scale_color_manual' to 
## scale the colors. Including labels for x, y, and the title.

## A vertical (dotted) line is used to emphasize the change in time.

plot_2_base <- index_data %>%
  ggplot() +
  geom_line(aes(x = date, y = index_value, color = policy), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("#0D1C93", "#0071D7", "#00B0BA", "#4DE374"),
                     labels = c("Containment/Health", "Economic Support",
                                "Government Response", "Stringency")) +
  geom_vline(aes(xintercept = date), linetype = "dashed", color = "#CCCCCC", size = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Policy Indices", 
       subtitle = "Value' is a measure of how many of the relevant indicators a government\nhas acted upon, and to what degree, with 0 indicating no action\nand 100 indicating significant action.",
       x = "Date", y = "Value", color = "Policy Index") +
  theme_classic() +
  transition_reveal(date)

# plot_2_base

# Customizing the theme of the plot.

plot_2_final <- plot_2_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"))

# plot_2_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.

animation_2 <- animate(plot_2_final, width = 600, height = 400, duration = 20, fps = 15)
# animation_2

## Creating a new data frame that analyzes the data, appropriate for the visualization.
## Using 'filter' to limit the states included and time frame ('date').
## Using 'mutate' to create a factor variable of the state.
## Using 'select' to simplify the data frame.

covid_data_cases <- covid_data %>%
  filter(date >= "2020-03-15" & date <= "2022-10-31",
         state == "Maine" | state == "New Hampshire" | state == "Vermont" |
         state == "Massachusetts" | state == "Rhode Island" | state == "Connecticut" |
         state == "New York" | state == "Pennsylvania" | state == "New Jersey") %>%
  mutate(state = factor(state)) %>%
  select(date, state, cases)

glimpse(covid_data_cases)

## Animated Plot 3 - This plot demonstrates the cumulative COVID cases by state
## over time, across the U.S. (specifically the Northeast). The animation occurs
## over a time frame from "2020-03-15" to "2022-10-31".

## Using faceting ('facet_wrap') to break up the line charts into different plots.
## Using 'colorRampPalette' to create a custom discrete color palette.
## Using 'fct_reorder' to determine the colors based on the cases (low to high).
## Using 'view_follow' to allow for adjustment of the y-axis during animation.

plot_3_base <- covid_data_cases %>%
  ggplot() +
  geom_line(aes(x = date, y = cases, color = fct_reorder(state, cases)), size = 1) +
  scale_color_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(9)) +
  labs(title = "Cumulative COVID Cases in the Northeast", x = "Date", y = "Cases") +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ state, nrow = 2) +
  transition_reveal(date) +
  view_follow(fixed_x = TRUE)

# plot_3_base

# Customizing the theme of the plot.

plot_3_final <- plot_3_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title.y = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        #
        legend.position = "none")

# plot_3_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.

animation_3 <- animate(plot_3_final, width = 800, height = 400, duration = 20, fps = 15)
# animation_3

## Creating a new data frame that analyzes the data, appropriate for the visualization.
## Using 'filter' to limit the states included and time frame ('date').
## Using 'select' and 'distinct' to simplify the data frame.
## Using 'mutate' to create a variable of the total number of COVID cases.

## Using 'group_by' to calculate statistics (percent) over groups.
## Using 'ungroup' to remove the grouping.
## Using 'arrange' to organize the data frame (percent, low to high).
## Using 'select' to simplify the data frame.

covid_data_sum <- covid_data %>%
  filter(date == "2022-10-31",
         state == "Maine" | state == "New Hampshire" | state == "Vermont" |
         state == "Massachusetts" | state == "Rhode Island" | state == "Connecticut" |
         state == "New York" | state == "Pennsylvania" | state == "New Jersey") %>%
  select(state, cases) %>%
  distinct() %>%
  mutate(total_count = sum(cases)) %>%
  group_by(state) %>%
  mutate(percent = 100 * round(cases / total_count, 4)) %>%
  ungroup() %>%
  arrange(percent) %>%
  select(state, percent)

covid_data_sum

## Creating a new data frame that will hold the percentages and positions for 
## the labels that show the percent each category makes up within the pie chart.

covid_data_sum_labels <- covid_data_sum %>%
  mutate(csum = rev(cumsum(rev(percent))),
         pos = percent / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent / 2, pos))

covid_data_sum_labels

## Static Plot 4 - This plot demonstrates the percentage of total cases (in the
## Northeast of the U.S.) that each state makes up. This is a static pie chart.

## Using 'geom_col' to set up the initial bar plot.
## Using 'geom_label_repel' to create the labels.
## Using 'coord_polar' to construct the pie chart.
## Using 'scale_fill_manual' with 'colorRampPalette' to set a custom discrete color palette.

plot_4_base <- covid_data_sum_labels %>%
  ggplot() +
  geom_col(aes(x = 0 , y = percent, fill = fct_inorder(state)), width = 1) +
  geom_label_repel(aes(x = 0, y = pos, label = paste0(percent, "%")),
                   size = 3, color = "#000000", nudge_x = 0.8, show.legend = FALSE,
                   alpha = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(9)) +
  labs(title = "Percent of Cumulative COVID Cases", subtitle = "(In the Northeast, By State)",
       x = "", y = "", fill = "State") +
  theme_void()

# plot_4_base

# Customizing the theme of the plot.

plot_4_final <- plot_4_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        #
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

# plot_4_final

## Using 'ggsave' to save the plot, with the appropriate dimensions.

ggsave("static_4.png", plot = plot_4_final,  device = "png", width = 1600, height = 1600, units = "px")

## Magick

## Reading in the animations, along with the static image.

anim_1 <- image_read(animation_1)
anim_2 <- image_read(animation_2)
anim_3 <- image_read(animation_3)
static_4 <- image_read("static_4.png")

## Viewing, cropping, and scaling/re-sizing the static image.

static_4
static_4 <- image_scale(image_crop(static_4, "0x1400+0+100"), "400x400")
static_4

## Checking to make sure the length of each animation is the same.

length(anim_1)
length(anim_2)
length(anim_3)

## Using 'image_append' to construct the top part of the GIF.

partial_gif_1 <- image_append(c(anim_1[1], anim_2[1]))

for (i in 2:300) {
  partial_gif_1 <- c(partial_gif_1, image_append(c(anim_1[i], anim_2[i])))
}

# partial_gif_1

## Using 'image_append' to construct the bottom part of the GIF.

partial_gif_2 <- image_append(c(anim_3[1], static_4))

for (i in 2:300) {
  partial_gif_2 <- c(partial_gif_2, image_append(c(anim_3[i], static_4)))
}

# partial_gif_2

## Using 'image_append' with 'stack = TRUE' to construct the final GIF.

final_gif <- image_append(c(partial_gif_1[1], partial_gif_2[1]), stack = TRUE)

for (i in 2:300) {
  final_gif <- c(final_gif, image_append(c(partial_gif_1[i], partial_gif_2[i]), stack = TRUE))
}

# final_gif

## Using 'image_write' to create the visualization, saving it.

image_write(final_gif, "COVID_Data.gif")

