## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 9: Data Scraping in R - Bonus
##
## Name: Carter Kruse
## Date: November 16th, 2022

## This script uses the 'rvest' package, including its functions 'read_html',
## 'html_nodes', and 'html_table'. A similar HTML parsing function may be used.

library(tidyverse)
library(devtools)
library(rvest)
library(gganimate)
library(magick)

## 5) NCAA Women's Basketball (Division I) - Bonus

## Link: https://www.ncaa.com/stats/basketball-women/d1

## Scraping the 'Won-Lost Percentage' data for 2015.

win_loss_table <- data.frame(read_html(str_c(str_c("https://www.ncaa.com/stats/basketball-women/d1/", "2015"), "/team/169")) %>%
                               html_nodes(css = "table") %>%
                               html_table(fill = TRUE))

## Adding a 'Year' column to the data frame.

win_loss_table$Year <- c(rep(2015, 50))

## Adding the 'Logo' column to the data frame.

logos <- read_html(str_c(str_c("https://www.ncaa.com/stats/basketball-women/d1/", "2015"), "/team/169")) %>%
  html_nodes(css = "#block-bespin-content img") %>%
  html_attr("src")

win_loss_table$Logo <- logos

## Viewing the data set.

glimpse(win_loss_table)
win_loss_table

## Using a for loop to quickly scrape the 'Won-Lost Percentage' data for 2016-2019.

for (p in 2016:2019) {
  partial_win_loss_table <- data.frame(read_html(str_c(str_c("https://www.ncaa.com/stats/basketball-women/d1/", p), "/team/169")) %>%
                                         html_nodes(css = "table") %>%
                                         html_table(fill = TRUE))
  
  logos <- read_html(str_c(str_c("https://www.ncaa.com/stats/basketball-women/d1/", p), "/team/169")) %>%
    html_nodes(css = "#block-bespin-content img") %>%
    html_attr("src")
  
  partial_win_loss_table$Year <- c(rep(p, 50)) # Adding a 'Year' column to the data frame.
  partial_win_loss_table$Logo <- logos # Adding the 'Logo' column to the data frame.
  
  win_loss_table <- rbind(win_loss_table, partial_win_loss_table) # Binding the data frames by row.
}

## Viewing the data set.

glimpse(win_loss_table)
win_loss_table

## Creating an animated bar plot that ranks the top 10 teams in terms of win
## percentage over the years 2015-2019.

## If done correctly, you should be able to see the bars actually slide up and down.
## The bar plot should have flipped coordinates, such that the longest bar/highest
## value should always be on top.

## Simplifying the data set so we may calculate the ranking ourselves.

win_loss_data <- win_loss_table %>%
  select(Team, Year, Pct, Logo)

glimpse(win_loss_data)

## Creating a ranking of the teams for each year and filtering for the top 10.

ranked_win_loss_data <- win_loss_data %>%
  group_by(Year) %>%
  arrange(Year, -Pct) %>%
  mutate(Rank = as.double(1:n()),
         Year = factor(Year)) %>%
  filter(Rank <= 10)

glimpse(ranked_win_loss_data)

## Basic Plot - A qualitative color palette (viridis, discrete) is used.

ranked_win_loss_base_bar_plot <- ranked_win_loss_data %>%
  ggplot() +
  geom_col(aes(x = Rank, y = Pct, fill = Team, group = Team), 
           width = 0.8, alpha = 0.8) +
  geom_text(aes(x = Rank, y = -4, label = Team, group = Team),
            hjust = 1, size = 4, color = "#000000", family = "sans") +
  scale_fill_viridis_d() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(-25, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  coord_flip(clip = "off") +
  labs(title = "NCAA Women's Basketball - Top 10 Teams", 
       subtitle = "{closest_state}", x = "Rank", y = "Pct", color = "Team") +
  theme_minimal() +
  theme(legend.position = "none")

ranked_win_loss_base_bar_plot

## Customizing the theme, including the titles, labels, and legend.

ranked_win_loss_final_bar_plot <- ranked_win_loss_base_bar_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        #
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title.x = element_text(color = "#000000", size = 9, family = "sans"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_blank(),
        #
        axis.line.x = element_line(color = "#000000", size = 0.5),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(color = "#000000", size = 0.5),
        axis.ticks.y = element_blank())

ranked_win_loss_final_bar_plot

## The bar chart is animated across the years.

ncaa_bar_animation <- ranked_win_loss_final_bar_plot +
  transition_states(Year, transition_length = 2, state_length = 1) +
  enter_fly(x_loc = -20, y_loc = 80) +
  exit_fly(x_loc = -20, y_loc = 80)

## Displaying and saving the animation.

animate(ncaa_bar_animation, duration = 15, fps = 30)
anim_save("ncaa_bar_animation.gif")

