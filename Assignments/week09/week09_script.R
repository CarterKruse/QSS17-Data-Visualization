## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 9: Data Scraping in R
##
## Name: Carter Kruse
## Date: November 16th, 2022

## This script uses the 'rvest' package, including its functions 'read_html',
## 'html_nodes', and 'html_table'. A similar HTML parsing function may be used.

library(tidyverse)
library(devtools)
library(rvest)

## 1) Ivy League - Wikipedia Page

## Link: https://en.wikipedia.org/wiki/Ivy_League

## Scraping the table of data which includes the name of the school, its
## location, its nickname/mascot, and its undergraduate enrollment.

ivy_html <- read_html("https://en.wikipedia.org/wiki/Ivy_League")

ivy_tables <- ivy_html %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE)

class(ivy_tables)

ivy_data_frame <- data.frame(ivy_tables[2])
glimpse(ivy_data_frame)

## Formatting the data frame.

ivy_data_frame$Undergraduates[[2]] <- str_sub(ivy_data_frame$Undergraduates[[2]], 1, -4)
ivy_data_frame$Location[[5]] <- str_sub(ivy_data_frame$Location[[5]], 1, -4)

ivy_data_frame$Academic.staff[[1]] <- str_sub(ivy_data_frame$Academic.staff[[1]], 1, -5)
ivy_data_frame$Academic.staff[[2]] <- str_sub(ivy_data_frame$Academic.staff[[2]], 1, -5)
ivy_data_frame$Academic.staff[[5]] <- str_sub(ivy_data_frame$Academic.staff[[5]], 1, -5)
ivy_data_frame$Academic.staff[[6]] <- str_sub(ivy_data_frame$Academic.staff[[6]], 1, -5)

glimpse(ivy_data_frame)

ivy_data_frame$Endowment <- str_sub(ivy_data_frame$Endowment, 1, -5)

glimpse(ivy_data_frame)

ivy_data_frame$Undergraduates <- str_remove(ivy_data_frame$Undergraduates, ",")
ivy_data_frame$Postgraduates <- str_remove(ivy_data_frame$Postgraduates, ",")
ivy_data_frame$Academic.staff <- str_remove(ivy_data_frame$Academic.staff, ",")

glimpse(ivy_data_frame)

ivy_data_frame$Undergraduates <- as.numeric(ivy_data_frame$Undergraduates)
ivy_data_frame$Postgraduates <- as.numeric(ivy_data_frame$Postgraduates)
ivy_data_frame$Academic.staff <- as.numeric(ivy_data_frame$Academic.staff)

glimpse(ivy_data_frame)

ivy_data_frame <- ivy_data_frame %>%
  mutate(Staff = Academic.staff) %>%
  select(Institution, Location, Undergraduates, Postgraduates, Endowment, Staff, Nickname)

glimpse(ivy_data_frame)

## Creating a bar plot of the undergraduate enrollment of each school, with the
## color of each bar matching the school's colors.

ivy_league_base_plot <- ivy_data_frame %>%
  ggplot() +
  geom_col(aes(x = Institution, y = Undergraduates, fill = Institution)) +
  scale_fill_manual(values = c("#4E3629", "#99CAEA", "#B31B1B", "#01693E",
                               "#A31F36", "#FF6000", "#011F5B", "#0A2240")) +
  labs(title = "Ivy League Undergraduate Enrollment", x = "Institution",
       y = "Undergraduates", fill = "Institution") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ivy_league_base_plot

## Customizing the base plot with a particular theme.

ivy_league_final_plot <- ivy_league_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        #
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title.y = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"),
        #
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

ivy_league_final_plot

## 2) Global Fortune 500 - Wikipedia Page

## Link: https://en.wikipedia.org/wiki/Fortune_Global_500

## Scraping the table of data which includes the rank, name of company, country,
## industry, and revenue (in USD).

fortune_html <- read_html("https://en.wikipedia.org/wiki/Fortune_Global_500")

fortune_tables <- fortune_html %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE)

class(fortune_tables)

fortune_data_frame <- data.frame(fortune_tables[1])
glimpse(fortune_data_frame)

## Formatting the data frame.

fortune_data_frame$Revenue.in.USD <- str_sub(fortune_data_frame$Revenue.in.USD, 2, -9)

glimpse(fortune_data_frame)

fortune_data_frame <- fortune_data_frame %>%
  mutate(Revenue = Revenue.in.USD) %>%
  select(Rank, Company, Country, Industry, Revenue)

glimpse(fortune_data_frame)

## Creating a bar plot of the revenue of each company, highlighting the petroleum
## companies with a similar color (red) to differentiate from the other type of
## companies on the list.

fortune_base_plot <- fortune_data_frame %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(Company, Rank), y = Revenue, fill = Industry)) +
  scale_fill_manual(values = c(rep("#C0C0C0", 5), "#FF0000", rep("#C0C0C0", 2))) +
  labs(title = "Fortune Global 500 (2022)", x = "Company", y = "Revenue (Billion $USD)", fill = "Industry") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))

fortune_base_plot

## Customizing the base plot with a particular theme.

fortune_final_plot <- fortune_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
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

fortune_final_plot

## 3) U.S. States By Carbon Emissions - Wikipedia Page

## Link: https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_carbon_dioxide_emissions

## Scraping the table of data which includes the rank, jurisdiction, CO2 emissions,
## population, alongside other variables.

emissions_html <- read_html("https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_carbon_dioxide_emissions")

emissions_tables <- emissions_html %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE)

class(emissions_tables)

emissions_data_frame <- data.frame(emissions_tables[2])
glimpse(emissions_data_frame)

## Formatting the data frame.

emissions_data_frame <- emissions_data_frame %>%
  filter(Jurisdiction != "States, D.C. and territories total" &
           Jurisdiction != "States and D.C. Total" &
           Jurisdiction != "Puerto Rico" &
           Jurisdiction != "U.S. Virgin Islands" &
           Jurisdiction != "Guam" &
           Jurisdiction != "American Samoa" &
           Jurisdiction != "Northern Mariana Islands") %>%
  mutate(State = Jurisdiction, Emissions = Annual.CO2.emissions..per.capita.in.metric.tons.) %>%
  select(State, Emissions)

glimpse(emissions_data_frame)

emissions_data_frame$Emissions <- as.numeric(emissions_data_frame$Emissions)
emissions_data_frame$State <- tolower(emissions_data_frame$State)

glimpse(emissions_data_frame)

emissions_data_frame$Emissions <- case_when(
  emissions_data_frame$Emissions > 50 ~ "50+",
  emissions_data_frame$Emissions > 30 & emissions_data_frame$Emissions <= 50 ~ "30-50",
  emissions_data_frame$Emissions > 25 & emissions_data_frame$Emissions <= 30 ~ "25-30",
  emissions_data_frame$Emissions > 20 & emissions_data_frame$Emissions <= 25 ~ "20-25",
  emissions_data_frame$Emissions > 15 & emissions_data_frame$Emissions <= 20 ~ "15-20",
  emissions_data_frame$Emissions > 10 & emissions_data_frame$Emissions <= 15 ~ "10-15",
  emissions_data_frame$Emissions > 5 & emissions_data_frame$Emissions <= 10 ~ "5-10",
  TRUE ~ "0-5"
)

glimpse(emissions_data_frame)

## Recreating the map in the top right of the Wikipedia page, with Hawaii and Alaska.

library(sf)
library(USAboundaries)
library(USAboundariesData)

state_data <- us_states()
class(state_data)
glimpse(state_data)

## Formatting the data table by removing Puerto Rico and modifying column titles.

state_data <- state_data %>%
  filter(name != "Puerto Rico") %>%
  mutate(State = tolower(name)) %>%
  select(State, geometry)

glimpse(state_data)

## Creating the 'mapping_data' by joining the 'state_data' and 'emissions_data_frame'.

mapping_data <- left_join(state_data, emissions_data_frame, by = "State") %>%
  mutate(Emissions = factor(Emissions, levels = c("50+", "30-50", "25-30", "20-25", 
                                                  "15-20", "10-15", "5-10", "0-5"),
                            ordered = TRUE))
glimpse(mapping_data)

## Creating a base plot of the emissions data for each state.

emissions_base_plot <- mapping_data %>%
  ggplot() +
  geom_sf(aes(fill = Emissions), size = 0.2, color = "#000000") +
  coord_sf(xlim = c(-125, -60), ylim = c(20, 52)) +
  scale_fill_manual(values = c("#300000", "#660000", "#9A0100", "#CD0000",
                               "#FF0000", "#FF6666", "#FFCDCD", "#FFFFFF")) +
  labs(title = "CO2 Emission per Capita per Year per State (2011)",
       x = "Longitude", y = "Latitude", fill = "Millions of metric tons") +
  theme_minimal()

emissions_base_plot

## Customizing the base plot with a particular theme.

emissions_final_plot <- emissions_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        #
        title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        #
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 5, family = "sans"),
        legend.text = element_text(color = "#000000", size = 5, family = "sans"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key.size = unit(2, "mm"),
        legend.position = c(0.85, 0.40)) +
  guides(fill = guide_legend(title.position = "bottom"))

emissions_final_plot

## Creating the visualizations for Alaska and Hawaii.

alaska <- mapping_data %>%
  ggplot() +
  geom_sf(aes(fill = Emissions), size = 0.2, color = "#000000") +
  coord_sf(xlim = c(-180, -130), ylim = c(50, 72)) +
  scale_fill_manual(values = c("#300000", "#660000", "#9A0100", "#CD0000",
                               "#FF0000", "#FF6666", "#FFCDCD", "#FFFFFF")) +
  theme_void() +
  theme(legend.position = "none")
  
alaska

hawaii <- mapping_data %>%
  ggplot() +
  geom_sf(aes(fill = Emissions), size = 0.2, color = "#000000") +
  coord_sf(xlim = c(-161, -154), ylim = c(18, 23)) +
  scale_fill_manual(values = c("#300000", "#660000", "#9A0100", "#CD0000",
                               "#FF0000", "#FF6666", "#FFCDCD", "#FFFFFF")) +
  theme_void() +
  theme(legend.position = "none")

hawaii

## Using 'cowplot' and 'ggdraw' to inset the separate maps for Alaska and Hawaii
## into the main plot.

library(cowplot)
library(ggtext)

emissions_plot_complete <- ggdraw() +
  draw_plot(emissions_final_plot) +
  draw_plot(alaska, x = -0.35, y = -0.25, scale = 0.25) +
  draw_plot(hawaii, x = -0.20, y = -0.27, scale = 0.15) +
  draw_label("CO2 emissions per capita per state \n in 2011", 
             x = 0.8, y = 0.3, color = "#000000", 
             size = 5, angle = 0)

emissions_plot_complete

## 4) NCAA Women's Basketball (Division I)

## Link: https://www.ncaa.com/stats/basketball-women/d1

## Scraping the 'Won-Lost Percentage' data for all teams.

win_loss_table <- data.frame(read_html(str_c("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p", "1")) %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE))

glimpse(win_loss_table)

for (p in 2:7) {
  win_loss_table <- rbind(win_loss_table, data.frame(read_html(str_c("https://www.ncaa.com/stats/basketball-women/d1/current/team/169/p", p)) %>%
                                                     html_nodes(css = "table") %>%
                                                     html_table(fill = TRUE)))
}

glimpse(win_loss_table)

## Scraping the 'Scorign Offense' data for all teams.

scoring_offense_table <- data.frame(read_html(str_c("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p", "1")) %>%
                               html_nodes(css = "table") %>%
                               html_table(fill = TRUE))

glimpse(scoring_offense_table)

for (p in 2:7) {
  scoring_offense_table <- rbind(scoring_offense_table, data.frame(read_html(str_c("https://www.ncaa.com/stats/basketball-women/d1/current/team/111/p", p)) %>%
                                                       html_nodes(css = "table") %>%
                                                       html_table(fill = TRUE)))
}

glimpse(scoring_offense_table)

## Joining the data frames into a single data frame for plotting.

ncaa_plotting_data <- inner_join(win_loss_table, scoring_offense_table, by = "Team")
glimpse(ncaa_plotting_data)

## Creating a scatter plot with 'Scoring Offense' (PPG, points per game) on the
## x-axis and 'Won-Lost Percentage' (Pct) on the y-axis.

## A 'smooth' line is included to demonstrate the trend.

ncaa_base <- ncaa_plotting_data %>%
  ggplot(aes(x = PPG, y = Pct)) +
  geom_point(aes(color = PPG)) +
  geom_smooth(color = "#A00000", alpha = 0.2, span = 0.6) +
  scale_color_distiller(palette = "YlOrRd", limits = c(35, 110)) +
  scale_x_continuous(limits = c(45, 110), breaks = c(40, 50, 60, 70, 80, 90, 100, 110)) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  labs(title = "NCAA Women's Basketball (2022)", subtitle = "Winning Percentage (PCT) vs. Points Per Game (PPG)",
       x = "PPG", y = "PCT", color = "PPG") +
  theme_minimal()

ncaa_base

## Modifying colors, titles, labels, and legend.

ncaa_final <- ncaa_base +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        #
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"),
        #
        legend.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key.size = unit(5, "mm"),
        legend.position = c(0.9, 0.3))

ncaa_final
