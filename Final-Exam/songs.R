## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 4) Songs
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(dplyr)
library(lubridate)

billboard <- read_csv("/Users/carterkruse/Data Viz Final/data/billboard_dat/hot_stuff.csv")
glimpse(billboard)

spotify <- read_csv("/Users/carterkruse/Data Viz Final/data/billboard_dat/hot100_audio_features.csv")
glimpse(spotify)

data_1 <- billboard %>%
  select(WeekID, SongID, `Weeks on Chart`)

data_2 <- spotify %>%
  select(SongID, danceability) %>%
  drop_na()

full_df <- left_join(data_1, data_2, by = "SongID") %>%
  drop_na()

complete_df <- full_df %>%
  mutate(Danceability = case_when(
    danceability < 0.2 ~ "Level 1",
    danceability < 0.4 & danceability >= 0.2 ~ "Level 2",
    danceability < 0.6 & danceability >= 0.4 ~ "Level 3",
    danceability < 0.8 & danceability >= 0.6 ~ "Level 4",
    danceability >= 0.8 ~ "Level 5",
    TRUE ~ "")) %>%
  select(WeekID, SongID, `Weeks on Chart`, Danceability) %>%
  distinct(WeekID, SongID, `Weeks on Chart`, Danceability)

complete_df

plot_df <- complete_df %>%
  group_by(WeekID, Danceability) %>%
  mutate(Average_Weeks_On_Chart = mean(`Weeks on Chart`, na.rm = TRUE)) %>%
  distinct(WeekID, Danceability, Average_Weeks_On_Chart)

plot_df

intermediate_plot_df <- plot_df %>%
  mutate(Year = year(mdy(WeekID)), Month = month(mdy(WeekID))) %>%
  select(WeekID, Year, Month, Average_Weeks_On_Chart, Danceability)

intermediate_plot_df

final_plot_df <- intermediate_plot_df %>%
  group_by(Year, Month, Danceability) %>% # No WeekID
  mutate(Average_Weeks = mean(Average_Weeks_On_Chart, na.rm = TRUE)) %>%
  distinct(Year, Month, Danceability, Average_Weeks)

final_plot_df

base_plot <- final_plot_df %>%
  ggplot(aes(x = Year, y = Average_Weeks, color = Danceability)) +
  geom_point(size = 0.5) +
  geom_smooth(size = 0.7, span = 6, alpha = 0.4) +
  scale_color_manual(values = c("#5936FE", "#1FA500", "#CCC307", "#ED9107", "#D73A14"),
                     labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"),
                     guide = guide_legend(reverse = TRUE)) +
  ylim(c(0, 35))

base_plot

theme_plot <- base_plot +
  labs(title = "Slow Songs No More! Billboard Top 100: 1958-2019",
       x = "Year", y = "Average Weeks on Billboard 100",
       color = "Danceability") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(color = "#F1F1F1"),
        title = element_text(color = "#000000", size = 7.5),
        axis.title = element_text(color = "#000000", size = 6.5),
        axis.text.x = element_text(size = 5, vjust = 1),
        axis.text.y = element_text(size = 5, hjust = 1),
        legend.title = element_text(color = "#000000", size = 7),
        legend.text = element_text(color = "#000000", size = 5),
        legend.key.height = unit(3.5, "mm"),
        legend.key.width = unit(5, "mm"))
    
theme_plot

ggsave(filename = "songs.png", plot = theme_plot, width = 8, height = 5, scale = 1, units = "in")
ggsave(filename = "songs.pdf", plot = theme_plot, width = 8, height = 5, scale = 1, units = "in")

