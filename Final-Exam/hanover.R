## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 5) Hanover
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(lubridate)
library(dplyr)

library(gridExtra)

hanover_noaa_data <- read_csv("/Users/carterkruse/Data Viz Final/data/hanover_weather/hanover_noaa_data.csv")
glimpse(hanover_noaa_data)

data_1 <- hanover_noaa_data %>%
  select(DATE, TOBS) %>%
  mutate(month_day = paste0("2000", format(as.Date(DATE), "%m%d"))) %>%
  group_by(month_day) %>%
  mutate(avg_daily_observed = mean(TOBS, na.rm = TRUE)) %>%
  distinct(avg_daily_observed) %>%
  mutate(cyclic_date = ymd(month_day))

data_1

data_2 <- hanover_noaa_data %>%
  select(DATE, TMAX, TMIN) %>%
  mutate(month_day = paste0("2000", format(as.Date(DATE), "%m%d"))) %>%
  group_by(month_day) %>%
  mutate(period_day_high = max(TMAX, na.rm = TRUE)) %>%
  mutate(period_day_low = min(TMIN, na.rm = TRUE)) %>%
  distinct(period_day_high, period_day_low) %>%
  mutate(cyclic_date = ymd(month_day))

data_2

base_plot <- ggplot() +
  geom_segment(data = data_2, aes(x = cyclic_date, y = period_day_low, 
                                  xend = cyclic_date, yend = period_day_high), 
               color = "#740B00", size = 1.1) +
  geom_line(data = data_1, aes(x = cyclic_date, y = avg_daily_observed),
            color = "#D5A932", size = 0.9)

base_plot

legend_max_min <- data.frame(period_day_high = 15, period_day_low = -15, cyclic_date = ymd("2000-07-15"))
legend_max_min

legend_avg <- data.frame(period_day_high = 4, period_day_low = -4, cyclic_date = ymd("2000-07-15"))
legend_avg

legend_lines <- data.frame(yloc = c(15, 0, -15),
                           cyclic_date_start = c(ymd("2000-07-17"), ymd("2000-07-19"), ymd("2000-07-17")),
                           cyclic_date_end = c(ymd("2000-07-25"), ymd("2000-07-25"), ymd("2000-07-25")))
legend_lines

base_legend_plot <- base_plot +
  geom_segment(data = legend_max_min, aes(x = cyclic_date, y = period_day_low, 
                                 xend = cyclic_date, yend = period_day_high), 
               color = "#740B00", size = 0.95) +
  geom_segment(data = legend_avg, aes(x = cyclic_date, y = period_day_low, 
                                  xend = cyclic_date, yend = period_day_high), 
               color = "#D5A932", size = 2) +
  geom_segment(data = legend_lines, aes(x = cyclic_date_start, y = yloc, 
                                       xend = cyclic_date_end, yend = yloc), 
               color = "#CCCCCC", size = 0.7)

base_legend_plot

theme_plot <- base_legend_plot +
  scale_y_continuous(breaks = c(-40, -20, 0, 20, 40, 60, 80, 100, 120),
                     limits = c(-45, 125)) +
  scale_x_date(limits = c(ymd("2000-01-01", ymd("2000-12-31"))),
               date_breaks = "1 month", 
               date_labels = c(" ", # Spacing used for centering on plot.
                               "      January", 
                               "     February", 
                               "        March", 
                               "        April",
                               "          May",
                               "         June", 
                               "         July", 
                               "       August", 
                               "    September", 
                               "      October",
                               "     November", 
                               "     December", " ")) +
  labs(title = "Daily Temperatures Since 2000 for Hanover, New Hampshire",
       y = "Temperature in Fahrenheit") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major.x = element_line(color = "#F1F1F1"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        title = element_text(color = "#000000", size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "#000000", size = 7),
        axis.text.x = element_text(size = 5, hjust = 0),
        axis.text.y = element_text(size = 5, vjust = 0))

theme_plot

annotated_plot <- theme_plot +
  annotate("rect", xmin = ymd("2000-07-10"), xmax = ymd("2000-09-22"), ymin = -18, ymax = 18, 
           fill = "#FFFFFF", color = "#CCCCCC", alpha = 0) +
  annotate("text", x = ymd("2000-07-29"), y = 15, label = "Period Day High", size = 2, hjust = 0) +
  annotate("text", x = ymd("2000-07-29"), y = 0, label = "Average Daily Observed", size = 2, hjust = 0) +
  annotate("text", x = ymd("2000-07-29"), y = -15, label = "Period Day Low", size = 2, hjust = 0)

annotated_plot

data_3 <- hanover_noaa_data %>%
  select(DATE, PRCP) %>%
  mutate(year = year(ymd(DATE)), month = month(ymd(DATE)), day = day(ymd(DATE)),
         month_day = paste0("2000", format(as.Date(DATE), "%m%d"))) %>%
  group_by(year, month) %>%
  mutate(cumulative_precipitation = cumsum(PRCP)) %>%
  ungroup() %>%
  group_by(month_day) %>%
  mutate(avg_cumulative_precipitation = mean(cumulative_precipitation, na.rm = TRUE)) %>%
  distinct(avg_cumulative_precipitation) %>%
  mutate(cyclic_date = ymd(month_day))

data_3

base_plot_prcp <- ggplot() +
  geom_line(data = data_3, aes(x = cyclic_date, y = avg_cumulative_precipitation),
            color = "#000000", size = 0.4)

base_plot_prcp

theme_plot_prcp <- base_plot_prcp +
  scale_y_continuous(breaks = c(0, 2, 4),
                     limits = c(0, 5)) +
  scale_x_date(limits = c(ymd("2000-01-01", ymd("2000-12-31"))),
               date_breaks = "1 month", 
               date_labels = c(" ", # Spacing used for centering on plot.
                               "      January", 
                               "     February", 
                               "        March", 
                               "        April",
                               "          May",
                               "         June", 
                               "         July", 
                               "       August", 
                               "    September", 
                               "      October",
                               "     November", 
                               "     December", " ")) +
  labs(title = "Average Cumulative Precipitation by Month for Hanover, New Hampshire",
       y = "Inches") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major.x = element_line(color = "#F1F1F1"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        title = element_text(color = "#000000", size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = "#000000", size = 7),
        axis.text.x = element_text(size = 5, hjust = 0),
        axis.text.y = element_text(size = 5, vjust = 0))

theme_plot_prcp

grid.arrange(annotated_plot, theme_plot_prcp, ncol = 1, heights = c(0.8, 0.2))
final_plot <- arrangeGrob(annotated_plot, theme_plot_prcp, ncol = 1, heights = c(0.8, 0.2))

ggsave(filename = "hanover.png", plot = final_plot, width = 8, height = 5, scale = 1, units = "in")
ggsave(filename = "hanover.pdf", plot = final_plot, width = 8, height = 5, scale = 1, units = "in")

