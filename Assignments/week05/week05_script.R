## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 5: Intermediate ggplot2 & Customization
##
## Name: Carter Kruse
## Date: October 17th, 2022

## To deal with missing data points, we will remove the 'NA' missing data points
## inside the statistical functions like 'mean', 'median', 'min' and 'max' with the
## 'na.rm = TRUE' argument.

## In this assignment, we will fix all labels (x-axis, y-axis, titles, legend)
## and use hex codes to make colors.

library(tidyverse)
library(devtools)

library(ggtext)
library(ggrepel)
library(gridExtra)

library(HistData)
library(lubridate)
library(cowplot)

## 1) Nightingale Data

## Florence Nightingale made a very valuable contribution to science and data
## visualization.

## We will recreate the famous rose plots (coxcomb plots/polar area plots) that
## Florence Nightingale created when she studied the causes of death for soldiers
## during the Crimean War.

## The data is available through the 'HistData' package, appropriately named 'Nightingale'.

data(Nightingale)
glimpse(Nightingale)

## Investigating the Nightingale data, with deaths and death rates in the data set.

?Nightingale

## Data transformation is necessary to correctly re-create the plot.
## Match the color as close as possible. Use hex codes and the alpha parameter to
## ensure we can see overlapping categories.

## Try to make the labels and captions match. This might require the use of
## 'geom_text' in an interesting way on the bar plot before it becomes a rose plot.

## You do not need to create the dashed line connecting the two plots. Just make
## the two rose plots in side-by-side panels using the 'gridExtra' or 'cowplot'
## package. The 'grid.arrange' function may be useful.

## Right-Hand Chart
## Creating a data frame, with modified labels, bars, and label positions.

nightingale_data_frame_right <- Nightingale %>%
  filter(Date <= "1855-03-01") %>%
  mutate(Label = case_when( # Changing the label names as appropriate.
    Date == "1854-04-01" ~ "APRIL \n 1854",
    Date == "1855-01-01" ~ "JANUARY 1855",
    Date == "1855-03-01" ~ "MARCH 1855",
    TRUE ~ toupper(month(Date, label = TRUE, abbr = FALSE)))) %>%
  select(Date, Label, Disease.rate, Wounds.rate, Other.rate) %>%
  mutate(Disease = sqrt(Disease.rate * 12 / pi), # Changing the values to represent areas.
         Wounds = sqrt(Wounds.rate * 12 / pi),
         Other = sqrt(Other.rate * 12 / pi),
         Angle = 90 - 360 * (c(1:12) - 0.5) / 12) %>%
  mutate(Inner_Bar = Wounds, # Creating new bars for the bar chart.
         Middle_Bar = ifelse(Other > Inner_Bar, Other - Inner_Bar, 0),
         Outer_Bar = ifelse(Disease > (Inner_Bar + Middle_Bar), Disease - (Inner_Bar + Middle_Bar), 0)) %>%
  pivot_longer(Inner_Bar:Outer_Bar, names_to = "Cause", values_to = "Value") %>%
  group_by(Date) %>%
  mutate(Value_Sum = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Position = case_when( # Changing the position of some of the labels.
    Date == "1854-04-01" ~ Value_Sum + 8,
    Date == "1854-05-01" ~ Value_Sum + 12,
    Date == "1854-06-01" ~ Value_Sum + 12,
    TRUE ~ Value_Sum
  ))

nightingale_data_frame_right

## Adding in the dark lines shown in the image.

september_line <- data.frame(x = c(5.5, 6.5), y = c(11, 11))
october_line <- data.frame(x = c(6.5, 7.5), y = c(14, 14))
november_line <- data.frame(x = c(7.5, 8.5), y = c(13, 13))

## Creating the right hand plot, using the data frame.

nightingale_right_plot <- nightingale_data_frame_right %>%
  ggplot(aes(x = fct_inorder(Label), y = Value)) +
  geom_bar(aes(fill = factor(Cause, ordered = TRUE, 
                             levels = c("Outer_Bar", "Middle_Bar", "Inner_Bar"))),
           stat = "identity", width = 1, alpha = 0.7) +
  geom_text(aes(label = Label, y = Position, angle = Angle),
            color = "#000000", family = "serif", size = 2, position = "identity", vjust = -1) +
  geom_line(data = september_line, aes(x = x, y = y), size = 0.6, color = "#332D2E") +
  geom_line(data = october_line, aes(x = x, y = y), size = 0.6, color = "#332D2E") +
  geom_line(data = november_line, aes(x = x, y = y), size = 0.6, color = "#332D2E") +
  coord_polar(start = (9 / 12) * 2 * pi, clip = "off") + # Adjusting Start
  scale_fill_manual(values = c("#AAB7BA", "#332D2E", "#ECB0AB")) +
  labs(subtitle = "1.<br>APRIL 1854 <span style = 'font-size:10pt'> TO </span> MARCH 1855") +
  theme_void() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "#000000", hjust = 0.5, vjust = 0,
                                         lineheight = 1.5, 
                                         family = "serif", size = 10,
                                         margin = margin(20, 0, -90, 0)),
        plot.margin = margin(-100, -50, 100, -50)) +
  annotate("text", x = 6.6, y = 36, label = "CRIMEA", size = 1.5, color = "#000000",
           family = "serif", fontface = "italic") +
  annotate("text", x = 3.3, y = 12.8, label = "BULGARIA", size = 1.5, color = "#000000",
           angle = 90, family = "serif", fontface = "italic")

nightingale_right_plot

## Left-Hand Chart
## Creating a data frame, with modified labels, bars, and label positions.

nightingale_data_frame_left <- Nightingale %>%
  filter(Date > "1855-03-01") %>%
  mutate(Label = case_when( # Changing the label names as appropriate.
    Date == "1856-01-01" ~ "JANUARY \n 1856",
    TRUE ~ toupper(month(Date, label = TRUE, abbr = FALSE)))) %>%
  select(Date, Label, Disease.rate, Wounds.rate, Other.rate) %>%
  mutate(Disease = sqrt(Disease.rate * 12 / pi), # Changing the values to represent areas.
         Wounds = sqrt(Wounds.rate * 12 / pi),
         Other = sqrt(Other.rate * 12 / pi),
         Angle = 90 - 360 * (c(1:12) - 0.5) / 12) %>%
  mutate(Inner_Bar = Other, # Creating new bars for the bar chart.
         Middle_Bar = ifelse(Wounds > Inner_Bar, Wounds - Inner_Bar, 0),
         Outer_Bar = ifelse(Disease > (Inner_Bar + Middle_Bar), Disease - (Inner_Bar + Middle_Bar), 0)) %>%
  pivot_longer(Inner_Bar:Outer_Bar, names_to = "Cause", values_to = "Value") %>%
  group_by(Date) %>%
  mutate(Value_Sum = sum(Value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Position = case_when( # Changing the position of some of the labels.
    Date == "1855-09-01" ~ Value_Sum + 3,
    Date == "1855-10-01" ~ Value_Sum + 7,
    Date == "1855-11-01" ~ Value_Sum + 4,
    Date == "1855-11-01" ~ Value_Sum + 8,
    Date == "1855-12-01" ~ Value_Sum + 9,
    Date == "1856-01-01" ~ Value_Sum + 9,
    Date == "1856-02-01" ~ Value_Sum + 12,
    Date == "1856-03-01" ~ Value_Sum + 12,
    TRUE ~ Value_Sum
  ))

nightingale_data_frame_left

## Creating the left hand plot, using the data frame.

nightingale_left_plot <- nightingale_data_frame_left %>%
  ggplot(aes(x = fct_inorder(Label), y = Value)) +
  geom_bar(aes(fill = factor(Cause, ordered = TRUE, 
                             levels = c("Outer_Bar", "Middle_Bar", "Inner_Bar"))),
           stat = "identity", width = 1, alpha = 0.7) +
  geom_text(aes(label = Label, y = Position, angle = Angle),
            color = "#000000", family = "serif", size = 2, position = "identity", vjust = -1) +
  coord_polar(start = (9 / 12) * 2 * pi, clip = "off") + # Adjusting Start
  scale_fill_manual(values = c("#AAB7BA", "#ECB0AB", "#332D2E")) +
  labs(subtitle = "2.<br>APRIL 1855 <span style = 'font-size:10pt'> TO </span> MARCH 1856") +
  theme_void() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "#000000", hjust = 0.5, vjust = 0,
                                         lineheight = 1.5, 
                                         family = "serif", size = 10,
                                         margin = margin(10, 0, 20, 0)),
        plot.margin = margin(0, 0, -100, 0))

nightingale_left_plot

## Text

words <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "The Areas of the blue, red, & black wedges are each measured from \n  the centre as the common vertex.
       The blue wedges measured from the centre of the circle represent area \n   for area the deaths from Preventable or Mitigable Zymotic diseases, the \n   red wedges measured from the centre the deaths from wounds, & the \n   black wedges measured from the centre the deaths from all other causes.
       The black line across the red triangle in Nov. 1854 marks the boundary \n    of the deaths from all other causes during the month.
       In October 1854, & April 1855, the black area coincides with the red, \n   in January & February 1856, the blue coincides with the black.
       The entire areas may be compared by following the blue, the red, & the \n    black lines enclosing them.") +
  theme_void() +
  theme(plot.title = element_text(size = 12, color = "#000000", family = "Trattatello",
                                  face = "italic"),
        plot.margin = margin(80, 400, 0, 160))

words

left_side <- plot_grid(nightingale_left_plot, words, ncol = 1, rel_heights = c(0.4, 0.6))
left_side

no_title <- plot_grid(left_side, nightingale_right_plot, rel_widths = c(0.4, 0.6))
no_title

title <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "DIAGRAM <span style='font-size:12pt'> OF THE</span> CAUSES <span style='font-size:12pt'>OF</span> MORTALITY",
       subtitle="<span style='font-size:10pt'>IN THE</span> ARMY <span style='font-size:10pt'>IN THE</span> EAST.") + 
  theme_void() +
  theme(plot.title = element_markdown(size = 18, color = "#000000", family = "Trattatello",
                                      hjust = 0.42),
        plot.subtitle = element_markdown(size = 12, color = "#000000", hjust = 0.44, face = "bold"),
        plot.margin = margin(0, 0, 0, 0))

title

final_nightingale_plot <- plot_grid(title, no_title, rel_heights = c(0.1, 1), ncol = 1) +
  theme(plot.margin = margin(0, -100, 0, 0),
        plot.background = element_rect(fill = "#FFF9FD", color = "#FFF9FD"))

final_nightingale_plot

ggsave(filename = "nightingale_plot.png", width = 12, height = 7)

## 2) WIID Data Set

## Installing the package 'readxl' to use the 'read_excel' function.

library(readxl)

## Reading in the WIID data set.

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")
glimpse(wiid)

## Showing the proportion of observations in the data set by UN region ('region_un')
## that are classified as the highest income group in a pie chart.

wiid_high_income_data <- wiid %>%
  filter(incomegroup == "High income") %>%
  mutate(total_count = n()) %>%
  group_by(region_un) %>%
  mutate(value = n()) %>%
  ungroup() %>%
  mutate(percent = 100 * round(value / total_count, 4)) %>%
  select(region_un, percent) %>%
  distinct()

glimpse(wiid_high_income_data)

## Data Transformation - Adding labels to each slice that shows the percent
## each category makes up within the pie chart.

wiid_high_income_data_labels <- wiid_high_income_data %>%
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent / 2, pos)) %>%
  select(region_un, percent, pos)

wiid_high_income_data_labels

## Basic Plot - The theme is 'theme_minimal'. The colors and label for the legend are modified.

wiid_high_income_base_plot <- wiid_high_income_data %>%
  ggplot(aes(x = "" , y = percent, fill = factor(region_un, ordered = TRUE,
                                                 levels = c("Europe", "Oceania", "Americas", "Asia", "Africa")))) +
  geom_col(width = 1) +
  geom_label_repel(data = wiid_high_income_data_labels,
                   aes(y = pos, label = paste0(percent, "%")),
                   size = 2, color = "#000000", nudge_x = 0.8, show.legend = FALSE,
                   alpha = 0.8) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#FFB0AC", "#ACCEE5", "#C4ECC1", "#E2CAE6", "#FFD79F")) +
  labs(title = "Low Income", subtitle = "(Proportion of Observations)",
       x = "", y = "Count", fill = "UN Region") +
  theme_minimal()

wiid_high_income_base_plot

## Final Plot - The axes are removed from the plotting environment.

wiid_high_income_final_plot <- wiid_high_income_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 8, family = "sans"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 7, family = "sans"),
        legend.text = element_text(color = "#000000", size = 6, family = "sans"),
        legend.key.size = unit(3, "mm"))

wiid_high_income_final_plot

## Showing the proportion of observations in the data set by UN region ('region_un')
## that are classified as the lowest income group in a pie chart.

## This pie chart looks remarkable different from the previous

wiid_low_income_data <- wiid %>%
  filter(incomegroup == "Low income") %>%
  mutate(total_count = n()) %>%
  group_by(region_un) %>%
  mutate(value = n()) %>%
  ungroup() %>%
  mutate(percent = 100 * round(value / total_count, 4)) %>%
  select(region_un, percent) %>%
  distinct()

glimpse(wiid_low_income_data)

## Data Transformation - Adding labels to each slice that shows the percent
## each category makes up within the pie chart.

wiid_low_income_data_labels <- wiid_low_income_data %>%
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent / 2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent / 2, pos)) %>%
  select(region_un, percent, pos)

wiid_low_income_data_labels

## Basic Plot - The theme is 'theme_minimal'. The colors and label for the legend are modified.

wiid_low_income_base_plot <- wiid_low_income_data %>%
  ggplot(aes(x = "" , y = percent, fill = factor(region_un, ordered = TRUE,
                                                 levels = c("Asia", "Africa", "Americas")))) +
  geom_col(width = 1) +
  geom_label_repel(data = wiid_low_income_data_labels,
                   aes(y = pos, label = paste0(percent, "%")),
                   size = 2, color = "#000000", nudge_x = 0.8, show.legend = FALSE,
                   alpha = 0.8) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#E2CAE6", "#FFD79F", "#C4ECC1")) +
  labs(title = "Low Income", subtitle = "(Proportion of Observations)",
       x = "", y = "Count", fill = "UN Region") +
  theme_minimal()

wiid_low_income_base_plot

## Final Plot - The axes are removed from the plotting environment.

wiid_low_income_final_plot <- wiid_low_income_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 8, family = "sans"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 7, family = "sans"),
        legend.text = element_text(color = "#000000", size = 6, family = "sans"),
        legend.key.size = unit(3, "mm"))

wiid_low_income_final_plot

## Creating a two-plot panel using 'grid.arrange' from the 'gridExtra' package to
## put the two pie charts together.

## The legend is only kept for the pie chart on the right. A central title at
## the top of the grid is included.

grid.arrange(wiid_low_income_final_plot + theme(legend.position = "none"), 
             wiid_high_income_final_plot, ncol = 2, heights = c(4, 4),
             top = "WIID Data Set - UN Region - Income Level",
             padding = unit(2, "line"))

## 3) Color - Using it to tell a story.

## Creating a subset of European countries called 'nordic' that includes the following
## countries.

nordic <- wiid %>%
  filter(country == "Denmark" | country == "Finland" | country == "Iceland" | country == "Norway" | country == "Sweden")

glimpse(nordic)

## Creating a plot with the average/mean Gini index values over time as a scatter plot,
## adding a 'geom_smooth' and adjusting its sensitivity, reducing the span argument
## to 0.6.

## Using colors and the alpha parameter, Sweden is highlighted, as if we are telling
## a data story about that country. The colors reflect that intent by highlighting
## Sweden relative to the other Nordic countries. The blue from Sweden's flag is used,
## while for the other countries, contrasting colors are used in a way that sets
## them in the background.

nordic_base_plot <- nordic %>%
  group_by(year, country) %>%
  summarize(avg_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_gini, color = country, fill = country)) +
  geom_point(alpha = 0.8) +
  geom_smooth(span = 0.6, size = 0, alpha = 0.1) +
  stat_smooth(geom = "line", span = 0.6, size = 1, aes(alpha = country),
              show.legend = FALSE) +
  scale_color_manual(values = c("#DD6F82", "#81E479", "#D562E9", "#F49749", "#005297")) +
  scale_fill_manual(values = c("#DD6F82", "#81E479", "#D562E9", "#F49749", "#005297")) +
  scale_alpha_manual(values = c(0.7, 0.7, 0.7, 0.7, 1.0)) +
  labs(title = "Gini Index by Year", subtitle = "(Nordic Countries)",
       x = "Year", y = "Gini Index", color = "Country", fill = "Country") +
  theme_minimal()

nordic_base_plot

## Final Plot - Customizing the theme, background, and axes.

nordic_final_plot <- nordic_base_plot +
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
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"),
        legend.position = c(0.8, 0.8))

nordic_final_plot

## 4) GDP & Gini Index

## Do countries with lower GDP per capita have lower Gini Index scores?
## Taking the log of the income-based variable GDP (in U.S. 2011 dollars) before
## plotting it against the reported Gini indices.

## Using 'geom_hex', which is similar to two-dimensional histograms, and thus,
## closely related to two-dimensional density plots.

## The number of bins (50) is set to be much higher than the default.
## The colors are modified (using color hex codes) to a five-color scale to highlight
## the variation.

wiid_gdp_gini_base_plot <- wiid %>%
  ggplot(aes(x = gini, y = log(gdp))) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colors = c("#81E27A", "#3870CC", "#340083", "#7E0070", "#C21313")) +
  labs(title = "log(GDP) vs. Gini Index", subtitle = "(WIID Data Set)",
       x = "Gini Index",  y = "log(GDP)", fill = "Count") +
  theme_minimal()

wiid_gdp_gini_base_plot

## Final Plot - Customizing the theme, background, and colors. The legend is
## moved inside the plotting panel, where is does not block any data.

wiid_gdp_gini_final_plot <- wiid_gdp_gini_base_plot +
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
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"),
        legend.position = c(0.85, 0.9))

wiid_gdp_gini_final_plot

## 5) WIID Data Set - Two-Dimensional Density Plot
## Gini Index Scores & GDP Per Capita

## Start with a basic scatter plot of these points in light gray.

wiid_2d_points_base_plot <- wiid %>%
  ggplot() +
  geom_point(aes(x = gini, y = gdp), color = "#CCCCCC") +
  scale_y_continuous(limits = c(0, 120000)) +
  labs(title = "GDP vs. Gini Index", subtitle = "(WIID Data Set)",
       x = "Gini Index",  y = "GDP") +
  theme_minimal()

wiid_2d_points_base_plot

## Final Plot - Customizing the theme, background, and colors.

wiid_2d_points_final_plot <- wiid_2d_points_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

wiid_2d_points_final_plot

## Creating a 2D density plot, with the levels of the density plot filled in.
## This is "added onto" the 2D scatter plot from the previous part.

## A different color scale (than the default) is used with which to fill in the
## density levels. The area between the density lines is colored.

color_palette <- colorRampPalette(c("#FCFDBF", "#D3436E", "#221150"))

wiid_2d_density_base_plot <- wiid %>%
  ggplot(aes(x = gini, y = gdp)) +
  geom_point(color = "#CCCCCC") +
  geom_density_2d_filled(alpha = 0.7, contour_var = "density",
                         breaks = c(1.0e-7, 2.5e-7, 4.0e-7,
                                    5.5e-7, 7.0e-7, 8.5e-7,
                                    1.0e-6, 2.5e-6, 4.0e-6),
                         show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 120000)) +
  scale_fill_manual(values = color_palette(9)) +
  labs(title = "GDP vs. Gini Index", subtitle = "(WIID Data Set)",
       x = "Gini Index",  y = "GDP") +
  theme_minimal()

wiid_2d_density_base_plot

## Final Plot - Customizing the theme, background, and colors.

wiid_2d_density_final_plot <- wiid_2d_density_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

wiid_2d_density_final_plot

## 6) UN Region - Individual Gini Index

## Every row/observation (raw), including repeats for a given country and year are
## included for comparison to the total distribution of observed Gini scores in
## the data set.

## Faceting a scatter plot of individual Gini index scores by UN Region.

## Removing the years where only a few countries have observations, filtering to
## start around 1940.

wiid_un_region_base_plot_facet <- wiid %>%
  filter(year >= 1940) %>%
  ggplot(aes(x = year, y = gini)) +
  geom_point(size = 0.3, color = "#000000") +
  scale_x_continuous(limits = c(1940, 2020), breaks = c(1960, 1990, 2020)) +
  scale_y_continuous(limits = c(0, 80), breaks = c(20, 40, 60, 80)) +
  labs(title = "Gini Index by Year by UN Region", x = "Year", y = "Gini Index") +
  theme_classic() +
  facet_grid(~ region_un) 

wiid_un_region_base_plot_facet

## Customizing the theme, background, and colors. Using the classic theme, while
## changing all lables to look nice and professional.

wiid_un_region_final_plot_facet <- wiid_un_region_base_plot_facet +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

wiid_un_region_final_plot_facet

## Modification - Add ALL the points to each facet underneath in a light gray color.

## To do so, we create a new data set with all of the points, removing the
## 'region_un' column.

wiid_all_points <- wiid %>%
  filter(year >= 1940) %>%
  select(-region_un)

wiid_all_points

## Creating the comparison plot, reducing the size of the points and adding the
## slightest bit of transparency, alongside adjusting the regional colors so they
## stand out from the gray.

wiid_un_region_final_plot_facet_compare <- wiid_un_region_final_plot_facet +
  geom_point(data = wiid_all_points, color = "#DDDDDD", size = 0.2, alpha = 0.8) +
  geom_point(data = wiid %>% filter(year >= 1940), aes(color = region_un), size = 0.3) +
  scale_color_viridis_d() +
  theme(legend.position = "none")

wiid_un_region_final_plot_facet_compare

## 7) WIID Data Set - Two-Dimensional Density Plot
## Gini Index Scores & GDP Per Capita

# Faceting the data by both income group and UN Region, ensuring that the income
## groups are in the correct order, from high to low.

## Start with a basic scatter plot of these points in light gray.

wiid_2d_points_base_plot_facet <- wiid %>%
  mutate(fct_income_group = factor(incomegroup, ordered = TRUE,
                                   levels = c("High income", "Upper middle income",
                                              "Lower middle income", "Low income"))) %>%
  ggplot() +
  geom_point(aes(x = gini, y = gdp), size = 0.2, color = "#CCCCCC") +
  scale_y_continuous(limits = c(0, 120000)) +
  labs(title = "GDP vs. Gini Index", subtitle = "(WIID Data Set)",
       x = "Gini Index",  y = "GDP") +
  theme_minimal() +
  facet_grid(region_un ~ fct_income_group)

wiid_2d_points_base_plot_facet

## Final Plot - Customizing the theme, background, and colors.

wiid_2d_points_final_plot_facet <- wiid_2d_points_base_plot_facet +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

wiid_2d_points_final_plot_facet

## Creating a 2D density plot, with the levels of the density plot filled in.
## This is "added onto" the 2D scatter plot from the previous part.

## A different color scale (than the default) is used with which to fill in the
## density levels. The area between the density lines is colored.

color_palette <- colorRampPalette(c("#FCFDBF", "#D3436E", "#221150"))

wiid_2d_density_base_plot_facet <- wiid %>%
  mutate(fct_income_group = factor(incomegroup, ordered = TRUE,
                                   levels = c("High income", "Upper middle income",
                                              "Lower middle income", "Low income"))) %>%
  ggplot(aes(x = gini, y = gdp)) +
  geom_point(color = "#CCCCCC", size = 0.2) +
  geom_density_2d_filled(alpha = 0.7, contour_var = "density", 
                         breaks = c(1.0e-7, 4.0e-7, 7.0e-7,
                                    1.0e-6, 4.0e-6, 7.0e-6,
                                    1.0e-5, 4.0e-5, 7.0e-5),
                         show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 120000)) +
  scale_fill_manual(values = color_palette(9)) +
  labs(title = "GDP vs. Gini Index", subtitle = "(WIID Data Set)",
       x = "Gini Index",  y = "GDP") +
  theme_minimal() +
  facet_grid(region_un ~ fct_income_group)

wiid_2d_density_base_plot_facet

## Final Plot - Customizing the theme, background, and colors.

wiid_2d_density_final_plot_facet <- wiid_2d_density_base_plot_facet +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

wiid_2d_density_final_plot_facet

