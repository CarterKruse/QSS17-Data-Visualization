## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 7: Animated Plots & GIFs
##
## Name: Carter Kruse
## Date: October 31st, 2022

library(tidyverse)
library(devtools)

library(magick)
library(gifski)

## 1) Making a GIF using the 'magick' package, using two GIFs from Giphy and
## splicing them together into one GIF.

## Guidance: It must incorporate a Halloween theme in some way. Keep it mostly 
## classy. No overtly partisan politics.

## Using the 'magick' package to load in the GIFs/images.

img_fairy_dog <- "https://media.giphy.com/media/3ohhwFapJTVNPKITWU/giphy.gif"
img_pirate_cat <- "https://media.giphy.com/media/LUIvcbR6yytz2/giphy.gif"
img_versus <- "https://static.wikia.nocookie.net/roosterteeth/images/f/ff/VS_logo.png"

dog <- image_read(img_fairy_dog); dog
cat <- image_read(img_pirate_cat); cat
versus <- image_read(img_versus); versus

## Cropping and re-sizing the GIFs/images.

dog_scaled <- image_crop(image_scale(dog, "250"), "250x250"); dog_scaled
cat_scaled <- image_crop(cat, "250x250+40+0"); cat_scaled
versus_scaled <- image_scale(versus, "x40"); versus_scaled

## Creating the GIF, slowing down the first part.

halloween_gif <- c(dog_scaled[rep(1:30, each = 2)], cat_scaled)
halloween_gif

## Inserting multiple frames that contains a picture that does not exist in the
## sampled GIFs. Think of it almost like a subliminal message.

halloween_gif[40:80] <- image_composite(halloween_gif[40:80], versus_scaled, offset = "+200+200")
halloween_gif

## Saving the GIF, to be submitted with the R script.

image_write_gif(halloween_gif, "/Users/carterkruse/Data Viz/week07/halloween.gif")

## Sources:
## https://media.giphy.com/media/3ohhwFapJTVNPKITWU/giphy.gif
## https://media.giphy.com/media/LUIvcbR6yytz2/giphy.gif

## 2) WIID Data Set - Gini Index (Western Europe vs. U.S.)

library(gganimate)
library(readxl)

## Creating an animated plot (line and scatter plot) using 'gganimate' of
## the average Gini index scores for Western European countries, along with the
## United States over time.

## Opening the WIDD data set.

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")
glimpse(wiid)

## Filtering the data set by country and year (past 2000).

wiid_data_frame <- wiid %>%
  filter(year >= 2000 & (region_un_sub == "Western Europe" | country == "United States")) %>%
  group_by(country, year) %>%
  summarize(avg_gini = mean(gini, na.rm = TRUE))

glimpse(wiid_data_frame)

## Basic Plot - The colors of the lines are set so that the United States is
## highlighted compared to Western European countries.

wiid_base_plot <- wiid_data_frame %>%
  ggplot(aes(x = year, y = avg_gini, color = country)) +
  geom_point(size = 2) +
  geom_path() +
  scale_x_continuous(limits = c(2000, 2020), breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(limits = c(24, 46), breaks = c(25, 30, 35, 40, 45)) +
  scale_color_manual(values = c("#151A9D", "#3135AD", "#4C50BE", "#686CCE",
                                "#8487DE", "#9FA2EF", "#BBBDFF", "#9E1515")) +
  labs(title = "Gini Index Scores by Year by Country",
       subtitle = "(Western Europe & U.S.)",
       x = "Year", y = "Gini Index", color = "Country") +
  theme_minimal()

wiid_base_plot

## Customizing the theme, including the titles, labels, and legend.

wiid_final_plot <- wiid_base_plot +
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
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key.size = unit(5, "mm"))

wiid_final_plot

## The point and line/path geoms are animated as they move across years.

wiid_animation <- wiid_final_plot +
  transition_reveal(year) +
  shadow_trail(distance = 0.0475) # Preserves the prior 'points'.

animate(wiid_animation, duration = 10, fps = 20)
# anim_save("wiid_animation.gif")

## To see what the 'shadow_wake' does, we create a new plot with no line/path.

## Basic Plot - The colors of the points are set so that the United States is
## highlighted compared to Western European countries.

wiid_base_plot_shadow <- wiid_data_frame %>%
  ggplot(aes(x = year, y = avg_gini, color = country)) +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(2000, 2020), breaks = c(2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous(limits = c(24, 46), breaks = c(25, 30, 35, 40, 45)) +
  scale_color_manual(values = c("#151A9D", "#3135AD", "#4C50BE", "#686CCE",
                                "#8487DE", "#9FA2EF", "#BBBDFF", "#9E1515")) +
  labs(title = "Gini Index Scores by Year by Country",
       subtitle = "(Western Europe & U.S.)",
       x = "Year", y = "Gini Index", color = "Country") +
  theme_minimal()

wiid_base_plot_shadow

## Customizing the theme, including the titles, labels, and legend.

wiid_final_plot_shadow <- wiid_base_plot_shadow +
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
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key.size = unit(5, "mm"))

wiid_final_plot_shadow

## The point and line/path geoms are animated as they move across years.
## Adding a light gray 'shadow wake' of fairly short length.

wiid_animation_shadow <- wiid_final_plot_shadow +
  transition_reveal(year) +
  shadow_trail(distance = 0.0475) + # Preserves the prior 'points'.
  shadow_wake(wake_length = 0.2, colour = "#CCCCCC")

animate(wiid_animation_shadow, duration = 10, fps = 20)
# anim_save("wiid_animation_shadow.gif")

## 3) WIID Animated Bar Chart

## Create an animated bar plot that ranks the top 10 (highest) GDP/Gini Index
## countries from 1990 to the most recent year of the data.

## Thus, only 10 countries should show at any one time, but they will not always
## be the same 10 countries. Countries should animate in and out of the top 10.

## If done correctly, you should be able to see the bars actually slide up and down.
## The bar plot should have flipped coordinates, such that the longest bar/highest
## value should always be on top.

## Data Transformation
## Calculating the average GDP / Gini Index value for each country, each year.

wiid_base_data <- wiid %>%
  filter(year > 1990, year < 2020) %>%
  select(country, year, gini, gdp) %>%
  group_by(country, year) %>%
  mutate(avg_gini = mean(gini, na.rm = TRUE),
         avg_gdp = mean(gdp, na.rm = TRUE)) %>%
  select(country, year, avg_gini, avg_gdp) %>%
  distinct() %>%
  ungroup() %>%
  mutate(gdp_gini = avg_gdp / avg_gini, na.rm = TRUE) %>%
  select(country, year, gdp_gini)

glimpse(wiid_base_data)

## Creating a ranking of the countries for each year and filtering for the top 10.

ranked_wiid_data <- wiid_base_data %>%
  group_by(year) %>%
  arrange(year, -gdp_gini) %>%
  mutate(rank = as.double(1:n()),
         year = factor(year)) %>%
  filter(rank <= 10)

glimpse(ranked_wiid_data)

## Basic Plot - A qualitative color palette (viridis, discrete) is used.

wiid_base_bar_plot <- ranked_wiid_data %>%
  ggplot() +
  geom_col(aes(x = rank, y = gdp_gini, fill = country, group = country), 
           width = 0.8, alpha = 0.8) +
  geom_text(aes(x = rank, y = 0, label = country, group = country),
            hjust = 1.25, size = 3, color = "#000000", family = "sans") +
  scale_fill_viridis_d() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(-1000, 3800), breaks = c(0, 1000, 2000, 3000)) +
  coord_flip(clip = "off") +
  labs(title = "GDP / Gini (\"Racing\" Bar Chart)", 
       subtitle = "{closest_state}", x = "Rank", y = "GDP / Gini", color = "Country") +
  theme_minimal() +
  theme(legend.position = "none")

wiid_base_bar_plot

## Customizing the theme, including the titles, labels, and legend.

wiid_final_bar_plot <- wiid_base_bar_plot +
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

wiid_final_bar_plot

## The bar chart is animated across the years.

wiid_bar_animation <- wiid_final_bar_plot +
  transition_states(year, transition_length = 2, state_length = 1) +
  enter_fly(x_loc = -20, y_loc = 2000) +
  exit_fly(x_loc = -20, y_loc = 2000)

animate(wiid_bar_animation, duration = 40, fps = 30)
# anim_save("wiid_bar_animation.gif")

## 4) Senate Ideology

## VoteView (https://www.voteview.com) houses data sets of U.S. House and Senate
## votes. From those votes, and those votes only, Howard Rosenthal and Keith Poole
## created an estimate of ideology for members of Congress. The 'NOMINATE'
## scores are in two dimensions (like x and y on a coordinate plane).

## VoteView's 'About' Page
## Ideological positions are calculated using the DW-NOMINATE (Dynamic Weighted
## NOMINAL Three-Step Estimation). This procedure was developed by Poole and Rosenthal
## in the 1980s and is a 'scaling procedure', representing legislators on a 
## spatial map. In this sense, a spatial map is much like a road map. The closeness
## of two legislators on the map shows how similar their voting records are.
## Using this measure of distance, DW-NOMINATE is able to recover the 'dimensions'
## that inform congressional voting behavior.

## The primary dimension through most of American history has been 'liberal' vs.
## 'conservative' (also referred to as 'left' vs. 'right'). A second dimension
## picks up differences within the major political parties over slavery, currency,
## nativism, civil rights, and lifestyle issues during periods of American history.

## VoteView.com - Senate Member Ideology File
## We have been living in an era of high and seemingly increasing ideological
## polarization for a generation or more. This animated plot should show a strong
## shift since the 1940s, with politcal parties that used to overlap somewhat but
## now cluster in tight ideological circles.

## Reading in the csv file.

congress_data_frame <- read.csv("/Users/carterkruse/Data Viz/congress.csv")
glimpse(congress_data_frame)

## Creating an animated scatter plot of ideology (1st and 2nd dimension DW-NOMINATE
## score) for Senators from each state from the beginning of the FDR Administration
## to the most recent full Congress.

## Basic Plot - Selecting Congress #73 (FDR) and beyond. The colors chosen are
## theoretically appropriate (for Republicans and Democrats) and visually appealing.

## Labels are appropriate, with a partly animating subtitle.

congress_base_plot <- congress_data_frame %>%
  filter(congress >= 73) %>%
  ggplot() +
  geom_point(aes(x = nominate_dim1, y = nominate_dim2, color = factor(party_code))) +
  scale_color_manual(values = c("#0000FF", "#01FF00", "#FF0000", "#8800FF", "#FF9900", "#DE00FF"),
                     labels = c("Democratic Party", "Conservative Party", "Republican Party",
                                "Independent", "Progressive Party", "Farmer-Labor Party")) +
  labs(title = "Senate Member Ideology (FDR Administration - Present)", subtitle = "Congress {closest_state}",
       x = "Dimension 1", y = "Dimension 2", color = "Political Party") +
  theme_minimal()

congress_base_plot

## Customizing the theme, including the titles, labels, and legend.

congress_final_plot <- congress_base_plot +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        #
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text = element_blank(),
        #
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key.size = unit(5, "mm"))

congress_final_plot

## Think carefully about what the transition should be. In the political system
## of the United States, each Congress lasts two years. Thus, a 'year' variable
## and a 'Congress' variable are simply two ways to measure time.

## Creating the animation, across time, with a slow transition.

congress_animation <- congress_final_plot +
  transition_states(congress, transition_length = 1, state_length = 10)

## The duration of the animation is increased to 90 seconds, as this allows
## each state to be 2-3 seconds long.

animate(congress_animation, duration = 90)
# anim_save("congress_animation.gif")

## NEW PLOT - Replacing the points with party labels. While Democrats and Republicans
## dominate the two-party system during this period, many other 'third' parties
## exist at least briefly during this time.

## Creating a label of letter(s) for each party represented over the period, for
## example, 'D' for Democrats and 'R' for Republicans.

## See the following link for a guide to all the parties coded in the data from
## the beginning of the U.S: https://voteview.com/articles/data_help_parties.

## Basic Plot - Selecting Congress #73 (FDR) and beyond. The colors chosen are
## theoretically appropriate (for Republicans and Democrats) and visually appealing.

## Labels are appropriate, with a partly animating subtitle.

congress_data_frame 

congress_base_plot_letters <- congress_data_frame %>%
  filter(congress >= 73) %>%
  mutate(party = case_when(
    party_code == 100 ~ "D", # Democratic
    party_code == 112 ~ "C", # Conservative
    party_code == 200 ~ "R", # Republican
    party_code == 328 ~ "I", # Independent
    party_code == 370 ~ "P", # Progressive
    party_code == 537 ~ "F" # Farmer-Labor
  )) %>%
  ggplot() +
  geom_text(aes(label = factor(party), x = nominate_dim1, y = nominate_dim2, color = factor(party_code)),
            size = 3, show.legend = FALSE) +
  geom_point(aes(x = nominate_dim1, y = nominate_dim2, color = factor(party_code)), size = 0, stroke = 0) +
  scale_color_manual(values = c("#0000FF", "#01FF00", "#FF0000", "#8800FF", "#FF9900", "#DE00FF"),
                     labels = c("Democratic Party", "Conservative Party", "Republican Party",
                                "Independent", "Progressive Party", "Farmer-Labor Party")) +
  labs(title = "Senate Member Ideology (FDR Administration - Present)", subtitle = "Congress {closest_state}",
       x = "Dimension 1", y = "Dimension 2", color = "Political Party") +
  theme_minimal()

congress_base_plot_letters

## Customizing the theme, including the titles, labels, and legend.

congress_final_plot_letters <- congress_base_plot_letters +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        #
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text = element_blank(),
        #
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key.size = unit(5, "mm")) +
  guides(color = guide_legend(override.aes = list(size = 3, shape = c(utf8ToInt("D"), utf8ToInt("C"), utf8ToInt("R"),
                                                                      utf8ToInt("I"), utf8ToInt("P"), utf8ToInt("F")))))

congress_final_plot_letters

## Think carefully about what the transition should be. In the political system
## of the United States, each Congress lasts two years. Thus, a 'year' variable
## and a 'Congress' variable are simply two ways to measure time.

## Creating the animation, across time, with a slow transition.

congress_animation_letters <- congress_final_plot_letters +
  transition_states(congress, transition_length = 1, state_length = 10)

## The duration of the animation is increased to 90 seconds, as this allows
## each state to be 2-3 seconds long.

animate(congress_animation_letters, duration = 90)
# anim_save("congress_animation.gif")

