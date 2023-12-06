## Data Visualization (GOVT16-QSS17) Fall 2022
## Polar Coordinates
##
## Professor Robert A. Cooper
## Week 5

library(tidyverse)
library(ggtext) # Using text in plots.
library(gridExtra)
library(cowplot)

## Polar Coordinates

## Polar coordinates allow us to make pie charts, coxcomb/rose plots, and other
## circle-oriented plots. The first concept for polar coordinates is 'theta', which
## represents the variable that will be tied to the angles created at the center
## of the circle.

## For our purposes (in 'ggplot2'), 'theta' will be either 'x' or 'y', which will
## inform the angles from the center.

## Pie Chart - A single stacked bar plot turned polar.

data("mtcars")
glimpse(mtcars)
 
## Bar Chart
mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() 
 
mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  group_by(cyl_fact) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE)) %>%
  ggplot(aes(x = cyl_fact, y = mean_mpg, fill = cyl_fact)) +
  geom_col()

## Bar Chart (Plus Polar Coordinates)
mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  coord_polar(theta = "y")

## The "Florence Nightingale"
## Coxcomb Plot, Polar Area Plot, Rose Plot
mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(gear))) +
  geom_bar(width = 1) + # This guarantees equal size for equal units.
  coord_polar() +
  theme_minimal()

glimpse(mtcars)

## Actual Pie Chart - To create our pie chart, we re-arrange the aesthetics.
## To start, we use X as a constant.

mtcars %>%
  ggplot(aes(x = 0, fill = factor(cyl))) +
  geom_bar()

mtcars %>%
  ggplot(aes(x = 0, fill = factor(cyl))) +
  geom_bar() +
  coord_polar(theta = "y")

## The constant has no real meaning here, it is just meant to be a constant.

mtcars %>%
  ggplot(aes(x = 5, fill = factor(cyl))) +
  geom_bar() +
  coord_polar(theta = "y")
 
## Rotated Pie Chart
## The 'start' argument is based on radians.
mtcars %>%
  ggplot(aes(x = factor(0), fill = factor(cyl))) +
  geom_bar() +
  coord_polar(theta = "y", start =  pi)

## Multiplot Panels
## Let's make a couple of random plots.

data("mtcars")

plot1 <- mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  group_by(cyl_fact) %>%
  summarize(avgmpg = mean(mpg, na.rm = TRUE)) %>%
  ggplot(aes(x = cyl_fact, y = avgmpg, fill = cyl_fact)) +
  geom_bar(stat = "identity") + # Identical to geom_col().
  theme_minimal()

plot1

plot2 <- mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = 1, fill = cyl_fact)) +
  geom_bar() +
  coord_polar(theta = "y") +
  labs(title = "Dude, Check This Pie!") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(size = 7, hjust = 0.5))
  
plot2

grid.arrange(plot1, plot2, ncol = 2)







## That looks odd because of the aspect ratio of the two plots.
## Let's change the widths of the two to see what happens.

# Adding details to our plots. 

data("mtcars")

# Three plots. No detail, some detail, and much detail. 

# No detail. Bare bones, no cleanup. 

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point()

# Some detail. 

(plot1 <- mtcars %>%
    mutate(cyl_fact = factor(cyl)) %>%
    ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point(size = 0.5) +
    labs(x = "Weight (in Thousands of Pounds",
         y = "Fuel Efficiency (Miles Per Gallon",
         color = "Engine \n Cylinders") + 
    theme_minimal() +
    theme(axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          panel.grid.major = element_blank(),
          legend.position = c(.75, .75),
          legend.text = element_text(size = 7),
          legend.title = element_text(size = 7)))

# Much detail. 
# Note: the text looks small, but looks more appropriate in my slides. 
# The compression of the slide deck reduces the plot environment size, but 
# does not reduce the size of the text at the same ratio. 

(plot1 <- mtcars %>%
    mutate(cyl_fact = factor(cyl)) %>%
    ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point(size = 0.5) +
    labs(x = "Weight (in Thousands of Pounds)",
         y = "Fuel Efficiency (Miles Per Gallon)",
         color = "Engine \n Cylinders",
         title = "**Car Weight Impacts Fuel Efficiency**<br>
<span style = 'font-size:5pt;'>Miles per gallon (MPG) is on average higher for cars
with <span style = 'color:#8589F0;'>four cylinders</span> than for cars with
<span style = 'color:blue;'> six cylinders</span> or <span style = 'color:#D55E00;'> eight cylinders</span>. In general, MPG generally
declines with increasing weight of the car.</span>",
         caption = "This is a silly caption made <br> for **teaching purposes** only.") +
    scale_color_manual(values = c("#8589F0", "blue", "red")) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 6, family = "mono", color = "gray20"),
          axis.title.y = element_text(size = 6, family = "mono", color = "gray20"),
          panel.grid.major = element_blank(),
          legend.position = "none",
          legend.title = element_text(size = 6, family = "mono", color = "gray20"),
          legend.text = element_text(size = 6),
          plot.title = element_textbox_simple(size = 7, lineheight = 1, padding = margin(0,0,5,0)),
          plot.caption = element_markdown(size = 4)))










## Pie charts rotated AND labeled by slice. 
## If you want to label your pie charts in ggplot2,
## by proportion/percent, at least, you'll have to quickly 
## mutate a new proportion/percent variable. 
 
pie1 <- mtcars %>%
    ggplot(aes(x = factor(1), fill = factor(cyl))) +
    geom_bar() +
    coord_polar(theta = "y",
                start = pi) 

pie1

mtcars2 <- mtcars %>%
   mutate(cylfact = factor(cyl)) %>%
   group_by(cylfact) %>%
   summarize(nn = n()) %>%
   ungroup() %>%
   mutate(prop = nn/sum(nn)*100) 

mtcars2
 
pie2  <- mtcars2 %>%
   ggplot(aes(x = factor(1), fill = cylfact, y = nn)) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = paste0(round(prop, 2), "%"), x = 1),
                 position = position_stack(vjust = 0.5)) +
   coord_polar(theta = "y", start = 1) +
   theme_void()
 
 ### For two-panel plots. See also the 'cowplot' package
 
 library(gridExtra)

 grid.arrange(pie1, pie2, ncol = 2,
              top = "A Simple Grid Example") 
 
## Rose plot using the 'diamonds' data set. 

data("diamonds")
glimpse(diamonds) 

?diamonds # For further info on data. 

# What do we want here instead? First, we are changing theta to X.
# Second, the center angles are all the same,
# built off different categories of diamond color. 
# We now vary the plot by the length of the "petal". 

# Stacked version of roseplot. 

glimpse(diamonds)

diamonds %>%
   group_by(cut, color) %>%
   summarize(avgcarat = mean(carat, na.rm = TRUE)) %>%
   ggplot(aes(x = color, y = avgcarat, fill = cut)) +
   geom_bar(width = 1, stat = "identity") +
   labs(title = "Color, Cut, and Carat: Diamonds",
        x = "Color (D (Best) to J (Worst)",
        y = " Average Carat",
        fill = "Cut") +
   coord_polar(theta =  "x") +
   theme_minimal()


# Unstacked version of roseplot. With all starting at zero/center. 

diamonds %>%
  group_by(cut, color) %>%
  summarize(avgcarat = mean(carat, na.rm = TRUE)) %>%
  ggplot(aes(x = color, y = avgcarat, fill = cut)) +
  geom_bar(width = 1, stat = "identity", position = "identity", alpha = 0.8) +
  labs(title = "Color, Cut, and Carat: Diamonds",
       x = "Color (D (Best) to J (Worst)",
       y = " Average Carat",
       fill = "Cut") +
  coord_polar(theta =  "x") +
  theme_minimal()


########################

##############################3

# hexbin plots. 
# These are two-dimensional density plots. 
# The bins are not bars or squares, but hexagons. 

# When are they appropriate? Two continuous variables. 

data("diamonds")
glimpse(diamonds)

# A hexbin is a two-dimensional histogram. 
# Hex bins. 

library(hexbin)

diamonds %>%
  ggplot(aes(x = log(carat), y = log(price))) +
  geom_hex(bins = 50)  +
  labs(x = "Carats (Logged)",
       y = "Price in Dollars (Logged)",
       fill = "Count") +
  theme_minimal()
   

## 2D density plot. 
## This works for two continuous variables. 

data("diamonds")
glimpse(diamonds)


diamonds %>%
   ggplot(aes(x = carat, y = price)) +
   geom_density2d() +
   theme_minimal()

# There's actually an underlying stat_... for every geom. 
# It can be useful sometimes. 

diamonds %>%
   ggplot(aes(x = carat, y = price)) +
   stat_density2d(aes(fill = stat(level)), geom = "polygon") +
   theme_minimal()

# Less useful is the 2d_filled, which fills the entire data space. Like a raster. 

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_density2d_filled() +
  xlim(c(0, 1.15)) +
  ylim(c(0, 3500)) +
  theme_minimal()

################################################################################
# Grabbing plot-build data. 

# OECD line plots by year. Note: points underneath smoother would be an improvement.

library(readxl)

wiid <- read_excel(file.choose()) 

wiid_sub <- wiid %>%
  filter(year > 1960 & year < 2002) 

wiid_sub %>%
  distinct(year) %>%
  arrange(year) %>%
  print(n = nrow(.))

wiid_smooth <- wiid_sub %>%
  group_by(oecd, year) %>%
  summarize(med_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(., aes(x = year, y = med_gini)) +
  geom_point(aes(color = oecd), alpha = 0.5) +
  geom_smooth(aes(color = oecd), span = 0.5) +
  ylim(c(30, 60)) +
  labs(x = "Year",
       y = "Median Gini Score",
       title = "OECD vs. Non-OECD Countries and Income Inequality",
       color = "OECD") +
  theme_minimal()

wiid_smooth

# Let's say I want a geom_ribbon, like the W. Playfair plots. 
# Aesthetics: x OR y, then xmin/xmax or ymin/ymax. 

# 1. Start with a basic smoother. 
# 2. Save the build data from ggplot. 
# 3. Get into the list object that is created. 

plot_dat <- ggplot_build(wiid_smooth)
class(plot_dat)  
is.list(plot_dat) # Recall: special objects are sometimes made by packages. 
# That does not mean they do not have basic underlying formats that you know. 
length(plot_dat)

# Work with these data. 

plot_dat

# Investigate: 

plot_dat[[1]]
plot_dat[[1]][[1]] # These happen to be the plot data for the points. 
plot_dat[[1]][[2]] # These happen to be the plot data for the lines. 

# We want the line data, obviously. 

plot_dat2 <- data.frame(plot_dat[[1]][[2]]) # Just the line data. 

class(plot_dat2) # Just to double-check. 
glimpse(plot_dat2)
tail(plot_dat2)

# Let's look at the data underneath the build.
# We will be particularly interested in the 'group' variable.

head(plot_dat2)

plot_dat2 %>%
  count(group) # 80 values per group, it seems. 

plotdat2.2 <- plot_dat2 %>% 
  mutate(newid = rep(1:80, 2)) 

plotdat2.2

# We are going to use the 'newid' for a join!

plot_dat_list <- plotdat2.2 %>% 
  group_split(group) 

plot_dat_list
plot_dat_list[[1]]
plot_dat_list[[2]]

# We will use the 'newid' as our key variable in a join.

plot_join_dat <- full_join(plot_dat_list[[1]][,c("x", "y", "group", "newid")], 
                           plot_dat_list[[2]][,c("x", "y", "group", "newid")], 
                           by = c("newid"))

# One key issue is that occasionally the estimated X might not match up.
# IF the series from 1 group is longer/shorter than the other. 


wiid_smooth_data <- wiid %>%
  filter(year > 1960 & year < 2002) %>%
  group_by(oecd, year) %>%
  summarize(med_gini = median(gini)) 

# Make a ribbon plot. 

plot_join_dat %>%
  ggplot(aes(x = x.x, ymin = y.x, ymax = y.y)) +
  # geom_smooth(data = wiid_smooth_data, 
  #            aes(x = year, y = med_gini, color = oecd), 
  #            span = 0.5, 
  #            inherit.aes = FALSE) +
  #geom_line(aes(y = y.x), color = "red", size = 0.3) +
  #geom_line(aes(y = y.y), color = "red", size = 0.3) +
  geom_ribbon(fill = "#7fc7fd", alpha = 0.4) +
  geom_line(aes(y = y.x), color = "red", size = 0.3) +
  geom_line(aes(y = y.y), color = "red", size = 0.3) +
  # geom_textpath(aes(x = x.x, y = y.y, z = label)) + 
  ylim(c(20,60)) +
  labs(x = "Year",
       y = "Median Gini Scores",
       title = "OECD v. Non-OECD") +
  theme_minimal()

# We could even add text. 

library(geomtextpath)

text <- "Non-OECD Countries"
text2 <- "OECD Countries"

text_df <- data.frame(x = )

plot_join_dat %>%
  ggplot(aes(x = x.x, ymin = y.x, ymax = y.y)) +
  geom_ribbon(fill = "#7fc7fd", alpha = 0.4) +
  # geom_line(aes(y = y.x), color = "red", size = 0.3) +
  # geom_line(aes(y = y.y), color = "red", size = 0.3) +
  geom_textpath(aes(x = x.x, y = y.x), label = text, size = 2, color = "red") + 
  geom_textpath(aes(x = x.x, y = y.y), label = text2, size = 2, color = "red") + 
  ylim(c(20,60)) +
  labs(x = "Year",
       y = "Median Gini Scores",
       title = "OECD v. Non-OECD") +
  theme_minimal()

################################################################################
# Creating new variables for aesthetics. 
# Probably the most common way to make a plot `your own`. 

# Two examples from the assignments so far:

# 1: Labels for the scatterplot/dotplot of European countries flipped.

library(tidyverse)

wiid_labs <- wiid %>%
   filter(region_un == "Europe") %>%
   group_by(country) %>%
   summarize(mean_gini = mean(gini, na.rm = TRUE)) %>%
   mutate(lab_pos = ifelse(country == "Bosnia and Herzegovina", -.1,
                           ifelse(country == "Serbia and Montenegro", -.02,
                                  ifelse(country == "Macedonia, former Yugoslave Republic of", -.02, 0)))) 

wiid_labs %>%
   ggplot(., aes(x = reorder(country, mean_gini, na.rm = TRUE), y = mean_gini)) +
   geom_point() +
   geom_text(aes(label = country, y = mean_gini, hjust = lab_pos), size = 2.5, nudge_y = 0.5) +
   labs(x = "", y = "Average Gini Index Scores", 
        title = "Income Inequality in Europe") +
   coord_flip() +
   ylim(c(20,45)) +
   theme_minimal() +
   theme(axis.text.y = element_blank())



################################################################################
# Using things like inset plots from cowplot. 

library(tidyverse)
library(cowplot)

pacdat <- data.frame(x = c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6),
                     y = c(1, 2, 3, 4, 5, 1, 1, 1, 1, 1))

pacdot_plot <- pacdat %>%
   ggplot() +
   geom_point(aes(x = x, y = y), size = 5, color = "white") +
   expand_limits(x = 0, y = 0) +
   labs(title = "PAC MAN!", color = "white") +
   theme_void() +
   theme(plot.title = element_text(hjust = 0.5,
                                   color = "white",
                                   size = 20),
         plot.background = element_rect(fill = "#365096"))

pacdot_plot

pacman <- data.frame(y = c(rep("Pac", 13), rep("Man", 3)))

pacplot <- pacman %>%
   ggplot() +
   geom_bar(aes(x = 1, fill = y)) +
   coord_polar(theta = "y", 
               start = 1.6*pi) +
   scale_fill_manual(values = c("#365096", "#f6c704")) +
   theme_void() +
   theme(legend.position = "none",
         plot.background = element_rect(fill = "transparent"),
         rect = element_rect(fill = "transparent"),
         panel.border = element_rect(fill = "transparent",
                                     color = "#4360ae"))

pacplot

# Using ggdraw to inset a plot. Super useful. 

ggdraw() +
   draw_plot(pacdot_plot) +
   draw_plot(pacplot, x = .33, y = -.26, scale = 0.25)

