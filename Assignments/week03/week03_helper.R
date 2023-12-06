## Data Visualization (GOVT16-QSS17) Fall 2022
## Color Theory
##
## Professor Robert A. Cooper
## Week 3

library(tidyverse)
library(colorspace) # Conversion of RGB to hex code.
library(colorBlindness) # Color-blind testing code.
library(gridExtra) # Multiple plots on one visualization.
library(readxl) # Reading Excel files.

## Themes (ggplot2)
## The base 'theme' of ggplot2 is a gray background, with variable-based labels.
## Themes allow you to control many details about the plotting environment,
## specifically elements NOT related to data.

## Themes: https://ggplot2.tidyverse.org/reference/theme.html

data("mtcars")
glimpse(mtcars)

## Basic plot using the 'minimal' theme.
mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 0.5) +
  labs(title = "Car Fuel Efficiency by Weight", color = "Cylinders") +
  theme_minimal()

## Creating a theme that does not necessarily look great.
theme_frankenstein <- theme(axis.title = element_text(color = "red"),
                            plot.title = element_text(family = "Courier"),
                            axis.text.x = element_text(color = "blue"),
                            axis.text.y = element_text(color = "orange"),
                            axis.ticks.length = unit(3, "mm"),
                            panel.background = element_rect(fill = "white"),
                            panel.grid.major = element_line(size = 0.5, color = "gray95"),
                            panel.grid.minor = element_line(size = 0.3, color = "gray95"),
                            legend.text = element_text(size = 6),
                            legend.key = element_rect(fill = "white", size = 0.3),
                            legend.title = element_text(size = 6))

## Displaying the effects of the modified theme.
mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 0.5) +
  labs(title = "Car Fuel Efficiency by Weight", color = "Cylinders") +
  theme_frankenstein

## Data Transformation - Wide To Long

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")

## World Income Inequality Database
## https://www.wider.unu.edu/project/wiid-world-income-inequality-database
wiid

## The 'gather' function turns data to long form, with each column representing
## a variable, though it is deprecated. The 'spread' function is the opposite.

## Certain types of visualizations and/or statistical treatments require long form,
## while others require wide form. Long form, conceptually, is 'tidy', where each
## row is an observation, and each column is a variable.

head(wiid)
glimpse(wiid)
dim(wiid)

## Demonstration of the 'gather' function.
wiid %>%
  gather(key = "quintile", value = "percent", q1:q5) %>%
  select(country, year, gini, quintile, percent, everything()) %>%
  arrange(country, year)

## The new function to use is 'pivot_longer', which combines columns into one
## grouping variable and one value variable. The 'pivot_wider' function is the opposite.

wiid2 <- wiid %>%
  pivot_longer(names_to = "quintile", values_to = "percent", q1:q5) %>%
  select(country, year, gini, quintile, percent, everything()) %>%
  arrange(country, year)

glimpse(wiid2)

## To check the dimensions of our data, we may use the 'select' function.
wiid2 %>%
  select(country, year, quintile, percent, everything())

## Now, let's say we are in the reverse situation.
wiid2 %>%
  pivot_wider(names_from = "quintile", values_from = "percent") %>%
  select(starts_with("q"), everything())

## Demonstrating the effect using the 'diamonds' database.

data('diamonds')
head(diamonds)
glimpse(diamonds)
?diamonds

diamonds %>%
  pivot_longer(values_to = "value", names_to = "measure", x:z) %>%
  filter(value < 15) %>%
  ggplot(aes(x = value, y = price, color = measure)) +
  geom_point(size = 0.2, alpha = 0.5)

## The Anscombe Quartet

data(anscombe); anscombe

g1 <- ggplot(anscombe, aes(x = x1, y = y1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "X1", y = "Y1") +
  expand_limits(y = 0, x = 4) +
  theme_minimal()

g2 <- ggplot(anscombe, aes(x = x2, y = y2)) +
  geom_point(aes(x = x2, y = y2)) +
  geom_smooth(method = "lm") +
  labs(x = "X2", y = "Y2") +
  expand_limits(y = 0, x = 4) +
  theme_minimal()

g3 <- ggplot(anscombe, aes(x = x3, y = y3)) +
  geom_point(aes(x = x3, y = y3)) +
  geom_smooth(method = "lm") +
  labs(x = "X3", y = "Y3") +
  expand_limits(y = 0, x = 4) +
  theme_minimal()

g4 <- ggplot(anscombe, aes(x = x4, y = y4)) +
  geom_point(aes(x = x4, y = y4)) +
  geom_smooth(method = "lm") +
  labs(x = "X4", y = "Y4") +
  expand_limits(y = 0, x = 4) +
  theme_minimal()

## Take all four plots and place them in one visualization. 
grid.arrange(g1, g2, g3, g4,  nrow = 2, top = "The Anscombe Quartet")

## To confirm, run the separate linear models. 
lm1 <- lm(y1 ~ x1, data = anscombe); summary(lm1)
lm2 <- lm(y2 ~ x2, data = anscombe); summary(lm2)
lm3 <- lm(y3 ~ x3, data = anscombe); summary(lm3)
lm4 <- lm(y4 ~ x4, data = anscombe); summary(lm4)

## Various Geoms

## Loading in the IMBD movies dataset.
movies <- read_csv("/Users/carterkruse/Data Viz/movie_metadata.csv")
glimpse(movies)

## Purpose: Introduce a wide variety of geoms, including bar plots, box plots,
## dot plots, violin plots, histograms, density plots, 2d density plots, 2d histograms,
## hexbin plots.

## There are other plots (not shown), including pie charts, ternary plots, and bag plots.
## Pie charts are only good when you have a few categories and large differences
## between them, as the human eye is not great at distinguishing between similar
## slices/areas.

## Bar Plot - Fundamentally Sound, Hard To Beat
## Q: When is a bar plot good?
## A: When displaying a variable by categories.

## Either as a count, or some statistic/value in data by categories. In ggplot2,
## the latter becomes a geom 'column'.

## Bar Plot (w/ Counts)
diamonds %>%
  ggplot(aes(x = cut, fill = cut)) +
  geom_bar()

## Bar Plot (w/ Data Values/Statistics) - Two versions, with identical outputs.
diamonds %>%
  group_by(cut) %>%
  summarize(avg_carat = mean(carat, na.rm = TRUE)) %>%
  ggplot(aes(x = cut, y = avg_carat, fill = cut)) +
  geom_bar(stat = "identity")

diamonds %>%
  group_by(cut) %>%
  summarize(avg_carat = mean(carat, na.rm = TRUE)) %>%
  ggplot(aes(x = cut, y = avg_carat, fill = cut)) +
  geom_col()

## Box Plot - For continuous variables by categories.

diamonds %>%
  ggplot(aes(x = cut, y = carat)) +
  geom_boxplot()

## Dot Plots

head(diamonds)

diamonds %>%
  group_by(cut, color) %>%
  summarize(mean_carat = mean(carat, na.rm = TRUE)) %>%
  ggplot(aes(x = cut, y = mean_carat, fill = color)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.02)

## Violin Plots - Symmetrical Density Curves

diamonds %>%
  ggplot(aes(x = cut, y = carat, fill = cut)) +
  geom_violin()

## Histogram - Continuous variables cut into bins.

diamonds %>%
  ggplot(aes(x = carat)) +
  geom_histogram(bins = 50)

## Density Plot - Continuous variables with smoothed density curves.

diamonds %>%
  ggplot(aes(x = carat)) +
  geom_density(fill = 'blue')

diamonds %>%
  ggplot(aes(x = carat, fill = cut)) +
  geom_density()

## 2D Density Plot - 2 Continuous Variables

movies %>%
  ggplot(aes(x = log(gross), y = imdb_score)) +
  geom_density2d_filled()

## 2D Histogram

movies %>%
  ggplot(aes(x = log(budget), y = imdb_score)) +
  geom_bin2d(bins = 60)

movies %>%
  ggplot(aes(x = imdb_score, y = log(gross))) +
  geom_bin2d(bins = 60)

## Hexbins - Useful in showing variation.

movies %>%
  ggplot(aes(x = log(budget), y = imdb_score)) +
  geom_hex(bins = 60)

## Color Theory

## Hue - What is the true underlying color from color wheel.
## Value - Lightness/Darkness - Translates to gray-scale.
## Intensity - Purity of the hue. Is there any white or black added to hue?

## Tinting vs. Shading

## Hex Codes (Colors) - 16,777,216 Combinations! - https://www.color-hex.com/

## Color Theory Rectangles - Let's make some colors. Be creative, try your own.
rect <- data.frame(x1 = 3, x2 = 5, y1 = 2, y2 = 5); rect
rect2 <- data.frame(x1 = 5, x2 = 7, y1 = 2, y2 = 5); rect2
rect3 <- data.frame(x1 = 3.75, x2 = 4.25, y1 = 3, y2 = 4); rect3
rect4 <- data.frame(x1 = 5.75, x2 = 6.25, y1 = 3, y2 = 4); rect4
rect5 <- data.frame(x1 = 4.25, x2 = 5.75, y1 = 3.3, y2 =  3.7); rect5

## Can you make two colors look like one? Can you make one color look like two?

ggplot() +
  geom_rect(data = rect, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#ffe599") +
  geom_rect(data = rect2, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#1f00e3") +
  geom_rect(data = rect3, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#e5ac23") +
  geom_rect(data = rect4, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#e5ac23") +
  theme_minimal()

## Let's explore the notion of 'transparent' colors. The 'transparency' comes
## from an averaging over hue, intensity, and value.

rect6 <- data.frame(x1 = 3, x2 = 4, y1 = 2, y2 = 5); rect6
rect7 <- data.frame(x1 = 4, x2 = 5, y1 = 2, y2 = 5); rect7
rect8 <- data.frame(x1 = 5, x2 = 6, y1 = 2, y2 = 5); rect8
rect9 <- data.frame(x1 = 6, x2 = 7, y1 = 2, y2 = 5); rect9
rect10 <- data.frame(x1 = 7, x2 = 8, y1 = 2, y2 = 5); rect10
rect11 <- data.frame(x1 = 8, x2 = 9, y1 = 2, y2 = 5); rect11
rect12 <- data.frame(x1 = 9, x2 = 10, y1 = 2, y2 = 5); rect12
rect13 <- data.frame(x1 = 10, x2 = 11, y1 = 2, y2 = 5); rect13
rect14 <- data.frame(x1 = 4.5, x2 = 9.5, y1 = 3.25, y2 = 3.75); rect14

## Colors interact with each other and 'vibrate'. 

ggplot() +
  geom_rect(data = rect6, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#0b2fb2") +
  geom_rect(data = rect7, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#2849bf") +
  geom_rect(data = rect8, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#4c68d0") +
  geom_rect(data = rect9, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#7f95e1") +
  geom_rect(data = rect10, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#DE582B") +
  geom_rect(data = rect11, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#E26439") +
  geom_rect(data = rect12, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#E77048") +
  geom_rect(data = rect13, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#EC7C56") +
  geom_rect(data = rect14, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#734F96") +
  theme_minimal()

## To create a palette for our colors, we can use 'colorRampPalette' which creates
## a function. The argument allows us to return as many break points as we like.

rc_pal <- colorRampPalette(c("#de582b", "#ffac91")) # Vermillion Colors
rc_pal(8)

hallow_pal <- colorRampPalette(c("#fb7417", "#f0a16b"))

## Color Theory

data("mtcars")  

mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point(color = "red") +
  theme_minimal()

## You could let color vary by many things. Color can reinforce movement.

## In this case, color may reinforce the notion of fuel efficiency, or the
## 'heaviness of the carbon footprint'.

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = mpg)) +
  geom_point() +
  labs(x = "Weight (in Thousands of Pounds)",
       y = "Miles Per Gallon",
       title = "Fuel Efficiency by Weight") +
  scale_color_gradientn(colors = c("black", "gray85")) +
  theme_minimal() +
  theme(legend.position = "none")

## Color may reference a different variable altogether.
## The 'colorRampPalette' creates a function for any color scale, using any colors
## desired. You do not necessarily have to pick only two colors.

rc_pal <- colorRampPalette(c("blue", "green", "purple"))

## As the argument, you choose the number of breaks you want.
rc_pal(9)

## You may insert the function inside the plot itself, though check the number of
## categories, as the palette has to match.

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  distinct(cyl_fact)

## As indicated previously, when using a discrete scale, the number inside should match.
mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(x = "Weight (in Thousands of Pounds)",
       y = "Miles Per Gallon",
       title = "Fuel Efficiency",
       subtitle = "By Weight and # of Cylinders",
       color = "Cylinders") +
  scale_color_manual(values = rc_pal(3)) + 
  theme_minimal() + 
  theme(legend.position  = c(0.8, 0.7),
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.title = element_text(size = 9))

## Colorspace Library
## Let's say you want discrete colors from a continuous scale.
## HCL = Hue (Wavelength/'Color'), Chroma (Intensity), Luminosity (Value)
# RGB: Red-Green-Blue, 8 bits each, 0 to 255

## colorRamp vs. colorRampPalette
## colorRamp returns RGB, colorRampPalette returns hex codes.

rc_pal <- colorRamp(c("orange", "blue"))
rc_pal(1) # Returns RGB values at lowest end.
rc_pal(0) # Returns at highest end.

rc_pal2 <- colorRampPalette(c("orange", "red"))
rc_pal2(3)
rc_pal2(7) # Returns hex codes, according to the number of breaks.

## There are alternate ways to create a scale.
sequential_hcl(4, "Purples 3")
sequential_hcl(3, c(rc_pal(0), rc_pal(1))) 

## Conversion - Hex Codes to RGB and vice versa.
hex2RGB("#ff0000") # Returned as RGB proportion.
col2rgb("#ff0000") # Returned in units.

## Color Blindness
## Deuteranopia = Blindness To Green (Green Cones Absent)
## Protanopia = Blindness To Red (Red Cones Absent)
## Tritanopia = Blindness To Blue (Exceptionally Rare
## Rod Cells: Brightness, Shape, Size
## Cone Cells: Color (Red, Blue, & Green-Sensitive Cones)  

plot1 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar()

## Checking the effect of various color blind afflictions.

colorBlindness::cvdPlot(plot1)

## Let's apply a new color scheme. The color scheme focuses on value/luminosity
## differences.

col_new <- c("#304e8d", "#92e5ad", "#ee542d")

plot2 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = col_new) +
  theme_minimal()

colorBlindness::cvdPlot(plot2)

## Viridis - A useful color scale.

plot3 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_viridis_d() +
  theme_minimal()

colorBlindness::cvdPlot(plot3)

## Taking advantage of the likelihood of color blindness afflictions, the best choice
## is to (a) vary value/lightness and (b) use blues. 

## Single Hue, Different Values (& Intensities)
## Purplish Blues

col_new2 <- c("#322671", "#5a48ba", "#9d8dee")

plot4 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = col_new2) +
  theme_minimal()

colorBlindness::cvdPlot(plot4)

## Using the 'scales' package, you can look at the colors ahead of time with 'show_col()'.
library(scales)
ramp_palette <- colorRampPalette(c("red", "purple", "blue"))
show_col(ramp_palette(5))

