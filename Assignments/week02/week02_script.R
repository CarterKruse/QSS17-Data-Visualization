## Data Visualization (GOVT16-QSS17) Fall 2022
## Intro to the Tidyverse
##
## Name: Carter Kruse
## Date: September 26th, 2022

## 1) Filter

## Loading the 'tidyverse' package.

library(tidyverse)

## Loading the 'swiss' data set and looking at the 'head'.
## This data represents a few key socioeconomic indicators for some of the
## provinces of Switzerland in the late-nineteenth century.

data("swiss")
head(swiss)

## Using the question mark helper function to see more about the data.

?swiss

## Turning the data set into a tibble.

swiss_tibble <- as_tibble(swiss); swiss_tibble

## Filtering the data set for provinces that have less than 50% male involvement
## in agriculture, using the pipe operator.

swiss %>% 
  filter(Agriculture < 50)

## Filtering the data set for provinces above 50% Catholic and below 70% fertility.

swiss %>%
  filter(Catholic > 50, Fertility < 70)

## 2) Arrange

## Taking the 'swiss' data set and arranging it by infant mortality.

swiss %>%
  arrange(Infant.Mortality)

## Arranging the 'swiss' data set by Catholic in descending order.

swiss %>%
  arrange(desc(Catholic))

## Taking the 'swiss' data set, filtering the data for provinces with less than
## 50% Catholics and arranging by fertility.

swiss %>%
  filter(Catholic < 50) %>%
  arrange(Fertility)

## 3) Mutate

## Downloading the IMDB movie data set onto your computer and naming it movies.
## Using the 'read_csv' function from the 'readr' package.

movies <- read_csv("/Users/carterkruse/Data Viz/movie_metadata.csv")
## movies <- read_csv(file.choose())

## Exploring the 'movies' data set by looking at its structure via 'glimpse'/'str'.

glimpse(movies)
str(movies)

## The budgets of movies are measured in individual dollars. Adding a new variable 
## to the data set ('budget2') with the 'mutate' function, that states the budget
## measured in millions of dollars.

movies <- movies %>%
  mutate(budget2 = budget / 1000000)

glimpse(movies)

## Adding a 'length' variable that is properly measured in hours, which is how
## normal people measure the length of movies.

movies <- movies %>%
  mutate(length = duration / 60)

glimpse(movies)

## Sub-setting the movie data set from "Spectre" to "Skyfall", using the titles
## in 'movie_title' to do this, rather than row/column numbers.

## You may need to combine functions/commands in order to do this. This will require
## a function that works with characters/strings ('stringr' package).

## The [1] ensures that the index of the *first* instance of the pattern is used.

first_index <- str_which(movies$movie_title, "Spectre")[1]
second_index <- str_which(movies$movie_title, "Skyfall")[1]

movies[first_index: second_index, ]

## 4) Plots & ggplot2

## Reading in the United Nations World Income Inequality Database (WIID).
## This is new data, updated and published in May 2021.

## In order to do this, we install a package, 'readxl', to use the 'read_excel()' function.

library(readxl)

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")

## Showing the 'head' and 'tail' of the data set.

head(wiid)
tail(wiid)

## There are a lot of variables in the data set, so we use 'glimpse' as well.

glimpse(wiid)

wiid %>%
  distinct(region_wb)

## Filtering the data set for just North American countries (including Mexico).

revised_wiid <- wiid %>%
  filter(region_wb == "North America" | country == "Mexico")

## The following would include Greenland as well.
## revised_wiid <- wiid %>%
##   filter(region_un_sub == "Northern America" | country == "Mexico")

## Producing a scatter plot of Gini coefficient ('gini') over time, using
## the 'year' variable for the x-axis. Mapping 'country' onto color using aes.

revised_wiid %>%
  ggplot(aes(x = year, y = gini, color = country)) +
  geom_point() +
  labs(title = "Gini Coefficient vs. Year", x = "Year", y = "Gini Coefficient",
       color = "Country") +
  theme_minimal()

## There is a little bit of over plotting, but we can see the basic trends pretty well.

## Repeating the plot above, this time with a line plot.

revised_wiid %>%
  ggplot(aes(x = year, y = gini, color = country)) +
  geom_line() +
  labs(title = "Gini Coefficient vs. Year", x = "Year", y = "Gini Coefficient",
       color = "Country") +
  theme_minimal()

## There is a significant amount of movement happening within the country lines.
## Investigate the data a bit to try to figure out why.
## Why is this??
  ## There are multiple Gini coefficient values for each country, each year, which
  ## requires vertical lines to plot.

  ## For example, in 2007, the United States
  ## had the following Gini coefficient values: 42.03, 39.35, 39.31, 43.45, 45.03,
  ## 42.62, 42.56, 46.07, 44.40, 46.30, 37.60, 40.80, 29.10.

  ## As there is drastic variation in the values, the line plot jumps up and down
  ## for each year, for each country.

## 5) Basic Statistics & Summaries w/ 'summarize'

## Summarizing the Gini index ('gini'), the first quintile ('q1'), and the
## fifth quintile ('q5'), giving the median of each of these using a single
## summarize function.

wiid %>%
  summarize(gini_index = median(gini, na.rm = TRUE),
            first_quintile = median(q1, na.rm = TRUE),
            fifth_quintile = median(q5, na.rm = TRUE))

## Viewing the unique/distinct categories for UN Regions and Sub-Regions.

unique(wiid$region_un)
unique(wiid$region_un_sub)
unique(wiid$region_wb)

## Determining the highest values for the Gini index ('gini'), 1st decile ('d1'),
## and 10th decile ('d10') values reported for Africa for the year 2000.

## The deciles represent percentages of countries’ population and their income held,
## i.e. 'd1' reflects the percent of a country's income held by the poorest 10%
## of the population.

wiid %>%
  filter(year == 2000 & region_un == "Africa") %>%
  summarize(highest_gini = max(gini, na.rm = TRUE),
            highest_d1 = max(d1, na.rm = TRUE),
            highest_d10 = max(d10, na.rm = TRUE))

## Filtering the WIID for the United States in the year 2000 to show the Gini
## index value(s) without summarizing them.

wiid %>%
  filter(year == 2000 & country == "United States") %>%
  select(gini)

## This is instructive about the plot we tried earlier. Recall when a scatter plot
## and line plot were created for North American countries. Over-plotting occurred
## because there are multiple Gini coefficient values for each country, each year.

## Filtering the WIID for African countries, reporting the means, medians, and
## standard deviations of Gini coefficients for Africa's UN sub-regions.

## Hint: There should be grouping going on here, and the result should be just
## a few rows with four columns.

wiid %>%
  filter(region_un == "Africa") %>%
  group_by(region_un_sub) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE),
            median_gini = median(gini, na.rm = TRUE),
            standard_dev_gini = sd(gini, na.rm = TRUE))

## 6) Plotting

## Grouping the WIID data by UN region and year and creating separate scatter plots
## from a single, faceted plot environment of each region's mean Gini index values.
## The x-axis, y-axis, and title are re-labeled.

wiid %>%
  group_by(region_un, year) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_gini, color = region_un)) +
  geom_point() +
  facet_grid(rows = vars(region_un)) +
  labs(title = "Gini Coefficient vs. Year by UN Region", x = "Year", y = "Mean Gini Coefficient",
       color = "UN Region") +
  theme_minimal()

## Grouping the WIID data by OECD status and year and creating scatter plots for
## median Gini index values by OECD status.

wiid %>%
  group_by(oecd, year) %>%
  summarize(median_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = median_gini, color = oecd)) +
  geom_point() +
  geom_smooth() + # Using 'geom_smooth' to produce a smooth line.
  labs(title = "Gini Coefficient vs. Year by OECD Status", x = "Year", y = "Median Gini Coefficient",
       color = "OECD Status") +
  theme_minimal()

## Creating a scatter plot of median Gini index values for each UN region by year,
## filtering for years after 1945. Adding color for regions and faceting the plots
## by UN region.

wiid %>%
  group_by(region_un, year) %>%
  filter(year > 1945) %>%
  summarize(median_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = median_gini, color = region_un)) +
  geom_point() +
  facet_grid(rows = vars(region_un)) +
  labs(title = "Gini Coefficient vs. Year by UN Region", subtitle = "After 1945", 
       x = "Year", y = "Median Gini Coefficient", color = "UN Region") +
  theme_minimal()

## Creating a bar plot of median Gini index values for each UN region by year,
## filtering for years after 1945. Adding color for regions and faceting the plots
## by UN region.

wiid %>%
  group_by(region_un, year) %>%
  filter(year > 1945) %>%
  summarize(median_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = median_gini, fill = region_un)) +
  geom_bar(stat = "identity", width = 0.8) +
  facet_grid(rows = vars(region_un)) +
  labs(title = "Gini Coefficient vs. Year by UN Region", subtitle = "After 1945", 
       x = "Year", y = "Median Gini Coefficient", fill = "UN Region") +
  theme_minimal()

## Creating a histogram of the percentage of income held by the top decile ('d10')
## in countries across the world. Using a 'fill' aesthetic separating the
## histogram by UN region.

wiid %>%
  ggplot(aes(x = d10, fill = region_un)) +
  geom_histogram(binwidth = 2) + 
  labs(title = "Percentage of Income (Top Decile)", x = "Percentage", y = "Count", fill = "UN Region") +
  theme_minimal()

## The y-axis of the previous histogram was a count. We can change a histogram to
## behave more like a density plot, where the y-axis accounts for the proportion
## or density of each region's values.

## To do so, we take the previous histogram code and adapt it so that regions
## overlap on the plot (the default 'position' argument is a stacked histogram).
## Adding a y aesthetic changes the underlying statistic performed to a density
## and not a count. The 'alpha' transparency argument is set to 0.6

wiid %>%
  ggplot(aes(x = d10, y = ..density.., fill = region_un)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(title = "Percentage of Income (Top Decile)", x = "Percentage", y = "Density", fill = "UN Region") +
  theme_minimal()

## Alternatively: Use 'geom_density' to display a continuous plot.

wiid %>%
  ggplot(aes(x = d10, color = region_un, fill = region_un)) +
  geom_density(alpha = 0.6) +
  labs(title = "Percentage of Income (Top Decile)", x = "Percentage", y = "Density", 
       color = "UN Region", fill = "UN Region") +
  theme_minimal()

## Creating a box plot of the lowest quintile ('q1') values of income by UN region.
## This is another way of seeing income inequality beyond Gini scores.

wiid %>%
  group_by(region_un) %>%
  ggplot(aes(x = region_un, y = q1)) +
  geom_boxplot() + 
  labs(title = "Lowest Quintile Values (Income) vs. UN Region", 
       x = "UN Region", y = "Lowest Quintile Values (Income)") +
  theme_minimal()

## Checking to see if income inequality within countries varies by income group
## across the UN regions.

## Creating a series of box plots (using a 'facet' function) to show the average
## Gini scores broken down by income group and faceted by UN region.

## An ordered factor is necessary to ensure the income group categories are in
## the right order.

wiid %>%
  group_by(incomegroup, region_un) %>%
  ggplot(aes(x = factor(incomegroup, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"), ordered = TRUE), y = gini,
             color = region_un)) +
  geom_boxplot() +
  facet_grid(rows = vars(region_un)) +
  labs(title = "Gini Coefficient by Income Group by UN Region", 
       x = "Income Group", y = "Gini Coefficient", color = "UN Region") +
  theme_minimal()

