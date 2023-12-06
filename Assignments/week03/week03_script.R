## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 3: ggplot2 & Color Theory
##
## Name: Carter Kruse
## Date: October 3rd, 2022

## For this entire assignment, we will use the following three data sets:
## World Income Inequality Dataset (WIID), Presidential Approval Dataset, &
## IMDB Movie Ratings Dataset

## In this assignment, we will fix all labels, themes, and colors. The default
## theme will be 'theme_minimal'. We will fix all labels in the 'labs' line
## within the plot. When using colors, we will use hex codes (www.color-hex.com),
## unless specifically suggested otherwise. There are almost 17,000,000 colors available.

## To deal with missing data points, we will remove the 'NA' missing data points
## inside the statistical functions like 'mean', 'median', 'min' and 'max' with the
## 'na.rm = TRUE' argument.

library(tidyverse)

## 1) IMDB Dataset

## Opening the IMDB data set and naming it 'movie'.

movie <- read_csv("/Users/carterkruse/Data Viz/movie_metadata.csv")

## Checking the head/tail to make sure it loaded correctly.

head(movie)
tail(movie)

## Looking at the data set.

glimpse(movie)

## Creating a simple theme for plots.

theme_simple <- theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
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

## Does the IMDB score differ between color and black-and-white movies?

## Creating a scatter plot that is 'jittered', which is designed to be instructive.

movie %>%
  ggplot(aes(x = color, y = imdb_score)) +
  geom_jitter() +
  labs(title = "IMDB Score by Color", x = "", y = "IMDB Score") + 
  theme_simple

## Removing the 'NA' category from the 'jittered' scatterplot. There are a number
## of ways to remove the 'NA' correctly.

movie %>%
  filter(!is.na(color)) %>%
  ggplot(aes(x = color, y = imdb_score)) +
  geom_jitter() +
  labs(title = "IMDB Score by Color", x = "", y = "IMDB Score") + 
  theme_simple

## Plotting the same variables, this time with a histogram, distinguishing the
## levels of the 'color' variable with an aesthetic.

movie %>%
  filter(!is.na(color)) %>%
  ggplot(aes(x = imdb_score, fill = color)) +
  geom_histogram(bins = 30) +
  labs(title = "IMDB Score by Color", x = "IMDB Score", y = "Count", fill = "") + 
  theme_simple

## The default position for 'geom_histogram' is stacked, meaning the categories
## of the variable and their counts will be stacked on top of each other, which
## may not be ideal. We will fix the 'position' argument in the next problem.

## It would be better to make a histogram where there are separate histograms for
## the categories of the 'color' variable, and where they are not stacked.

## Further, as there are significantly fewer black-and-white films, we present
## within-categories proportions (densities), rather than counts.

## Investigating 'geom_histogram' with the helper function. Specifically, we pay
## attention to how to change the default histogram's 'position' argument, and how
## to change the default 'stat' from counts to proportions/densities.

?geom_histogram

## Creating a density plot with separate histograms for the categories of
## the 'color' variable, where they are not stacked.

movie %>%
  filter(!is.na(color)) %>%
  ggplot(aes(x = imdb_score, y = ..density.., fill = color)) +
  geom_histogram(bins = 20, position = "dodge", stat = "bin") +
  labs(title = "IMDB Score by Color", x = "IMDB Score", y = "Density (Percentage)", fill = "") + 
  theme_simple

## Creating a different density plot with separate histograms for the categories
## of the 'color' variable, where they are not stacked. This method is used to
## 'smooth out' the graph to determine a central value.

movie %>%
  filter(!is.na(color)) %>%
  ggplot(aes(x = imdb_score, fill = color)) +
  geom_histogram(bins = 20, position = "dodge", stat = "density", n = 20) +
  labs(title = "IMDB Score by Color", x = "IMDB Score", y = "Density (Percentage)", fill = "") + 
  theme_simple

## 2) Approval Data Set

## The data set represents presidential approval data from 1977 to 2022. The variables
## include key dependent variables like presidential approval (approve) and
## presidential favorability ratings (favor), which are not the same. Favorability
## reflects a measurement of likability, while approval is about job performance.

## There are key economic variables, including a Gulf War indicator variable and
## a Lewinsky Scandal Variable. The data is in units of quarter-years.

## Opening the approval data set and naming it 'approve'.

approve <- read.csv("/Users/carterkruse/Data Viz/approval_data.csv")

## Checking the head/tail to make sure it loaded correctly.

head(approve)
tail(approve)

## Looking at the data set.

glimpse(approve)

## To quickly demonstrate why we are performing a multi-step problem, we make a
## quick line plot of 'year' on the x-axis and 'approve' on the y-axis.

approve %>%
  ggplot(aes(x = year, y = approve)) +
  geom_line() +
  labs(title = "(Presidential) Approval Rating by Year", x = "Year", y = "Approval Rating", fill = "") + 
  theme_simple

## The line looks rather strange, given what the unit of analysis is.

## Creating a new variable combining the time period variables ('year' and 'qrt')
## into a single year-quarter variable and plotting it to see the effect.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  ggplot(aes(x = year_qrt, y = approve)) +
  geom_line() +
  labs(title = "(Presidential) Approval Rating by Year", x = "Year", y = "Approval Rating", fill = "") + 
  theme_simple

## Sub-setting the data, selecting only the new time period variable and the
# economic approval and foreign policy approval measures.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, approve, favor)

## The two approval variables are really just one approval variable with different
## types. Thus, we combine them into a variable 'type' and an approval 'value'.
## This involves converting the data from 'wide form' to 'long form'.

## Using a series of 'pipe' operators to ensure it is a single bit of 'tidyverse' code.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, approve, favor) %>%
  pivot_longer(names_to = "type", values_to = "value", approve:favor) %>%
  select(everything())

## Creating a line plot of the two types of 'approval' mapped onto the y-axis,
## with the new time variable on the x-axis.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, approve, favor) %>%
  pivot_longer(names_to = "type", values_to = "value", approve:favor) %>%
  select(everything()) %>%
  ggplot(aes(x = year_qrt, y = value)) +
  geom_line(aes(color = type)) +
  scale_color_manual(values = c("#2A5AD3", "#EE6666"), labels = c("Approval Rating", "Favorability Rating")) +
  labs(title = "(Presidential) Approval Rating by Year", x = "Year", y = "Approval Rating", color = "") + 
  theme_simple +
  theme(legend.position = c(0.75, 0.8),
        legend.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(size = 0))

## Plotting the same graph, adding a smoother geom. We make the normal lines more
## transparent than the smoother lines via the 'alpha' parameter, setting it at 0.4
## This creates depth. Further, as before, we have clean labels and a theme.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, approve, favor) %>%
  pivot_longer(names_to = "type", values_to = "value", approve:favor) %>%
  select(everything()) %>%
  ggplot(aes(x = year_qrt, y = value)) +
  geom_line(aes(color = type), alpha = 0.4) +
  geom_smooth(aes(color = type), n = 200, span = 0.3, alpha = 0.2) + # The 'span' controls the amount of smoothing.
  scale_color_manual(values = c("#2A5AD3", "#EE6666"), labels = c("Approval Rating", "Favorability Rating")) +
  labs(title = "(Presidential) Approval Rating by Year", x = "Year", y = "Approval Rating", color = "") + 
  theme_simple +
  theme(legend.position = c(0.75, 0.8),
        legend.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(size = 0))

## Now, we create a line plot showing the two series of quarterly economic data,
## 'qrtinfl' (quarterly inflation) and 'qrtunem' (quarterly unemployment) from 
## the 'approve' data set.

## We repeat the same process and all the steps we just completed for the line plots
## making two lines. The colors of the approval types are red and blue.

## Creating a new variable combining the time period variables ('year' and 'qrt')
## into a single year-quarter variable and plotting it to see the effect.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  ggplot(aes(x = year_qrt, y = qrtinfl)) +
  geom_line() +
  labs(title = "Inflation by Year", x = "Year", y = "Inflation", fill = "") + 
  theme_simple

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  ggplot(aes(x = year_qrt, y = qrtunem)) +
  geom_line() +
  labs(title = "Unemployment by Year", x = "Year", y = "Unemployment", fill = "") + 
  theme_simple

## Sub-setting the data, selecting only the new time period variable and the
##  economic data (quarterly inflation and quarterly unemployment).

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, qrtinfl, qrtunem)

## The two economic data variables are really just one economic variable with different 
## types. Thus, we combine them into a variable 'type' and an approval 'value'.
## This involves converting the data from 'wide form' to 'long form'.

## Using a series of 'pipe' operators to ensure it is a single bit of 'tidyverse' code.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, qrtinfl, qrtunem) %>%
  pivot_longer(names_to = "type", values_to = "value", qrtinfl:qrtunem) %>%
  select(everything())

## Creating a line plot of the two types of economic data mapped onto the y-axis,
## with the new time variable on the x-axis.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, qrtinfl, qrtunem) %>%
  pivot_longer(names_to = "type", values_to = "value", qrtinfl:qrtunem) %>%
  select(everything()) %>%
  ggplot(aes(x = year_qrt, y = value)) +
  geom_line(aes(color = type)) +
  scale_color_manual(values = c("#2A5AD3", "#EE6666"), labels = c("Inflation", "Unemployment")) +
  labs(title = "Inflation & Unemployment by Year", x = "Year", y = "Economic Data (Percentage)", color = "") + 
  theme_simple +
  theme(legend.position = c(0.75, 0.8),
        legend.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(size = 0))

## Plotting the same graph, adding a smoother geom. We make the normal lines more
## transparent than the smoother lines via the 'alpha' parameter, setting it at 0.4
## This creates depth. Further, as before, we have clean labels and a theme.

approve %>%
  mutate(year_qrt = year + (qrt / 4)) %>%
  select(year_qrt, qrtinfl, qrtunem) %>%
  pivot_longer(names_to = "type", values_to = "value", qrtinfl:qrtunem) %>%
  select(everything()) %>%
  ggplot(aes(x = year_qrt, y = value)) +
  geom_line(aes(color = type), alpha = 0.4) +
  geom_smooth(aes(color = type), n = 200, span = 0.4, alpha = 0.2) + # The 'span' controls the amount of smoothing.
  scale_color_manual(values = c("#2A5AD3", "#EE6666"), labels = c("Inflation", "Unemployment")) +
  labs(title = "Inflation & Unemployment by Year", x = "Year", y = "Economic Data (Percentage)", color = "") + 
  theme_simple +
  theme(legend.position = c(0.75, 0.8),
        legend.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(size = 0))

## 3) WIID Data Set

## In order to do this, we install a package, 'readxl', to use the 'read_excel()' function.

library(readxl)

## Opening the WIDD data set and naming it 'wiid'.

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")

## Checking the head/tail to make sure it loaded correctly.

head(wiid)
tail(wiid)

## Looking at the data set.

glimpse(wiid)

## Creating a comparison between countries that is NOT a bar plot by plotting the
## average Gini index values as points for five countries in 2000: Germany, France,
## Italy, Spain, and Norway. The individual countries represent the x-axis.

## Add labels to the points that say the names of the countries, making sure that
## the labels do not overlap with the points. Since we are labeling the countries
## near the points themselves, we remove x-axis labels, as they are now redundant.

wiid %>%
  filter(year == 2000 & (country == "Germany" | country == "France" | country == "Italy" | country == "Spain" | country == "Norway")) %>%
  group_by(country) %>%
  summarize(avg_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = country, y = avg_gini, color = country, label = country)) +
  geom_point() +
  geom_text(hjust = -0.18, vjust = 0) +
  scale_color_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5", "#792BD5")) +
  labs(title = "Average Gini Index by Country (2000)", x = "Country", y = "Average Gini Index", color = "Country") +
  theme_simple +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

## Creating a density plot for the UN sub-regions of Africa by separating the
## density plot into plots by sub-region. The areas of the density plots are
## filled in with color.

wiid %>%
  filter(region_un == "Africa") %>%
  select(region_un_sub, gini) %>%
  ggplot(aes(x = gini, group = region_un_sub, color = region_un_sub, fill = region_un_sub)) +
  geom_density(alpha = 0.2) + # Used to see the different density curves clearly.
  scale_color_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5", "#792BD5")) +
  scale_fill_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5", "#792BD5")) +
  labs(title = "Income Inequality in Africa", x = "Gini Index", y = "Density", color = "UN Sub-Region", fill = "UN Sub-Region") +
  theme_simple

## Alternatively: We may use faceting to quickly view the discrepancies.

wiid %>%
  filter(region_un == "Africa") %>%
  group_by(region_un_sub) %>%
  select(region_un_sub, gini) %>%
  ggplot(aes(x = gini)) +
  geom_density(aes(color = region_un_sub, fill = region_un_sub), alpha = 0.2) +
  facet_wrap(~ region_un_sub, ncol = 1) +
  scale_color_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5", "#792BD5")) +
  scale_fill_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5", "#792BD5")) +
  labs(title = "Income Inequality in Africa", x = "Gini Index", y = "Density",
       color = "UN Sub-Region", fill = "UN Sub-Region") +
  theme_simple +
  theme(legend.position = "none")

library(forcats)

## Creating a bar plot of the difference between each African country's average
## Gini index score and the average Gini index score of the entire continent.

## Flipping the axes and coloring the bars according to the viridis color scale.

wiid %>%
  filter(region_un == "Africa") %>%
  mutate(avg_gini = mean(gini, na.rm = TRUE)) %>%
  group_by(country) %>%
  summarize(gini_difference = mean(gini, na.rm = TRUE) - avg_gini) %>%
  distinct() %>%
  ggplot(aes(x = fct_reorder(country, gini_difference), y = gini_difference, fill = fct_reorder(country, gini_difference))) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Income Inequality in Africa", x = "Country", y = "Gini Difference", 
       caption = "'Gini Difference' refers to the difference between each African country's average Gini index score 
       and the average Gini index score of the entire continent.") +
  theme_simple +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 6))

## Plotting the average Gini index scores by country for the UN region Europe, 
## showing one dot per country. We use "floating dots" to create this visual.

## Creating a point plot of 'Country' on the x-axis and 'Average Gini Index' on
## the y-axis. Do not add point labels; the x-axis labels are used instead.

## Flipping the axis and reordering the values.

wiid %>%
  filter(region_un == "Europe") %>%
  group_by(country) %>%
  summarize(avg_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = fct_reorder(country, avg_gini), y = avg_gini, color = fct_reorder(country, avg_gini))) +
  geom_point() +
  coord_flip() +
  scale_color_viridis_d() +
  labs(title = "Income Inequality (Europe)", x = "Country", y = "Average Gini Index", color = "Country") +
  theme_simple +
  theme(legend.position = "none")

## By suppressing the axis title 'Country', we create a pretty cool, simple, clean,
## and modern graphic.

wiid %>%
  filter(region_un == "Europe") %>%
  group_by(country) %>%
  summarize(avg_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = fct_reorder(country, avg_gini), y = avg_gini, color = fct_reorder(country, avg_gini))) +
  geom_point() +
  coord_flip() +
  scale_color_viridis_d() +
  labs(title = "Income Inequality (Europe)", x = "Country", y = "Average Gini Index", color = "Country") +
  theme_simple +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks.length = unit(1, "mm"))

## Creating a (transparent) scatter plot of median Gini index score by year for
## each of the UN sub-regions of the 'Americas' (UN Region). The color aesthetic
## is associated with each sub-region.

wiid %>%
  filter(region_un == "Americas") %>%
  group_by(region_un_sub, year) %>%
  summarize(avg_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_gini, color = region_un_sub)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5")) +
  labs(title = "(Median) Gini Index Score by Sub-Region by Year", subtitle = "UN Region: Americas",
       x = "Year", y = "Median Gini Index", color = "UN Sub-Region") +
  theme_simple +
  theme(legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"))

## Creating a (transparent) scatter plot of median Gini index score by year for
## each of the UN sub-regions of the 'Americas' (UN Region), with a smoothed line,
## using 'geom_smooth'.

wiid %>%
  filter(region_un == "Americas") %>%
  group_by(region_un_sub, year) %>%
  summarize(avg_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_gini, color = region_un_sub)) +
  geom_point(alpha = 0.5) +
  geom_smooth(n = 200, span = 0.8, alpha = 0.1) +
  scale_color_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5")) +
  labs(title = "(Median) Gini Index Score by Sub-Region by Year", subtitle = "UN Region: Americas",
       x = "Year", y = "Median Gini Index", color = "UN Sub-Region") +
  theme_simple +
  theme(legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"))

## Adding a gray line that averages over all the other smoothed lines, adjusting
## the colors and alpha as appropriate.

wiid %>%
  filter(region_un == "Americas") %>%
  group_by(region_un_sub, year) %>%
  summarize(avg_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_gini)) +
  geom_point(aes(color = region_un_sub), alpha = 0.5) +
  geom_smooth(aes(color = region_un_sub), n = 200, span = 0.8, alpha = 0.1) +
  geom_smooth(color = "#757575", n = 200, span = 0.8, alpha = 0.2) +
  scale_color_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5")) +
  labs(title = "(Median) Gini Index Score by Sub-Region by Year", subtitle = "UN Region: Americas",
       x = "Year", y = "Median Gini Index", color = "UN Sub-Region") +
  theme_simple

## Creating a dot plot, instead of a jittered scatter plot, looking at
## 'Western Asia', a UN sub-region, reporting Gini index by income group.

## The bins are organized on the y-axis and the dot plot centers the stacking
## of dots.

wiid %>%
  mutate(income_group = factor(incomegroup, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  filter(region_un_sub == "Western Asia") %>%
  ggplot(aes(x = income_group, y = gini, fill = income_group), color = "#000000") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.5, alpha = 0.6) +
  scale_fill_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5")) +
  scale_x_discrete(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")) +
  labs(title = "Gini Index Score by Income Group", subtitle = "UN Sub-Region: Western Asia",
       x = "Income Group", y = "Gini Index") +
  theme_simple +
  theme(legend.position = "none")

## Adding a further layer, a 'stat_summary', by adding a median Gini index score
## to the plot.

wiid %>%
  mutate(income_group = factor(incomegroup, levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))) %>%
  filter(region_un_sub == "Western Asia") %>%
  ggplot(aes(x = income_group, y = gini, fill = income_group), color = "#000000") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.5, alpha = 0.6) +
  scale_fill_manual(values = c("#D52B2B", "#40D52B", "#2BD4D5", "#2B5AD5")) +
  scale_x_discrete(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")) +
  labs(title = "Gini Index Score by Income Group", subtitle = "UN Sub-Region: Western Asia",
       x = "Income Group", y = "Gini Index") +
  theme_simple +
  theme(legend.position = "none") +
  stat_summary(fun = "median", color = "#FF0000", size = 2, geom = "point")

