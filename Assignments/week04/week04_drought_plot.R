## Data Visualization (GOVT16-QSS17) Fall 2022
## Drought Plot
##
## Name: Carter Kruse
## Date: October 4th, 2022

## Importing Libraries
library(tidyverse)
library(lubridate)
library(usdata)
library(reshape2)
library(colorBlindness)

## Reading in the CSV file for the data set.
drought2.0 <- read.csv("/Users/carterkruse/Data Viz/us_drought_dat/drought2.csv")

## Looking at the data set.
glimpse(drought2.0)

## Modifying/creating new columns for the data set.
drought2.1 <- drought2.0 %>%
  mutate(state = State, year = year(date))

## Looking at the data set.
glimpse(drought2.1)

## Creating 'region' variable based on the 'state' variable.
drought2.2 <- drought2.1 %>%
  mutate(region = (state == "WA" | state == "OR" | state == "ID" | 
                   state == "MT" | state == "WY" | state == "CO" | 
                   state == "CA" | state == "NV" | state == "UT" |
                   state == "AZ" | state == "NM") * (1) +
                   (state == "ND" | state == "SD" | state == "NE" | 
                   state == "KS" | state == "MN" | state == "IA" | 
                   state == "MO" | state == "WI" | state == "IL" |
                   state == "MI" | state == "IN" | state == "OH") * (2) +
                   (state == "TX" | state == "OK" | state == "AR" | 
                   state == "LA" | state == "MS" | state == "AL" | 
                   state == "TN" | state == "KY" | state == "WV" |
                   state == "MD" | state == "DE" | state == "VA" |
                   state == "NC" | state == "SC" | state == "GA" |
                   state == "FL") * (3) +
                   (state == "PA" | state == "NJ" | state == "NY" | 
                   state == "CT" | state == "RI" | state == "MA" | 
                   state == "VT" | state == "NH" | state == "ME") * (4))

## Looking at the data set.
glimpse(drought2.2)

## Grouping the data set by 'region' and 'year', finding 'dsci' averages.
drought2.3 <- drought2.2 %>%
  group_by(region, year) %>%
  summarize(avg_dsci = mean(DSCI, na.rm = TRUE))

## Looking at the data set.
glimpse(drought2.3)

## Removing all years except for the last 10.
drought2.4 <- subset(drought2.3, year != 2000 & year != 2001 & year != 2002 & 
                                 year != 2003 & year != 2004 & year != 2005 &
                                 year != 2005 & year != 2006 & year != 2007 &
                                 year != 2008 & year != 2009 & year != 2010 &
                                 year != 2011 & year != 2012)

## Looking at the data set.
glimpse(drought2.4)

## Joining the 'x' variables to consider.
drought2.5 <- melt(drought2.4, id.vars = c("year", "region"))

## Looking at the data set.
glimpse(drought2.5)

## Plotting the data.
plot1 <- drought2.5 %>%
  ggplot(aes(x = region, y = value, group = as.factor(region), fill = as.factor(region))) +
  geom_bar(stat = "identity", width = 0.8, position = "dodge") +
  facet_grid(~year) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("Regions") + ylab("Average DSCI Value") + labs(fill = "Region") +
  ggtitle("Average DSCI Value by Region by Year") +
  scale_fill_viridis_d(labels = c("Pacific", "West", "Midwest", "South", "Northeast"))

plot1

## Checking the plot in color blind palettes.
colorBlindness::cvdPlot(plot1)
