## Data Visualization (GOVT16-QSS17) Fall 2022
## Week 8: Geospatial Data in R
##
## Name: Carter Kruse
## Date: November 7th, 2022

## Packages needed to complete this assignment include the following: 'tidyverse',
## 'USAboundaries', 'rgdal', 'sp', 'sf', and 'ggspatial'.

## Look at the documentation for 'USAboundaries'

## In this assignment, we will fix all labels (x-axis, y-axis, titles, legend)
## and use hex codes to make colors.

library(tidyverse)
library(devtools)

library(sp)
library(sf)
library(rgdal)
library(ggspatial)

library(USAboundaries)
library(USAboundariesData)

## 1) Mapping Vermont & New Hampshire

## Adding Vermont & New Hampshire cities to the map, including Hanover.
## There are multiple city data sets available, 'us_cities()' will be used.

## Loading in the data sets.

city_data <- us_cities(); city_data
glimpse(city_data)
class(city_data)

state_data <- us_states(); state_data
glimpse(state_data)
class(state_data)

## Modifying 'city_data' and 'state_data' to only include Vermont & New Hampshire.

partial_city_data <- city_data %>%
  filter(state_name == "Vermont" | state_name == "New Hampshire")

partial_city_data

final_state_data <- state_data %>%
  filter(name == "Vermont" | name == "New Hampshire")

final_state_data

## Checking to see if Hanover is in the 'partial_city_data', which it is not.

"Hanover" %in% partial_city_data$city

## Adding Hanover to the city data.

city_data_hanover = st_sf(city = "Hanover", state_name = "New Hampshire", state_abbr = "NH", 
                          county = "GRAFTON", county_name = "Grafton", stplfips_2010 = "3333333", 
                          name_2010 = "Hanover city", city_source = "US Census Bureau", 
                          population_source = "US Census Bureau", place_type = "Incorporated City", 
                          year = 2010, population = 8636, 
                          geometry = st_sfc(st_point(c(-72.2896, 43.7022))), crs = st_crs(4326))

final_city_data <- rbind(partial_city_data, city_data_hanover)

final_city_data

## Basic Map - The size of the points is based on the population of the cities.

nh_vt_map <- ggplot() +
  geom_sf(data = final_state_data, size = 0.2, color = "#000000") +
  geom_sf(data = final_city_data, aes(size = population), color = "#2970CD", alpha = 0.5) +
  coord_sf(xlim = c(-74, -70), ylim = c(42, 46)) +
  labs(title = "New Hampshire & Vermont - City By Population",
       x = "Longitude", y = "Latitude", size = "Population") +
  theme_minimal()

nh_vt_map

## Modifying the colors and adjusting the titles.

nh_vt_map_final <- nh_vt_map +
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

nh_vt_map_final

## 2) WIID Data - 'Choropleth' (Geopolitical Map)

library(readxl)

## 'Choropleth' - Geopolitical map with a variable or variables defining aesthetics of the map.

## Opening the WIDD data set and naming it 'wiid'.

wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")
glimpse(wiid)

## Loading in the data set for the world map (of countries).

world_map <- map_data("world")
glimpse(world_map)
class(world_map)

## Calculating the average Gini index for each country in Europe from 2000 onward.

wiid_europe <- wiid %>%
  filter(region_un == "Europe", year >= 2000) %>%
  group_by(country) %>%
  mutate(avg_gini = mean(gini, na.rm = TRUE),
         region = case_when(
           country == "Czechia" ~ "Czech Republic", # Re-Coding
           TRUE ~ country)) %>%
  ungroup() %>%
  select(region, avg_gini) %>%
  distinct(region, avg_gini)

glimpse(wiid_europe)

## Joining the data sets to include the 'avg_gini' with the 'lat' and 'long'.

choropleth_data <- right_join(world_map, wiid_europe, by = "region") %>%
  select(long, lat, group, avg_gini, region)

glimpse(choropleth_data)

choropleth_data %>%
  distinct(region)

## Removing Russia from the plot and focusing on continental Europe.

removed_regions = c("Russia", "Ireland", "Iceland", "Serbia and Montenegro", "United Kingdom")

## Basic Map - The labels are fied and the size of border lines are dropped to 0.2

choropleth_base_map <- choropleth_data %>%
  filter(! region %in% removed_regions) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = avg_gini), color = "#000000", size = 0.2) +
  scale_fill_continuous(breaks = c(27, 30, 33, 36, 39)) +
  scale_x_continuous(breaks = c(0, 20, 40),
                     labels = c("0ºE", "20ºE", "40ºE")) +
  scale_y_continuous(breaks = c(40, 50, 60, 70),
                     labels = c("40ºN", "50ºN", "60ºN", "60ºN")) +
  coord_fixed(ratio = 1.2, xlim = c(-15, 40), ylim = c(35, 72)) +
  labs(title = "Average Gini Index (Europe)", subtitle = "(2000 - Present)",
       x = "Longitude", y = "Latitude", fill = "Averge Gini Index") +
  theme_minimal()

choropleth_base_map

## Plotting the (final) map with the legend inside the plotting environment, and
## the other theme elements.

choropleth_final_map <- choropleth_base_map +
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
        legend.position = c(0.2, 0.8))

choropleth_final_map

## 3) Louisiana Purchase - Historical Data

## Map of the United States just after the Louisiana Purchase in 1803.
## You can essentially choose a date at any point in the history of the U.S. and
## produce a map matching the date.

## Know which type of spatial objects you are working with.

louisiana_purchase_map <- us_states(map_date = "1803-04-30")
glimpse(louisiana_purchase_map)
class(louisiana_purchase_map)

## Basic Map - The Louisiana Purchase is its own color.

louisiana_purchase_base_map <- louisiana_purchase_map %>%
  ggplot() +
  geom_sf(aes(fill = name), size = 0.3, color = "#000000") +
  coord_sf(xlim = c(-115, -60), ylim = c(25, 52)) +
  scale_fill_manual(values = c(rep("gray90", 6), "red", rep("gray90", 15))) +
  labs(title = "Louisiana Purchase", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "none")

louisiana_purchase_base_map

library(ggrepel)
library(ggsflabel)

## Plotting the (final) map with the territory labels and theme elements.

louisiana_purchase_final_map <- louisiana_purchase_base_map  +
  geom_sf_label_repel(aes(label = name), size = 2, force = 1, force_pull = 10,
                      nudge_x = 0, nudge_y = 0, seed = 10,
                      max.overlaps = Inf, min.segment.length = 0) +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

louisiana_purchase_final_map

## 4) Trail Of Tears

## A way to honor people who have suffered greatly is to know their story.
## To know the plight of Native Americans, we make a map of their journey from
## the East to the Oklahoma Territory.

## Reading in the '.shp' file through the 'st_read' function.

trail_of_tears <- st_read("/Users/carterkruse/Data Viz/trail_of_tears/trte_nht_100k_line.shp")
glimpse(trail_of_tears)

## Depending on whether you use 'ggplot2' or 'tmap' to map the trail, you might
## need to transform the object into another type (data frame, sf, sp).

## Using a historical map.

historical_state_map <- us_states(map_date = "1850-01-12")
glimpse(historical_state_map)

## Creating appropriate labels for the territories.

historical_state_map_labels <- historical_state_map %>%
  filter(name == "Missouri" | name == "Arkansas" | name == "Mississippi" |
           name == "Illinois" | name == "Tennessee" | name == "Alabama" |
           name == "Georgia" | name == "Kentucky" | name == "Indiana" |
           name == "Ohio")

## Plotting basic map, giving the trials a color (red). The states in which the
## trails are located are zoomed in.

trail_of_tears_base_map <- ggplot() +
  geom_sf(data = historical_state_map, size = 0.2, color = "#000000") +
  geom_sf(data = trail_of_tears, color = "#FF0000", alpha = 0.6) +
  coord_sf(xlim = c(-96, -82), ylim = c(33, 40)) +
  labs(title = "Trail Of Tears", x = "Longitude", y = "Latitude") +
  theme_minimal()

trail_of_tears_base_map

## Plotting the (final) map with the labels and theme elements.

trail_of_tears_final_map <- trail_of_tears_base_map  +
  geom_sf_label_repel(data = historical_state_map_labels, aes(label = name), size = 2, 
                      force = 0, force_pull = 10, seed = 10, max.overlaps = Inf) +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(size = 8, hjust = 1, family = "sans"))

trail_of_tears_final_map

## 5) Presidential Election

## In the 2016 election, any one of a number of swing states could have been counted
## as the difference-maker in the Electoral College delegate count.

## The data set is from the MIT Election Data & Science Lab, for election results by county.

?us_counties
county_data <- us_counties(states = "Florida")
glimpse(county_data)

## Simplifying the data set to include only the county name (lowercase) and geometry.
simple_county_data <- county_data %>%
  select(name, geometry) %>%
  mutate(county = tolower(name)) %>%
  select(county, geometry)

glimpse(simple_county_data)

## Reading in the csv file containing the election results.

election_results <- read_csv("/Users/carterkruse/Data Viz/presidential_elections/countypres_2000-2016.csv")
glimpse(election_results)

## Determining the winner in each county, by political party.

florida_county_election_results <- election_results %>%
  filter(year == 2016, state == "Florida") %>%
  mutate(county = tolower(county)) %>%
  group_by(county) %>%
  top_n(1, candidatevotes) %>%
  select(county, party)

## Joining the county data (geometry) with the election results.

final_election_map_data <- left_join(simple_county_data, florida_county_election_results, by = "county")

## Mapping the data from the winner in each county to counties for the state of Florida,
## using a red and blue discrete palette for showing the winners from eahc county.

election_base_map <- final_election_map_data %>%
  ggplot() +
  geom_sf(aes(fill = party), size = 0.2, color = "#000000") +
  coord_sf(xlim = c(-88, -80), ylim = c(24, 31)) +
  scale_fill_manual(values = c("#0000FF", "#FF0000"),
                    labels = c("Democrat", "Republican")) +
  labs(title = "Florida (2016) Election Map by County", x = "Longitude", y = "Latitude",
       fill = "Party") +
  theme_minimal()

election_base_map

## Plotting the (final) map with the theme elements.

election_map_final <- election_base_map +
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

election_map_final

