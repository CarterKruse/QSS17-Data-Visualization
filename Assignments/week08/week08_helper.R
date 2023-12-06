## Data Visualization (GOVT16-QSS17) Fall 2022
## Introduction To Geographic Data & Mapping
##
## Professor Robert A. Cooper
## Week 8

library(tidyverse)
library(devtools)

## Mapping - All about points, lines, and polygons.

## You may need to install some of the following packages from GitHub.

library(sf) # For working with 'simple feature' format.
library(rgdal) # For reading in 'sp' objects.
library(ggspatial) # For 'sp' and 'sf' to data frame (rarely used).
library(USAboundaries) # For a set of US map data points.
library(USAboundariesData) # Data sets related to USAboundaries.
library(tmap) # A way to easily map 'sf' and 'sp' objects.

library(rnaturalearth) # Mapping Data
library(rnaturalearthdata) # High-Resolution Natural Earth Data

## Mapping in R

## A few object classes to be familiar with: 'sp', 'sf', and 'spatial data frames'.
## 'sp' Objects: S4 Object, Extra-Nested List-Like Object
## 'sf' Objects: S3 Data Frame + 'Geometry' (A List Object)
## 'Spatial Data Frames': S3 Regular Data Frames (With Longitude/Latitude Variables)

## Mapping Data: Many Sources
## 'ggplot2': Contains its own map data.
## 'maps': Various data sets.
## 'USAboundaries'
## 'USAboundariesData'

## Spatial Data Objects
## Complexity: 'sp' > 'sf' > 'Data Frame' 
## 'sp' (Spatial Objects): S4 class objects with different nesting structure.
## 'sf' (Simple Feature Objects): Contain a 'geometry' variable for coordinates.
## Data Frames: Polygon data with longitude/latitude as separate variables.

## Spatial Objects
## 'sp' Objects - S4 Class
## All others we are working with in this course are S3.
## Subsetting - There is a difference, as we have slots (fields).
## The '$' is replaced with '@', and slot() replaces [[]].

## 'sp' Objects
## You are importing shape files, which are folders of multiple files, including
## a '.shp' file. Do not ever remove the '.shp' file from the folder with the other
## spatial files.

spdat1 <- readOGR("/Users/carterkruse/Data Viz/US_Climate/US_Climate.shp") # 'rgdal'
sp_sfdat <- st_read("/Users/carterkruse/Data Viz/US_Climate/US_Climate.shp") # 'sf'

## The 'rgdal' package is increasingly being folded into the 'sf' package.
## We will focus primarily on the 'sf' package and 'sf' objects.

## The two read-ins create different object types. The 'sf' object is simplified.
class(spdat1)
class(sp_sfdat)

spdat1
sp_sfdat

## There is a crazy sub-setted data flow in the structure of 'spdat1'.
str(spdat1)
str(sp_sfdat)

## Sub-setting an 'sp' uses 'slots' with the '@' symbol.
spdat1@data # Features about the climate stations.

## Moving down to the smallest subset looks different.

## There will always be a 'bbox', a 'bounding box' (smallest box encompassing all points in set).
spdat1@bbox

## There will always be a projection string.
spdat1@proj4string

## Similar to a normal list, you can continue to subsetting and transform/tidy data.
spdat1@data$STATE

## Mapping - 'sp' Objects - 'tmap' or 'spplot'
## Alternative: Convert the 'sp' object to 'sf' or 'spatial data frame'.
## 'sf' objects are flexible and easier to work with.

## 'sf' Objects - Look and act like data frames, except for the geometry.

dev.off()

## devtools::install_github("https://github.com/ropensci/USAboundariesData")
## library(USAboundariesData)
## library(USAboundaries)

## library(usmap)
## library(usmapdata)

## Map Data
state_dat <- us_states(); state_dat

## Point Data
sp_sfdat

## Simple Plot - US
sp_sfdat %>%
  ggplot() +
  geom_sf()

## Simple Plot - US w/ Coordinates
sp_sfdat %>%
  ggplot() +
  geom_sf() +
  coord_sf()

## Map 'Data Frames' & 'sf' Objects - Mix In ggplot
## Let's say we wanted to map the stations for June temperatures.

sp_sfdat %>%
  ggplot() +
  geom_sf(aes(color = T06), size = 0.4) +
  coord_sf() +
  theme_minimal()

## Let's add a map!
## Combining the underlying map data with the climate data.

states1 <- map_data("state") # From ggplot2 (Data Frame)
states2 <- us_states() # From USAboundaries ('sf' Object w/ Geometry)

class(states1)
class(states2)

states2 %>%
  ggplot() +
  geom_sf(size = 0.2) +
  geom_sf(data = sp_sfdat, aes(color = T06), size = 0.6) +
  coord_sf(xlim = c(-130, -55), ylim = c(20, 55)) +
  theme_minimal()

## Mix 'Data Frame' & 'sf' Object

ggplot(data = states1) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#3C3C3C", color = "#000000", size = 0.2) +
  geom_sf(data = sp_sfdat, aes(color = T01), size = 1)  +
  labs(title = "U.S. Climate Data: Measurement Stations", color = "Temperature \n (Degrees Celsius)") +
  theme_minimal() +
  coord_sf(xlim = c(-130, -60), ylim = c(23, 50)) +
  scale_color_gradientn(colors = c("#001DFF", "#FCFF00")) +
  theme(legend.position = c(.9, .3))











glimpse(sp_sfdat)
head(sp_sfdat)


glimpse(sp_sfdat)
sp_sfdat # sf object, which is an S3 object. 

isS4(spdat1)
class(spdat1); class(sp_sfdat)

# To install the collection of rnaturalearth packages, try the code below. 

# devtools::install_github("ropenscilabs/rnaturalearth")
# devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")


# We will discuss three types of mapping objects. 
# NOTE: if you get very/overly interested in mapping, I suggest long-term that 
# you also take a look at QGIS, a separate, open-sources GIS software. 
# QGIS can connect to both R and Python, for those interested. 

# data frame/spatial data frames. Easiest, but easiest to screw up.
# sf. simple features. Best to work with, in my opinion.
# sp. spatial polygon data. GIS style. Complicated, nested structure. 



# https://www.naturalearthdata.com/downloads/




# Typically, what information are we looking for? 
# (1) Longitude and latitude.
# (2) Instructions on if and how to draw the points together. 
#     - Points can form (a) points, (b) lines, or (c) polygons.
#     - They can also be organized into small square areas, called rasters. 
# (3) A 'bounding box', the box with the smallest measure within which all the points lie.
# (4) A coordinate reference system, or CRS. How and where to draw our data on our flat map. 

# The CRS contains a lot of information:
#   - The geographic coordinate reference system
#   - The most common is WGS 84, or World Geodetic System 1984. 
#   - Another fairly common is EPSG (European Petroeum Study Group)

# Most importantly for you, the CRS for different objects has to match if you want to map them together!!!

# For sf objects, all of this exists inside a single list variable called GEOMETRY. 
# For sp objects, these pieces of information are in various 'slots' in the object. 

sp_sfdat$geometry[1]


# Subsetting an 'sp' uses "slots" with the '@' symbol, for starters.

spdat1@data # Features about the climate stations here. 
spdat1@coords
spdat1@bbox # There will always be a 'bbox': a "bounding box" (smallest box encompassing all points in set)








# Maps comes in a range of low to high-resolution forms. 
# The choice depends on what you want to do with the data and your computer's ability to produce the map.

state_dat <- us_states() #map data. 
state_dat

class(state_dat)

state_dat$geometry
sp_sfdat$geometry[1]

# One example of the continental U.S.
# For typical maps of the US, Alaska and Hawaii are problems unless you inset them. 

state_dat %>%
  ggplot() +
  geom_sf(size = 0.1) +
  coord_sf(xlim = c(-135, -60)) + # To drop Alaska and Hawaii. 
  theme_minimal()

# To add Hawaii and Alaska back, we have some options:
# (1) fiftystater. 
# (2) usmaps package. plot_usmap function. 
# (3) Use inset plotting. Make 3 plots; inset 2. 
 
# library(devtools)
devtools::install_github("wmurphyrd/fiftystater")

# For fiftystater...

library(fiftystater)

fifty_states

fifty_states %>%
  ggplot(aes(x = long, y = lat, group = group, order = order)) +
  geom_polygon(fill = "gray80", color = "gray20", size = 0.2) +
  coord_map() +
  theme_minimal()







#########################
# Data frame with x and y long & lat variables. 
# This is fine, but instructions are needed. 
# sp and sf objects come with all their instructions ready to go. 
# Data frame objects have to get the grouping and ordering JUST RIGHT to work. 

# More maps. This time from ggplot2 itself. 

world <- map_data("world") # from ggplot2. Normal data frame. 
head(world); class(world)

states1 <- map_data("state") # from ggplot2. Normal data frame.
head(states1)

# Simple map of US from a data frame. 
# Thus, we use geom_polygon instead. 
# NOTE: don't go from sp to sf to data frame.
# Use data frame data if the map data are already in data frame format!

states1 %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "gray20", size = 0.1) +
  coord_map() +
  theme_minimal()






#########
# Let's work a bit with the USAboundaries data. 
# If you have trouble finding USAboundariesData, then...

library(devtools)
devtools::install_github("https://github.com/ropensci/USAboundariesData")

library(USAboundariesData)
library(USAboundaries) # A number of levels of mapping data available. Investigate. 
library(ggspatial) # Easily transform sp and sf objects into data frames. 

class(state_dat); glimpse(state_dat)
state_dat # US states, sf object. 

# ggspatial allows you to convert directly to data frame from sf or sp.
# NOTE: this is a cautionary tale. Don't go to data frames unless you have no options. 

statez <- df_spatial(state_dat)
class(statez); glimpse(statez)
head(statez)

# From the converted data frame object...
# WHAT IS WRONG HERE? Can you tell?

statez %>%
  ggplot(aes(x = x, y = y, group = feature_id)) +
  geom_polygon()

# The above fails across various grouping variables. So just use sf!!!!!

state_dat %>%
  ggplot() +
  geom_sf() +
  coord_sf() +
  theme_minimal()

# tmap is also an option. Useful for working with sp objects. 
# tmap is a very nice package. 

library(tmap)

state_dat %>%
  tm_shape() +
  tm_borders()

###########################################################################
# USABoundaries. Historical data. 
# The USABoundaries data allow us to map the US and its historical borders. 

usa <- us_states("1785-01-01")
class(usa); head(usa)

# We could try tmap, just to see how it is different. tmap vs. sf below.  

# tmap takes sf objects or sp objects. 

usa %>%
  tm_shape() +
  tm_borders()

# ggplot can take data frames or sf objects. 

usa %>%
  ggplot() +
  geom_sf()+
  coord_sf() +
  theme_minimal()

### You can also get cities data from USABoundaries.  

cty_dat <- USAboundaries::us_cities()

glimpse(cty_dat)
class(cty_dat) # See class 'sf' and 'data.frame'. 

##########
##### Spatial data frames. From ggplot itself. 
# These are normal data frames with longitude and latitude as variables.
# You need to be especially careful about grouping. 
# Now we can make a map with geom_polygon. 

# Let's grab some map data from ggplot2. 
# Finding the grouping variable won't always be this easy.

world <-map_data("world2")
head(world)

county <- map_data("county")
head(county)

world %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray80", color = "black", size = 0.1) +
  theme_minimal() +
  coord_map()

###
# Another great data source: rnaturalearth.
# One advantage of rnaturalearth is that you have choices about level of resolution.


library("rnaturalearth")
library("rnaturalearthdata")



world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

# Let's take a quick look at the resulting map data.

glimpse(world)

# Once again, you could use tmap or sf::geom_sf, no problem.  

world %>%
  ggplot() +
  geom_sf(size  = 0.2) +
  coord_sf() +
  theme_minimal()

