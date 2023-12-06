## Data Visualization (GOVT16-QSS17) Fall 2022
## Functions
##
## Professor Robert A. Cooper
## Week 6

## Functional Programming, Loops, Map Functions

library(tidyverse)
library(ggplot2)
library(purrr)
library(repurrrsive)
library(readxl)

## Functions
## Start with the arguments needed, then brackets. Always print what you want.

adding <- function(x, y) {
  addition <- x + y
  print(addition)
}

adding(3, 4)

coops <- function(x, y) {
  value <- x^2 + y^3
  print(value)
}

coops(4, 8)

basic <- function(data) {
  data_sub <- data %>%
    select(starts_with("q"))
  print(data_sub)
}

## Opening the WIDD data set and naming it 'wiid'.
wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")

## Looking at the data set.
glimpse(wiid)

## Base R - Split Function
wiid %>%
  split(.$region_un)

## Using the 'map()' function from 'purrr'. This is a vectorized function applied
## to a list, which is particularly powerful if you combine it with functions.

wiid %>%
  split(.$region_un) %>%
  map(basic)

## All 'map()' functions either accept functions, formulas, or a character vector
## (used to extract components by name), or a numeric vector (used to extract by position).

wiid %>%
  split(.$region_un) %>%
  map(~ lm(gini ~ gdp, data = .)) %>%
  map(summary)

## Various Geoms

## Waffle Plot - Alternative To Pie Chart

## A waffle plot serves as a strong alternative to polar coordinates, as it lacks
## the perceptual issues of angles.

library(waffle)

data("diamonds")
glimpse(diamonds)

## Set up basically as if you are about to make a pie chart.

## We skip the 'waffle_iron()' function, which does work for you.

## What are the aesthetics needed for a waffle plot? 'x' and 'y' represent the
## location of each square; (0, 0) the bottom left. The fill aesthetic is for
## the grouping variable.

diamonds %>%
  group_by(cut) %>%
  summarize(nn = n()) %>%
  mutate(perc = round((nn / sum(nn)) * 100)) %>%
  uncount(perc) %>%
  mutate(x = rep(1:10, each = 10),
         y = rep(1:10, 10)) %>%
  ggplot(aes(x, y, fill = cut)) +
  geom_waffle(size = 1) +
  labs(fill = "Cut") +
  theme_void() +
  theme(legend.position = "top")

## Ridgeline Plot - A different way to look at density plots.

library(ggridges)

## Density Plot

wiid %>%
  ggplot(aes(x = gini)) +
  geom_density(alpha = 0.3) +
  labs(x = "Gini Index Density", y = "UN Region", fill = "UN Region") +
  theme_minimal()

## Overlapping Density Plots

wiid %>%
  ggplot(aes(x = gini, fill = region_un)) +
  geom_density(alpha = 0.3) +
  labs(x = "Gini Index Density", y = "UN Region", fill = "UN Region") +
  theme_minimal()

## Ridgeline Plot
## A vast improvement, in many cases. The degree of overlap can be controlled with
## the scale parameter.

wiid %>%
  ggplot(aes(x = gini, y = region_un, fill = region_un)) +
  geom_density_ridges(alpha = 0.4, scale = 2) +
  labs(x = "Gini Index Density", y = "UN Region", fill = "UN Region") +
  theme_minimal() +
  theme(legend.position = "none")












library(devtools)


install_github("https://github.com/liamgilbey/ggwaffle")

library(ggwaffle)

library(hrbrthemes) # One way to change to some custom themes. 



devtools::install_github
library(ggwaffle)


##########

# What is a functions, and how do we create our own? 
# A function is essentially a wrapper for other code. 
# Maybe a bundle of functions you do often. 

# This is the essence of a function.
# Arguments go in the parentheses. 
# Code/functions go inside the brackets. 

easy_math <- function(x, y){
  stuff <- x + y^2
  print(stuff)
}

# Like other functions, I can reverse arguments if I name them explicitly.

easy_math(4, 8)
easy_math(y = 4, x = 8)

# You can also write functions that do not require an argument. 

lettersampler <- function(){
  letz <- sample(letters, 40, replace = TRUE)
  LETZ <- sample(LETTERS, 40, replace = TRUE)
  print(str_c(letz, LETZ))
}

# Note: functions require a print/return.
## Otherwise, you'd not see the results of the function. 

lettersampler()

##############################################

## Looking up objects and their defined values. 
# R can look a level up; all objects don't have to be defined inside function.

# In this first case, all objects are defined within the function. 

f <- function(){
  x <- 1
  y <- 2
  c(x,y)
}
f() # See result.
rm(f) # rm is for 'remove'. 

#  A function looking one layer up, meaning outside the function. 

x <- 99988
ww <- 9843
g <- function(){
  y <- 2
  print(c(x, y))
}
g() # See result. 
rm(g) # Remove object. 

##########
### For loops. 

## commands inside loops: for, while, repeat, break, next. Also if. 

### Example 1. 

x <- 1:10
z <- matrix(rep(NA, 10), ncol = 2)

x; z

# seq_along() is not the ONLY way to set up.  
# Check against length() and note the difference. 

for(i in seq_along(x)){   # Instead of seq_along, can be length as well.
  z[i] <- i^2
  print(z)
} 

# Length can produce funky errors if x happens to be of length zero. 
# Just use seq_along. 

for(i in length(x)){  
  z[i] <- i^2
  print(z)
} 

z; class(z) # Numeric vector with all 10 values. 

### Example 2. A for loop inside and outside a function. What's the difference?

x <- c(2,5,3,9,8,11,6)
count <- 0

for (val in x) {
  if(val %% 2 == 0)  count = count+1
}
print(count)

# Note that count changes every time you keep running the for loop. 

# Note: you'll get a different result if you stuff all that in a function. 
# The 'fresh start' principle of functions. 

count_func <- function(x){
  count <- 0
  for (val in x) {
    if(val %% 2 == 0)  count = count+1
  }
  print(count)
}

count_func(x)

# Note: now count_func always the same value. 
# Create a vector filled with random normal values

## Coding your own dice game or deck of cards!
#
## Simple function rolling two dice. Note: NO arguments defined. 

roll <- function(){
  die <- 1:6
  dice <- sample(die, 2, replace = TRUE)
  sum(dice) # Sum also produces a printed output. 
  output <- list(dice = dice, sumdice = sum(dice))
  print(output)
}

roll() # You need the parentheses to activate the function and run it. 

## Rewrite the above function, but add an argument. 
## Now it doesn't have to be six-sided dice. 

roll2 <- function(bones){
  dice <- sample(bones, 2, replace = TRUE)
  print(dice) # Need to print the results you want to see!
  sum(dice) # Sum also produces a printed output. 
}
roll2(bones = 1:10) 

## Deck of Cards
# Start with some vectors. Card values and suits. 

cards <- c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace")
suit <- c("spades", "diamonds", "hearts", "clubs")
value <- c(13:1)
rep(cards, 4) # Replicate/repeat card values 4 times. 
rep(suit, each = 13) # Replicate/repeat suits 13 times. 
deck <- data.frame(cbind(card = rep(cards, 4), 
                         suit = rep(suit, each = 13), 
                         value = rep(value, 4)));  deck
colnames(deck) <- c("card", "suit", "value"); deck

# Sampling or shuffling. 

draw <- sample_n(deck, 5) # dplyr function for sampling rows/observations. 
draw
draw[draw$card == "eight",] # pulls out 8 of diamonds, for example. 


#######################################################################
# Functional programming with apply-like functions from purrr. 

data(mtcars) # Example data. 

# Mapping from purrr.
# purrr maps apply functions to lists or atomic vectors. 

mtcars2 <- mtcars # Making a copy. 
glimpse(mtcars)

# Below the map function is used on data frames, which are also lists.  

head(mtcars)

# Using map to iterate through the variables to see their class. 

map(mtcars, is.numeric)

# The following works well because all variables start as numeric in mtcars. 

mtcars %>%
  map(., quantile) # Give 0-25-50-75-100 percentile.  

# The ending for the map function is based on what the output should be. 

mtcars %>%
  group_split(am) %>% # Splitting into two data frames by automatic v. manual. 
  map(., `[`, "cyl") # Subsets, keeping only the cylinder variable. 

mtcars %>%
  group_split(am) %>%
  map_dfr(., `[`, "cyl") # Pulls them back together into one data frame. 

# Replace above with your own function!

# Write your own quantile function! Then apply it with map() function. 

quant2 <- function(x){
  quantile(x, probs = c(.025, .5, .975))
}

head(mtcars)

confind_cars <- mtcars %>%
  map_dfr(quant2) # Your own custom function can be applied. Save to data frame. 

confind_cars

##########
# Apply functions are equivalent to map functions in most ways. 

sapply(mtcars, mean) #Vector-based. Returns a vector. 
lapply(mtcars, mean) # produces a list. 
mapply(rep, 1:3, 3:1) # sapply with multiple elements. 

# Continued use of map. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared") # type of map shows what type of object it will return.

# Let's break it down bit by bit. At least the first part. 

# Split dataset into a list. 

mtcars %>%
  group_split(cyl)

# Use map to apply a function. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) # Apply the linear regression function to a list. 

# You can then continue to apply functions. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) 

# You can keep adding functions to apply. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared") 

# And so on and so on...

################################################################################










#######


# The violin plot. A cousin to the box plot AND the density plot. 

wiid %>%
  ggplot(aes(x = region_un, y = gini, fill = region_un)) +
  #geom_jitter(alpha = 0.2, aes(color = region_un), size = 0.2) +
  #geom_boxplot(alpha = 0.4) +
  geom_violin() +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip()

# Compare the violin plot to the boxplot, for example. Or combine!

wiid %>%
  ggplot(aes(x = region_un, y = gini, fill = region_un)) +
  geom_jitter(alpha = 0.2, aes(color = region_un), size = 0.2) +
  geom_boxplot(alpha = 0.4) +
  geom_violin(alpha = 0.2) +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip()

# The stream plot. A nice overarching plot, when useful. 

library(ggstream)
library(readxl)
  
moviedat <- ggstream::blockbusters
?blockbusters # Check the data. 

# What do we gain? What do we lost in this type of visualization?

# Stream plots have a few options: ridge area plot. symmetrical area, and proportional. 
# Default is symmetry. 

blockbusters

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream() +
  theme_minimal()

# There are no negative box office numbers, so what do we do with the centered 0?

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream(type = "ridge") + # The area or ridge very 
  theme_minimal()

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream(type = "proportional") +
  theme_minimal()

# Stream plots are variations on smoother lines. # The span is now the 'bandwidth'. 
# Compare bandwidths of .3 and .8, for example. 

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream(bw = .3) +
  theme_minimal()

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream(bw = .8) +
  theme_minimal()

# Some interesting stacking and sorting options exist for different types of data. 

ggplot(blockbusters, aes(year, box_office, fill = genre)) +
  geom_stream(bw = .8, sorting = "inside_out") +
  theme_minimal()

