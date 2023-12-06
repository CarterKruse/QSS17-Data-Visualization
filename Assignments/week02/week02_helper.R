## Data Visualization (GOVT16-QSS17) Fall 2022
## Tidyverse & Basic Plotting
##
## Professor Robert A. Cooper
## Week 2

## Don't forget to load your packages!
library(tidyverse)

## Initially, we did no plotting, which we will now introduce.
## To plot, we will use 'ggplot2' and 'tidyverse'.

## In this section, we will introduce a few more pieces of code for data transformation.
## Terms: aesthetics, mapping, themes, fill v. color, histograms, bar plots, line plots, etc. 

## Loading the 'mtcars' data, which is in base R.
data("mtcars")
head(mtcars)

## Code - The Pipe Operator
## Piping with 'magrittr' (part of the 'tidyverse') replaces 'turducken-style'
## coding by avoiding nested functions.

summarize(group_by(mutate(mtcars, newwt = wt * 1000, factcyl = factor(cyl)), factcyl), mean_wt = mean(wt))

mtcars %>%
  mutate(newwt = wt * 1000,
         factcyl = factor(cyl)) %>%
  group_by(factcyl) %>%
  summarize(mean_wt = mean(wt))

## The difference in the options above is pretty clear.

## Tibbles - Just a different way to present a data frame or data table.

## Comparing 'mtcars' to 'car_tibble' to see the difference. 
car_tibble <- as_tibble(mtcars); car_tibble
mtcars
car_tibble

##  Filtering - How do I know the inputs for the filter are working??

mtcars %>% # The '%>%' is the 'pipe' operator. 
  filter(cyl == 6) # Subset the data to cars with 6 cylinders only.

mtcars %>%
  filter(cyl == 6 & gear == 4) # Subset the data by 2 variables instead of 1.

mtcars %>%
  filter(wt > 2.8)

## Arranging - This just changes the view of the data. 

mtcars %>%
  arrange(wt) # Arrange data in ascending order of weight in tons. 

mtcars %>%
  arrange(desc(wt))

## Mutate - How you create a new variable in the 'tidyverse'.

## Mutate attaches the new variable to the end of the data frame/tibble. You must
## name the variable, and then define it.
mtcars %>%
  mutate(mean_weight = mean(wt), # New variable created with mean weight of all cars.
         fact_cyl = factor(cyl)) # New variable created turning 'cyl' into a factor variable. 

## Note: The object above was not saved, thus, the code is temporary and unsaved.
## The new variables will not be included when opening 'mtcars' again.

mtcars2 <- mtcars %>%
  mutate(mean_weight = mean(wt),
         fact_cyl = factor(cyl))

mtcars; mtcars2

## Select

## Select subsets data by choosing columns to keep/discard. It works with 'tidyselect'
## functions, in addition to variable names. Select can be used to rearrange variables.

mtcars2 %>%
  select(fact_cyl, wt, mpg) # Just sub-setting by these variables.

## 'tidyselect' Helpers
## Similar to a 'stringr' for variable names.

mtcars2 %>%
  select(contains("cyl")) # Sub-setting using a 'tidyselect' helper.

mtcars2 %>%
  select(starts_with("m"))

## We can use the 'tidyselect' helper everything() with select to re-arrange the data frame.
## Tell it which goes first, followed by everything. 

mtcars2 %>%
  select(wt, mpg, fact_cyl, mean_weight, everything())

## Group By

## 'group_by' sets a quiet rule... only a tibble will show you if a grouping
## rule has been set. Otherwise, you have to follow it with a 'summarize' or
## 'mutate' to see it work.

mtcars2 %>%
  group_by(fact_cyl)

## Without grouping, there is no line at the top identifying the number of groups.
mtcars2 %>% 
  tibble()

## 'group_by' will produce different results, depending on 'mutate' vs. 'summarize'.
mtcars2 %>%
  group_by(fact_cyl) %>%
  mutate(mean_mpg = mean(mpg, na.rm = TRUE))

mtcars2 %>%
  group_by(fact_cyl) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE))

## 'group_by' can group on multiple variables.
mtcars2 %>%
  group_by(fact_cyl, am) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE))

## Summarize

## 'summarize' creates new variables, yet it kicks you out of the data frame/tibble.
## It creates a new tibble altogether.
## What does 'summarize' bring forward into the new tibble?
## Only (1) the variables you group on and (2) the new variables you create.

mtcars2 %>%
  group_by(fact_cyl, am) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE),
            max_mpg = max(mpg, na.rm = TRUE),
            min_mpg = min(mpg, na.rm = TRUE))

## Plotting & ggplot2

## ggplot2 works differently from base R. The 'aesthetic' is the most important
## thing to know, followed by the 'geom'.

## The aesthetic connects the variation in your variables to your plot. It 'maps'
## a variable onto some element of your plot: size, color, line, etc.

## If you make an arbitrary choice in a plot (i.e. line color = red), then that
## does not go inside the aesthetic. All aesthetics are tied to variables.

## The geoms are all the geometric/physical ways our data can be expressed.
## The list of possible geoms expands with each package added. In this course,
## we will focus on a subset of these geoms.

## https://ggplot2.tidyverse.org/reference/

## Example 1: Simple (scatter) plot of car weight against fuel efficiency.
## Many features are not yet discussed. In this workshop, we start simple, unadorned
## and slowly add features.

glimpse(mtcars)

mtcars %>%
  ggplot(., aes(x = wt, y = mpg)) + # The period is used as a placeholder.
  geom_point()

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Weight by Fuel Efficiency", x = "Weight", y = "Miles Per Gallon") +
  theme_minimal()

## Bar Plot - Count up the number of cars with 4, 6, and 8 cylinders.
## 'geom_bar' does the count automatically.

mtcars %>%
  ggplot(aes(x = cyl)) +
  geom_bar()

## The x-axis looks a little odd. Why do you think that is?

glimpse(mtcars)

## This is the first real lesson in object classes! How do we fix this?

mtcars %>%
  ggplot(aes(x = factor(cyl))) +
  geom_bar()

## Line Plot

## For these to be the most useful, sometimes we might want to add a third aesthetic.
## The following example looks odd, on purpose.
mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_line()

## Let's go back to a scatter plot, keeping the third aesthetic.
## This is probably the most sensible format for visualization.

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point()

## 'Faceting' - We could break up the plot into multiple windows.

## There is little difference between 'facet_grid' and 'facet_wrap', which you
## will not really see until you have more categories.

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  facet_grid(cols = vars(cyl)) 

## You can facet by multiple variables, though use caution if you go beyond two!

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  facet_grid(cols = vars(cyl), rows = vars(am)) 

## Histograms/Frequency Polygons

## Characterizing individual continuous variables in buckets/bins.
## The plot below will be more appropriate with larger data sets.

mtcars %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(bins = 10) 

## Geoms
## There are a wide variety of geoms, though many are not covered here.

df1 <- data.frame(x = c(1, 7, 3),
                  y = c(13, 4, 17),
                  label = c("a", "b", "c"))

## Different Geoms, Same Data
## Exercise: Turn a geom on, and comment out the others.

## There is a subtle difference between 'geom_path' and 'geom_line'.
## 'geom_line' connects in the order of the x-axis values, while 'geom_path'
## connects in the order of the data.

df1 %>%
  ggplot(aes(x, y, label = label)) +
  # geom_point() # +
  # geom_text() # +
  # geom_area() # +
  geom_path() # +
  # geom_line() # +
  # geom_step() # +
  # geom_polygon()

## Data Analysis & Uncertainty
## When producing a point estimate for data analysis, you produce an estimate
## of uncertainty. To make a nice plot of these, you need 4 pieces of information.
## Geoms: 'pointrange', 'errorbar', 'linerange', etc.

df2 <- data.frame(x = c(1, 2, 3),
                  y = c(10, 4, 7),
                  ylow = c(9.5, 3.5, 6.5),
                  yhigh = c(10.5, 4.5, 7.5),
                  label = c("a", "b", "c"))

ggplot(df2, aes(x = x, y = y, ymin = ylow, ymax = yhigh)) +
  # geom_linerange()
  geom_pointrange() 
  # geom_errorbar()

## Filtering - String Variables

## You can filter with 'stringr' functions, so long as the input with 'filter' is a logical.
## Thus, 'filter' works with 'str_detect'.

## data %>%
##  filter(str_detect(variable, "pattern"))

## You cannot, for example, subset between two patterns. For that, you can use
## base R sub-setting (brackets with colon), but that does not work with the
## 'filter'/'str_detect' combination.

## Instead, you would use row/index number. For that, you would use something
## like 'slice()' from dplyr, paired with a 'stringr' function that returns the
## index/row number.

## 'str_which' is a dplyr version of 'which'.

data("mtcars")

mtcars2 <- mtcars %>%
  mutate(carmodel = rownames(mtcars))

mtcars2

## Now we can practice a little with 'stringr' functions on data.
## Many 'stringr' functions operate on a single variable/column. These do not
## stand along as functions to pipe direct into. In this case, you want to mutate
## and use 'str_...' to define the new variable.

## The following does not work.
mtcars2 %>%
  str_which(carmodel, "Toyota")

## The following does work, oddly (with a warning), but on ALL variables. 
mtcars2 %>%
  str_replace_all("Toyota", "giraffe")

## The following is the intended format.
mtcars2 %>%
  mutate(giraffe = str_replace_all(carmodel, "Toyota", "giraffe"))

## There are two ways to filter on a 'str_detect' function.

mtcars2 %>%
  mutate(toyotaz = str_detect(carmodel, "Toyota")) %>%
  filter(toyotaz) # If you do not provide a rule, filter will keep TRUEs. 

mtcars2 %>%
  filter(str_detect(carmodel, "Toyota"))

## Retrieving the index/row number instead of TRUE/FALSE, we use 'str_which'.
str_which(mtcars2$carmodel, "Toyota")

## This does not work. Why?
mtcars2 %>%
  mutate(rowz = str_which(carmodel, "Toyota"))

## The best match with 'str_which' is the 'slice' function, which uses index numbers,
## not TRUE/FALSE.
mtcars2 %>%
  slice(str_which(carmodel, "Toyota"))

## Using the 'str_c' function.

letters # Base R vector of lower-case letters.
LETTERS # Base R vector of upper-case letters. 

## The 'str_c' functions acts like a paste function. 
combo <- str_c(letters, LETTERS); combo
combo2 <- str_c(letters, LETTERS, collapse = " "); combo2

## The above may be used to make data scraping easier.
## Let's say there is data in 7 HTML pages.
fakeurl <- "www.coolwebsite.cooldata/yada/p" 
1:7

## When using the ':' sequence operator correctly, you can create a sequence of strings.
urlz <- str_c(fakeurl, 1:600)
urlz

## The 'stringr' functions are very sensitive, so the pattern matching must be
## explicit and exact (assuming your rule is so).

mtcars2

copycars <- mtcars2 
copycars[21, "carmodel"] <- "toyota"

copycars

copycars %>%
  filter(str_detect(carmodel, "toyota")) # Only keeps lower case.

copycars %>%
  filter(str_detect(carmodel, "Toyota")) # Only keeps upper case. 

copycars %>%
  filter(str_detect(carmodel, "[T|t]oyota")) # Keeps both! This is a regular expression.

## stringr Package - Filtering with string variables.

## Regular Expressions - Very powerful if you know how to use them.
## What are they? They are text strings that describe the patterns you are looking for.
## Certain characters require special 'escapes', as their normal characters
## have specific coding/programming functions.

## RStudio - Cheatsheet

## A vector of nonsense!
garbage <- as.character(c("jlfe8392rfd90TTFETD32d32de2",
                          "d89ufe4.;'..;'.];[]ifroi",
                          "w79432798432R2D289726923",
                          "fjf;adfeajfddara543[;;[afeafioewiohficpsrruleshdjhkaiou",
                          "u80fcdsoijcdskfds",
                          "w89hoihadwlfpodwop"))

glimpse(garbage)

nonsense <- data.frame(garb = garbage, stringsAsFactors = FALSE); nonsense

## Extract patterns from larger strings... one, or all within a string.
## Regular expressions allow you to find patterns that fit a rule without being
## too specific. Below, we are looking for all string patterns that contain two
## letters side by side.

nonsense %>%
  mutate(garb2 = str_extract_all(garb, "[:alpha:][:alpha:]"))

## You can look for a specific pattern.
nonsense %>%
  mutate(icpsr = str_detect(garb, "icpsr"))

nonsense %>%
  mutate(icpsr2 = str_extract_all(garb, "icpsr"))

## You can combine a specific pattern with a generic rule.
nonsense %>%
  mutate(icpsr3 = str_extract_all(garb, "icpsr[:alpha:][:alpha:][:alpha:][:alpha:][:alpha:]"))

## Removing various characters.
## An 'escape' is required for any character that has another meaning in code.
nonsense %>%
  mutate(clean = str_replace_all(garb, "\\;", "")) %>%
  glimpse()

nonsense %>%
  mutate(clean = str_replace_all(garb, "[\\;|\\.|\\[|\\]|\\']", ""))

## You can make a game out of it if you'd like. We can do a Madlibs style stringr function.
madlibs <- c("There once was a BLANK1 named BLANK2. She was very BLANK3 and enjoyed BLANK4 with her friend BLANK5 the BLANK6. Clearly, they would be friends forever.")

## Game 1
## A vector of words for the game.
name <- c("Thor", "Athena", "Snoopy", "Garfield", "Johnny")
noun <- c("monkey", "dog", "rabbit", "banana slug", "snowy owl")
verb <- c("skateboarding", "parasailing", "snorkeling", "sleeping")
adjective <- c("fluffy", "playful", "orange")

## Sampler - Just one way to do it.
## Hard brackets allow you to choose one of a number of patterns.
madlibs %>%
  str_replace("BLANK[1]", sample(noun, 1)) %>%
  str_replace("BLANK[3]", sample(adjective, 1)) %>%
  str_replace("BLANK[4]", sample(verb, 1)) %>%
  str_replace("BLANK[2]", sample(name, 1)) %>%
  str_replace("BLANK[5]", sample(name, 1)) %>%
  str_replace("BLANK[6]", sample(noun, 1))

## Movies
movies <- read_csv("/Users/carterkruse/Data Viz/movie_metadata.csv")

## An example using 'stringr' that detects all movies with 'Jurassic' in the title.
movies %>%
  mutate(dinosaur = str_detect(movie_title, "Jurassic")) %>%
  select(dinosaur, everything()) %>%
  count(dinosaur)

## The 'str_which' functions tells you which row/index number satisfies the condition.
## The 'slice' function keeps the rows indicated, using a row/index number.
movies %>%
  slice(str_which(movie_title, "Jurassic")) %>%
  select(movie_title, everything())

## Using 'stringr' with a regular expression.
movies %>%
  filter(str_detect(movie_title, "[:digit:]")) %>%
  select(movie_title, everything())
