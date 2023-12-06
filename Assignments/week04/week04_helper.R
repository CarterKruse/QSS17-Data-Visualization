## Data Visualization (GOVT16-QSS17) Fall 2022
## Translating Data Work Into Visualizations
##
## Professor Robert A. Cooper
## Week 4

library(tidyverse)

## Join Functions
## Key to data management is the ability to join or merge data.

## There is a suite of 'join' functions in 'dplyr' for this purpose, which is more
## flexible than the Base R function ('merge').

## Functions: 'full_join', 'left_join', 'right_join', 'semi_join', 'anti_join'

a <- c(1, 43, 843, 34)
b <- c("Amy", "Javier", "Gustav", "Beatrice")
g <- c(4L, 3L, 8L, 12L)

d <- c(51, 82, 72, 349)
e <- c("Sandrine", "Ahmed", "Gustav", "Beatrice")
f <- c(24L, 53L, 83L, 1L)

y <- data.frame(a = a, b = b, g = g); y
z <- data.frame(d = d, e = e, f = f); z

full <- full_join(y, z, by = c("b" = "e"))
full

left <- left_join(y, z, by = c("b" = "e"))
left

right <- right_join(y, z, by = c("b" = "e"))
right

semi <- semi_join(y, z, by = c("b" = "e"))
semi

anti1 <- anti_join(y, z, by = c("b" = "e"))
anti1
anti2 <- anti_join(z, y, by = c("e" = "b"))
anti2

## This function clears the plotting environment.
dev.off()

## Join Functions

## Creating various vectors to be placed in data frames.
h <- sample(letters, 4, replace = TRUE)mapping_data %>%
  mutate(emissions = factor(emissions, levels = c("50+", "30-50", "25-30", "20-25", 
                                                  "15-20", "10-15", "5-10", "0-5"),
                            ordered = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = emissions), size = 0.2) + # ADD COLOR
  coord_sf(xlim = c(-180, -130), ylim = c(50, 70)) +
  scale_fill_manual(values = c("#300000", "#660000", "#9A0100", "#CD0000",
                               "#FF0000", "#FF6666", "#FFCDCD", "#FFFFFF")) +
  labs(fill = "Millions of metric tons") + 
  theme_minimal() +
  theme(legend.position = "none")
i <- sample(0:10, 4, replace = TRUE)
people1 <- c("Amy", "Javier", "Gustav", "Beatrice")

df1 <- data.frame(h = h, i = i, people = people1); df1

j <- sample(letters, 4, replace = TRUE)
k <- sample(110:310, 4, replace = TRUE)
people2 <- c("Sandrine", "Ahmed", "Gustav", "Beatrice")

df2 <- data.frame(j = j, k = k, people = people2); df2

df1; df2

## With 'full_join', all rows are kept and NAs fill non-matches.
## There may be duplicate rows.

full_join(df1, df2, by = c("people"))

## With 'inner_join', complete matches are returned, all else is dropped.
## This keeps rows in BOTH X and Y (data).

inner_join(df1, df2, by = c("people"))

## With 'left_join', left-side data rows are kept and right-side unique rows are dropped. 

left_join(df1, df2, by = c("people"))

## With 'right_join', right-side data rows are kept and left-side unique rows are dropped.

right_join(df1, df2, by = c("people"))

## Filtering Joins: 'semi_join' & 'anti_join'

## With 'anti_join', non-matches are returned. These non-matches are only one way.
## To reverse (return non-matches in Y), switch the order of the data frames.

anti_join(df1, df2, by = c("people"))
anti_join(df2, df1, by = c("people"))

## With 'semi_join', the join acts almost as a filter for left-hand data, returning
## only rows of X that have a match in Y (data).

semi_join(df1, df2, by = c("people"))
semi_join(df2, df1, by = c("people"))
















# The 'lubridate' Package

# The %in% function, case_when, and ifelse(). 
# Working with strings in stringr.
# Relational data and joining data with dplyr join functions. 


wiid %>%
  group_by(country, region_un, region_un_sub, year) 

# Working with lubridate. 

# lubridate is an excellent package for formatting dates. 

library(lubridate) # tidyverse, but not core tidyverse.
library(nycflights13) # for dates data. 

nycflights13::flights

glimpse(flights)

flights2 <- flights %>%
  mutate(date_new = make_date(year, month, day)) %>%
  select(date_new, everything()) 

# We can reverse this from our date_new variable. 

flights2 %>%
  mutate(dayz = day(date_new),
         monthz = month(date_new),
         yearz = year(date_new)) %>%
  select(dayz, monthz, yearz)

# Let's check the specific date-time of the flights. 
# We could also have parsed the date, month, day, or year from that variable. 

flights %>%
  mutate(dayz = day(time_hour),
         monthz = month(time_hour),
         yearz = year(time_hour),
         datez = date(time_hour)) %>%
  select(dayz, monthz, yearz, datez)

# A date-time variable counts in seconds since 1970-01-01. 
# If we want a date-time variable and we don't have one, then R will automatically
# fill each time. 

flights2 %>%
  mutate(date_time_new = )

# You can do some conversions with duration. 

duration(36, "minutes")

# Any time you want to check the date or time, you can. 

today()
now()

#####
## FILTERING

# Introducing the %in% vector. 
# subset vector. 

# %in%. Very useful in subsetting and filtering. Returns a logical. 

letter_vect <- c("zz", "hh", sample(letters, 3))
letter_vect

letters # base R letters vector.
letter_vect # our subset vector. 

letters_df <- data.frame(alphabet = letters)
letters_df

# small vector 'in' large vector.

letter_vect %in% letters

# large vector 'in' small vector. 

letters %in% letter_vect

letters_df %>%
  filter(alphabet %in% letter_vect)



# count function. Note: I said use the arrange function, but you don't have to!

movies %>%
  count(genres, color, sort = TRUE) 

# distinct function. This only shows the unique categories of a variable. 

movies %>%
  distinct(color)




# Recoding variables with ifelse and/or case_when. 
# case_when v. if/else. Conditional recoding. 

glimpse(movies)

# count function. Just for getting a sense of your data. 

movies %>%
  count(genres) 

movies %>%
  count(color, language)

movies %>%
  group_by(language) %>%
  summarize(n = sum(.)) # .groups message at the end. 

movies %>%
  count(language) %>%
  arrange(desc(n))

# distinct function. 

movies %>%
  distinct(language)











# The case_when function. A very flexible ifelse-like function. 

movies %>%
  mutate(big_time = ifelse(budget > mean(budget, na.rm=TRUE), "big time", "small fry")) %>%
  select(big_time, everything()) %>%
  count(big_time)

head(movies)


# case_when v. if/else. Conditional recoding. 

movies %>%
  mutate(big_time = case_when(
    budget > mean(budget, na.rm = TRUE) ~ "big time",
    budget <= mean(budget, na.rm = TRUE) ~ "small fry",
    is.na(budget) ~ "LIARS!"
  )) %>% 
  select(movie_title, big_time, budget) %>%
  count(big_time)

movies %>%
  mutate(big_time = case_when(
    budget > mean(budget, na.rm = TRUE) ~ "big time",
    budget <= mean(budget, na.rm = TRUE) ~ "small fry",
    is.na(budget) ~ "LIARS!"
  )) %>% 
  filter(big_time == "LIARS!") %>%
  dplyr::select(movie_title, big_time, budget)


# You can nest ifelse() functions when required.
# This is roughly equivalent code, but with ifelse(). 

movies %>%
  mutate(money = ifelse(budget > 50000000, "big time", 
                        ifelse(budget > 25000000 & budget <= 50000000, "medium", "small fry"))) %>%
  select(money, everything()) %>%
  count(money)


# This is equivalent code, but with ifelse(). 

movies %>%
  mutate(big_time = ifelse(budget > mean(budget, na.rm = TRUE), "big time", 
                           ifelse(budget <= mean(budget, na.rm = TRUE), "small fry",
                                  no = "NA"))) %>%
  select(big_time, everything()) %>%
  count(big_time)

movies %>%
  mutate(big_time = ifelse(budget > mean(budget, na.rm = TRUE), "big time", 
                           ifelse(budget <= mean(budget, na.rm = TRUE), "small fry",
                           ))) %>%
  select(big_time, everything())

