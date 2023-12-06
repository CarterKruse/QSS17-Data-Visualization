## Data Visualization (GOVT16-QSS17) Fall 2022
## Web (Data) Scraping ('rvest')
##
## Professor Robert A. Cooper
## Week 9

library(tidyverse)
library(rvest)
library(selectr) # Translating 'CSS' to 'XPATH'.

## API (Application Programming Interface) vs. No API

## HTML - We are searching the HTML for the data.
## CSS vs. XPATH - Two different pattern/style searching ways to locate the data
## in the HTML.

## XPATH is often quite finicky. CSS is more generic and a bit easier.
## XPATH tends to be inconsistent, but it is good at searching for specific content.

## XPATH - Stands for XML path. XML is a query language, used for defining
## procedures for getting information from web sites or info systems.

## CSS - 'Cascading Style Sheets' - More general and do not search for specific content.
## CSS looks for an HTML style or general class. When in doubt, use CSS.

## Article - https://medium.com/dataflow-kit/css-selectors-vs-xpath-f368b431c9dc

ivy <- read_html("https://en.wikipedia.org/wiki/Ivy_League")

?html_nodes

# Ivy League Problem
# install.packages("pageviews")
# library(pageviews)

# ivy_pageviews <- article_pageviews(project = "en.wikipedia", article = "Ivy League",
#                                    end = "2018010101")
# str(ivy_pageviews); ivy_pageviews$views

ivy_tab <- ivy %>%
  html_nodes(css = "table") %>%
  html_table(fill = TRUE) # Actually saved as a list.

class(ivy_tab)
ivy_tab[2]

# XPATH equivalent to data in a table is 'td' or table data in HTML.

ivy_dat <- data.frame(ivy_tab[[2]]); ivy_dat

class(ivy_dat); glimpse(ivy_dat)

ivy_tab[[2]][2] # Location
ivy_tab[[2]][1] # Institution
ivy_tab[[2]][3] # Nickname
ivy_tab[[2]][4] # Undergrad Enrollment


# To find a good CSS path, SelectorGadget can be very useful to you. 
# SelectorGadget goes in your Bookmarks toolbar in your web browser. 

# When it comes to scraping, we will generally boil it down to three types
# of html elements: tables, text, and attributes.
# Tables and text are generally self-explanatory.
# Attributes are other data, or metadata, required for the construction of the web page. 

# To convert from CSS to XPATH, you can use selectr.

selectr::css_to_xpath("td", translator = "html")


#####
## Example: NBA Standings. 

###############################################################################
###############
## NBA. 

library(rvest)
library(ggrepel)
library(gridExtra)

team_stand <- read_html("https://www.espn.com/nba/standings") 
team_stand

team_st <- team_stand %>% 
  html_nodes(css = "table") %>%
  html_table()

team_st
class(team_st)
length(team_st)

# Explore the html components if necessary. 

team_stand %>%
  html_elements("section")

# Use html_node(s)  to locate the information.
# Use table/text/attrs to extract and store the info. 

team_st <- team_stand %>% 
  html_nodes(css = "table") %>%
  html_table()

# How does it differ if you use html_node? Brings back the first node/element in list. 

team_stand %>% 
  html_node(css = "table") %>%
  html_table()

team_st

# Produces a list of 4 tables. 

east1 <- team_st[[1]] 
east2 <- team_st[[2]]

east1
east2

# Get the two conferences together. 
east
east <- cbind(east1, east2)
west <- cbind(team_st[[3]], team_st[[4]])


west

# Combine. 

allteam <- rbind(east, west); allteam

# NBA Plots. 

plot1 <- allteam %>%
  ggplot(., aes(x = PPG, y = PCT, size = DIFF)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = "Points Per Game Scored", y = "Win Percentage",
       title = "Does Great Offense Win More Games in the NBA?") +
  theme_minimal() +
  theme(legend.position = "none")

plot2 <- allteam %>%
  ggplot(., aes(x = `OPP PPG`, y = PCT, size = DIFF)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = "Points Per Game Allowed", y = "Win Percentage",
       title = "Or Does Great Defense?",
       caption = "Source: https://www.espn.com/nba/stats") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(plot1, plot2, ncol = 2)

##### Example 2. Fortune 500.


fort <-  "https://en.wikipedia.org/wiki/Fortune_Global_500"
f500 <- read_html(fort)

ftab <- f500 %>%
  html_nodes("table") %>%
  html_table()

ftab # Guest what it is? A list!

fdat <- ftab[[1]]; fdat

###############################################################################
## Another, slightly more difficult example. 

eos <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders"
eos_html <- read_html(eos)

#  What's the difference between html_node and html_nodes?

eos_dat1 <- eos_html %>%
  html_node(".field-title a") %>%
  html_text()

eos_dat2 <- eos_html %>%
  html_nodes(".field-title a") %>%
  html_text()

eos_dat1
eos_dat2

# If time permits, we will practice making a plot from these data. 

################################################################################
# Executive orders scrape. American Presidency Project. 

library(tidyverse)
library(rvest)

# All the pages. 

paste0("a", "b")
paste0("b", 4)

urls <- paste0("https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?items_per_page=60&page=", 0:99)

urls

#urls60_1 <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?items_per_page=60&page=1"
#urls60_2 <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?items_per_page=60&page=2"
#urls60_0 <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders?items_per_page=60&page=0"

# One page. Just 10 results. Default. 

urlzz <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/executive-orders"

# Investigate the page! Use something like selectorgadget. 

# This first bit will get you the date.  

urlzz %>%
  read_html() %>%
  html_nodes(".date-display-single") %>%
  html_text()

# NOW WE NEED IT TO ITERATE. This time we will practice with lapply. 

length(urls)

orderz_df <- lapply(1:length(urls), function(i){
  
  print(paste0("Gathering ", i, " of ", length(urls)))
  
  pagez <- read_html(urls[i])
  
  date <- pagez %>%
    html_nodes(".date-display-single") %>%
    html_text() %>%
    trimws()
  
  return(date) # Stores the values in the object. Doesn't print, and that's okay!
  
}) %>%
  map_dfr(as.data.frame) %>%
  rename(order_date = `.x[[i]]`)

head(orderz_df) 
glimpse(orderz_df)

library(gganimate)

orderz_df %>%
  mutate(year = str_extract(order_date, "[:digit:][:digit:][:digit:][:digit:]"),
         year2 = as.numeric(year)) %>%
  count(year2) %>%
  ggplot(aes(x = year2, y = n)) +
  geom_line() +
  geom_point() +
  transition_reveal(year2) +
  labs(x = "Year", 
       y = "Number of Orders",
       title = "Presidential Executive Orders over Time") +
  theme_minimal()

######

# The following is an example of how to use the html_attr function to retrieve attributes. 

fort500 <- read_html("https://en.wikipedia.org/wiki/Fortune_Global_500")

# The Global Fortune 500 page has flag icons next to countries of origin. 

fort500 %>%
  html_nodes(".thumbborder") %>%
  html_attr("src")

# The next step is to use the source attribute to get the flag images. 
# I suggest using magick!

