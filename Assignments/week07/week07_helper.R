## Data Visualization (GOVT16-QSS17) Fall 2022
## Annimated Plots
##
## Professor Robert A. Cooper
## Week 9

library(tidyverse)
library(gganimate)
library(magick)
library(gifski)
library(readxl)

## Loading in the data set.

data("mtcars")
head(mtcars)

## TRANSITIONS
transition_states # Similar to faceting, needs to be categorical.
transition_reveal # Time-Based (Calculates Intermediary Values)
transition_time # Time-Based (Discrete Time Points)
transition_manual # Discrete States (With No Animation Allowed) 

## EASING
## How the transition is made, its velocity, shape, and speed.
## Does not apply to crossing discreet categories/observations.
## Options: 'bounce', 'circular', 'elastic', 'back', 'quadratic', 'cubic',
## 'quartic', 'sine', 'exponential'... + 'in', 'out', or 'in-out' (for forward,
## reverse, or split forward-reverse)

## ENTERS/EXITS
## How to enter and exit, there are many available.
## Options: 'shrink', 'fly', 'fade', 'drift', 'color', 'recolor'
## Each is as simple as an added line to the code.

## LABELING
## Options: 'previous', 'closest', 'next'
## In Braces: labs(title = "{closest_state}")
## Changes just a bit based on which kind of transition you are using.

## Group aesthetics are important! Think about a single point.
## Do not animate across observations.
## As long as there is an aesthetic set distinguishing groups, you will not run
## into a problem.

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = cyl_fact)) +
  geom_point() +
  theme(legend.position = "none") +
  transition_states(cyl_fact) +
  enter_fly(x_loc = -1) +
  enter_drift(x_mod = +5, y_mod = +20) +
  exit_shrink() +
  theme_minimal()

## Opening the WIDD data set and naming it 'wiid'.
wiid <- read_excel("/Users/carterkruse/Data Viz/WIID_31MAY2021/WIID_31MAY2021.xlsx")

## Looking at the data set.
glimpse(wiid)

## Using 'transition_reveal' for time (single point). A single country to start.

wiid %>%
  filter(country == "Finland", year > 1940) %>%
  group_by(year) %>%
  summarize(avg_gini = mean(gini, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_gini, color = year)) +
  geom_path() +
  geom_point() +
  labs(title = "Finland's Income Inequality") +
  transition_reveal(along = year) +
  theme_minimal()

## Using 'transition_states' by different countries. Adding a grouping aesthetic
## guarantees appropriate transitions.

wiid %>%
  filter(region_un_sub == "Southern Europe", year > 1940) %>%
  ggplot(aes(x = year, y = gini)) +
  geom_point() +
  transition_states(country) +
  ease_aes("cubic-in-out") +
  theme_minimal()

## A title indicates what each state represents. Animating titles depends on the 
## type of transition.

wiid %>%
  filter(region_un_sub == "Southern Europe", year > 1940) %>%
  ggplot(aes(x = year, y = gini, color = country)) +
  geom_point() +
  labs(title = "{closest_state}") +
  transition_states(country) +
  ease_aes("cubic-in-out") + # Negated by grouping variable.
  theme_minimal()

## Slowing Down Animation

wiid_anim <- wiid %>%
  filter(region_un_sub == "Southern Europe", year > 1940) %>%
  ggplot(aes(x = year, y = gini, color = country)) +
  geom_point() +
  transition_states(country, transition_length = 5, state_length = 10) +
  labs(title = "{closest_state}") +
  theme_minimal() +
  theme(legend.position = "none")

animate(wiid_anim, duration = 32, fps = 2)

anim_save("wiid1_anim.gif")

## Animated Rose Plot

data("diamonds")
glimpse(diamonds)

plot1 <- diamonds %>%
  group_by(cut, color) %>%
  summarize(avg_carat = mean(carat, na.rm = TRUE)) %>%
  ggplot(aes(x = color, y = avg_carat, fill = cut)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Color, Cut, & Carat: Diamonds",
       x = "Color (D (Best) to J (Worst))",
       y = "Average Carat",
       fill = "Cut") +
  coord_polar(theta = "x") +
  theme_minimal()

plot1

plot1 + transition_states(color)

## We can keep the states shown.

plot1 + transition_manual(color, cumulative = TRUE)

## 'transition_filter' (Highlighting Variation In A Different Way)

## We may have three variable dimensions on our 2D plot, but we can highlight the
## variation even better through an appropriate animation.

data("mtcars")
glimpse(mtcars)

## Let's look at the base plot.

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot() +
  geom_point(aes(x = wt, y = mpg, color = cyl_fact), size = 2.5)

## Now, the animated version

cars1 <- mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot() +
  geom_point(aes(x = wt, y = mpg, color = cyl_fact), size = 2.5) +
  labs(x = "Weight",
       y = "Fuel Efficiency (Miles Per Gallon)",
       title = "{closest_filter}-Weight Vehicles",
       subtitle = "{closest_expression} (Thousands of Pounds)",
       color = "Engine \n Cylinders") +
  scale_color_manual(values = c("#5878EA", "#F2711A", "#65B419")) +
  theme_minimal() +
  transition_filter(transition_length = 2, filter_length = 1,
                    Light = wt < 2.5, Medium = wt >= 2.5 & wt <= 3.5,
                    Heavy = wt > 3.5, keep = TRUE) +
  exit_recolor(color = "#B3B3B3") +
  exit_shrink(size = 0.4)

cars1

## GIFs (Using the 'magick' package.)

library(tidyverse)
library(gganimate)
library(magick)
library(gifski)
library(readxl)

img_doc <- "https://media.giphy.com/media/rULGb0wtaeAEM/giphy.gif" # Dr Emmett Brown. 
img_cat <-"https://media.giphy.com/media/mlvseq9yvZhba/giphy.gif"
img_bomb <- "https://media.giphy.com/media/wFmJu7354Csog/giphy.gif"
img_bron <- paste0("https://media3.giphy.com/media/3o72F9VC4WjFDCPTag/",
                   "giphy.gif?cid=ecf05e470ceb6cd8542e8c243d0e0a2282c3390e5c",
                   "72fd17&rid=giphy.gif")

bron <- image_read(img_bron)
bomb <- image_read(img_bomb)
cat <- image_read(img_cat)
doc <- image_read(img_doc)

## Show Image (Or Reverse)
doc
bomb
cat
bron

length(doc)
length(bron)

## Let's say we want to re-scale the size of the image. 

bomb2 <- image_scale(bomb, "490x280")
bomb2.1 <- image_scale(bomb, "x288") 

bomb2
bomb2.1

## Reversing GIFs ('rev(img)')

rev(1:10)
rev(bron)
rev(bomb)

## LeBron + Bomb
## Combining two GIFs back-to-back

dunkbomb <- c(bron, bomb2.1)
dunkbomb

smile <- image_read("/Users/carterkruse/Downloads/smile.png")

smile2 <- image_scale(smile, "50x50")

dunkbomb[30:40] <- image_composite(dunkbomb[30:40], smile2, offset = "+400+200")

dunkbomb

length(dunkbomb)

magick::magick_options()
magick::magick_config()

## Annotating GIFs

dunkbomb[30:34] <- image_annotate(dunkbomb[30:34], 
                                  "WOW", 
                                  size = 70,
                                  gravity = "southwest", 
                                  color = "green")

dunkbomb

## Dr. Emmett Brown + A-Bomb 

bomb_scaled <- image_scale(bomb, "x280")
doc_scaled <- image_scale(doc, "x280")

docbomb <- c(rep(doc_scaled, 2), bomb_scaled)
docbomb

## Save GIFs

image_write_gif(dunkbomb, "/Users/carterkruse/Desktop/dunkbomb.gif")

## There are many image transformations available.

cat

image_trim(cat, fuzz = 2)
image_scale(cat, "300")
image_charcoal(cat)

## GIFs are broken down by frame, so you can pause/reverse.

length(bron)
bron

## Combination of slowing down/reversing, etc.

bron[c(1:44, rep(c(rep(44, times = 30), 43:30, 
                   rep(30, times = 5), 31:43), 
                 times = 2), rev(1:44))]

## You can slow down the GIF.

bron[rep(1:44, each = 4)]
bron[c(1:30, rep(31:44, each = 3))]







# Lots of fun to be had here. 
# The LOGIC is the same with animated plots. 
# Functionally, they are essentially slide decks of images.

wiid_anim1 <- wiid %>%
  filter(year > 2000 & region_un_sub == "Northern America") %>%
  group_by(country, year) %>%
  summarize(mean_gini = mean(gini)) %>%
  ggplot(aes(x = year, y = mean_gini, color = country)) +
  geom_path() +
  geom_point(size = 0.5) +
  labs(x = "Year", y = "Gini Index") +
  theme(legend.position = "none") +
  transition_reveal(along = year) + 
  shadow_wake(wake_length = 0.2, size = 1, alpha = FALSE, colour = 'grey92') +
  theme_minimal()


# Animated plots are really just flipping through slide decks. 
# To begin, set up a normal plot. 

# Pay attention to which variable you want to animate on. 
# Almost always, it is either a grouping variable or your time variable(s).

# Key elements to an animated plot:
# 1: transitions. These are the most fundamental to an animation. 
# 2: views. These allow your plotting window to change along with the data. 
# 3: shadows. Do you want traces or memory? Shadows are where you create those. 
# 4: enters and exits: How should the data enter the plot and leave the plot? 
# 5: easings: the movement during the transition, defined by some function. 


# The first thing you need is a transition. 




# Then we feed it into the animate() function.

# Extra arguments determine the length of the animation and rendering. 

# Duration refers to total time to cycle. 
# nframes is the number of total frames (including, thus, copies)
# fps is the frames per second. 
# renderer is format. Default is gif. 

animate(wiid_anim1, nframes = 80, fps = 2, duration= 10) # too slow.

animate(wiid_anim1, nframes = 20, fps = 2, duration= 10) # about right.

animate(wiid_anim1, nframes = 20, fps = 2, duration= 20) # too slow. 

animate(wiid_anim1, nframes = 20, fps = 2, duration= 19) # too slow. 

animate(wiid_anim1, nframes = 20, fps = 2, duration= 11) # too slow. 

# Then we save the animated plot. 

# gifski might need to be installed. 

anim_save("wiid_anim1.gif")

##### Let's take some of our old examples and animate them. 

data('mtcars')
head(mtcars)

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  ggtitle('{closest_state}',
          subtitle = 'Frame {frame} of {nframes}') +
  
  # gganimate code starts here.
  
  transition_states(
    gear, # similar to a 'faceting' variable.
    transition_length = 2, # timing of transition.
    state_length = 1 # timing of state. 
  ) +
  enter_fly() + 
  exit_shrink() +
  ease_aes('sine-in-out') # sets the 'easing' (how to transition)


# There's something wrong with this plot, conceptually. Can you guess? 

# Technically speaking we shouldn't transition between different groups.
# Especially across units that cannot change from group to group.
# Can cars morph from one type of engine to another?
# Technically, yes. But

# Your title can animate along with your plot. 

ggplot(mtcars, aes(wt, mpg, group = gear)) + 
  geom_point() +
  ggtitle('{closest_state}',
          subtitle = 'Frame {frame} of {nframes}') +
  theme_minimal() +
  
  # gganimate code starts here.
  
  transition_states(
    gear, # similar to a 'faceting' variable.
    transition_length = 2, # timing of transition.
    state_length = 1 # timing of state. 
  ) +
  enter_fly(x_loc = -1) + 
  exit_shrink() 


### Some options work better than others depending on grouping/animating.
# You wouldn't really use an easing with discrete groups, for example. 

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = cyl_fact)) +
  geom_point() +
  theme(legend.position = "none") +
  transition_states(cyl_fact,
                    transition_length = 2,
                    state_length = 1) +
  # enter_fly(x_loc = -1) +
  # exit_shrink() +
  theme_minimal()

anim_save("mtcars_anim.gif")

# Or, if you want to render the animation in a different way. 
# Save the animation plot as an object; then animate and render it.
# You can 'render' the animation in different formats. movie, gif, etc. 

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = cyl_fact)) +
  geom_point() +
  theme(legend.position = "none") +
  transition_states(cyl_fact,
                    transition_length = 2,
                    state_length = 1) +
  #ease_aes("cubic-in-out") +
  #shadow_wake(wake_length = 0.2, size = 1, alpha = FALSE, colour = 'grey92') +
  #enter_fly(x_loc = -1) +
  #exit_shrink() +
  theme_minimal()


plot1dat <- wiid %>%
  filter(year > 1990) %>%
  mutate(population2 = population/1000000,
         log_gdppc = log(gdp)) %>%
  group_by(country, year, region_un, population2) %>%
  mutate(mean_gini = mean(gini, na.rm = TRUE),
         year2 = as.integer(year)) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE),
            mean_loggdp = mean(log_gdppc, na.rm = TRUE))

head(plot1dat)


plot1dat %>%
  ggplot(., aes(x = mean_loggdp, 
                mean_gini, 
                size = population2, 
                color = region_un, 
                group = country)) +
  geom_point(alpha = 0.7) +
  #scale_colour_manual(values = country_colors) +
  # scale_size(range = c(2, 12)) +
  # scale_x_continuous(trans = "log") +
  scale_y_continuous(limits = c(15, 65)) +
  # Here comes the gganimate specific bits
  #labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Gini') +
  #transition_states(year,
  #                  transition_length = 1,
  #                  state_length = 2) +
  # transition_reveal(along = year) +
  labs(title = 'Income Inequality and Per Capita Income in {frame_time}',
       y = "Gini Index", x = "Logged Per Capita Income",
       color = "UN Region", size = "Population \n in Millions") +
  # ease_aes('sine-in-out') +
  theme_minimal()

anim1 <- animate(p1,
                 duration = 100,
                 fps = 10)
anim1
anim_save("wiid_anim.gif", anim1)

magick::image_write(anim1, "anim1.gif")



## ANIMATING PLOTS USING THE MAGICK LIBRARY ONLY.

# In the event that gganimate is not working, magick can still animate your plot. 

library(magick)

# Work the data as needed. 

wiid2 <- wiid %>%
  filter(year >= 2000) %>%
  group_by(country, year) %>%
  summarize(mean_gini = mean(gini_reported))

# Create an 'empty' magick image.
img <- image_graph(600, 340, res = 96)

# Split the data by the animated variable (often time for example)
data_list <- split(wiid2, wiid2$year) # Makes separate lists by year.
data_list # Check out what split did.

# lapply takes a function and applies it to a list.
out <- lapply(data_list, function(data){
  p <- ggplot(data, aes(x = country, y = mean_gini)) +
    geom_point() + ylim(20, 90) +
    ggtitle(data$year) +
    theme_minimal()
  print(p)
})

dev.off() # Closes the current plotting environment.

animation <- image_animate(img, fps = 2) # Animates image.
image_write(animation, "animate.gif") # Saves animation to a file.

# image_write will save to your current directory if you do not specify otherwise. 






# GIFs using the magick package. 

# Magick. 

library(magick)

# First, you need to know import and export functions. 


monkeycat1 <- image_read(file.choose())
monkeycat2 <- image_read(file.choose())

monkeycat1
monkeycat2

# Working with images. Cropping. 

monkeycat1 %>%
  image_crop("250x250+330")

# You can scale down by pixels. 

image_scale(monkeycat1, "500")
?image_scale

# You can rotate. 

image_rotate(monkeycat1, 45)

# You can flip. 

image_flip(monkeycat1)


# We can manipulate the colors of our images through image_modulate.

image_modulate(monkeycat1, brightness = 150, saturation = 150, hue = 150)

# We can change base color with image_colorize.

image_colorize(monkeycat1, color ="blue", opacity = 50)


# You can fill certain parts of the image, if you know pixel location. 

image_fill(monkeycat1, "blue", point = "+500+360", fuzz = 20) 

# You can add text wherever you like. 

bluetail <- image_fill(monkeycat1, "blue", point = "+500+360", fuzz = 20) %>%
  image_annotate(., "Hey, why is my tail blue??", size = 30, color = "white", location = "+100+55")

bluetail

# Technically speaking, these image transformations are done in layers. 
# You can compress the layers into one with image_flatten.  

image_flatten(bluetail)

# Let's make a GIF. Save your basic image. 

mc1 <- monkeycat1

# You can repeat/replicate an image just like you can in other vectors, with rep(). 

mc_gif <- rep(mc1, 3)
length(mc_gif)

mc_gif

# You can recode/replace just like data vectors. 

mc_gif[2] <- bluetail

mc_gif

# Let's add one more. 

mc_gif2 <- c(mc_gif, rep(monkeycat2, 3))
mc_gif2

# To write/save your new GIFs, use the image_write function. 

image_write_gif(mc_gif, "monkeycat.gif")

