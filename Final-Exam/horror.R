## Data Visualization (GOVT16-QSS17) Fall 2022
## Final Exam - 3) Horror
##
## Name: Carter Kruse
## Date: November 20th, 2022

library(ggplot2)
library(tidyverse)
library(devtools)

library(rvest)
library(selectr)
library(magick)

library(stringr)
library(ggtext)

horror <- read_html("https://editorial.rottentomatoes.com/guide/best-horror-movies-of-all-time/")

poster <- horror %>% html_elements(".article_poster") %>% html_attr("src")
title <- horror %>% html_elements(".article_movie_title a") %>% html_text2()
year <- horror %>% html_elements(".start-year") %>% html_text2()
countdown <- horror %>% html_elements(".countdown-index") %>% html_text2()
critics <- horror %>% html_elements(".critics-consensus") %>% html_text2()

horror_table <- data.frame(title = title, year = year, countdown = countdown, critics = critics)
glimpse(horror_table)

top_5 <- horror_table %>%
  filter(as.numeric(str_sub(year, 2, -2)) < 1970) %>%
  tail(n = 5)

glimpse(top_5)

top_5$full_title = paste(top_5$countdown, top_5$title, top_5$year)
glimpse(top_5)

top_5$critics[[1]] <- paste0(str_sub(top_5$critics[[1]], 1, 98), "<br>", str_sub(top_5$critics[[1]], 99, -1))
top_5$critics[[2]] <- paste0(str_sub(top_5$critics[[2]], 1, 108), "<br>", str_sub(top_5$critics[[2]], 109, -1))
top_5$critics[[3]] <- paste0(str_sub(top_5$critics[[3]], 1, 80), "<br>", str_sub(top_5$critics[[3]], 81, -1))
top_5$critics[[4]] <- paste0(str_sub(top_5$critics[[4]], 1, 107), "<br>", str_sub(top_5$critics[[4]], 108, -1))
top_5$critics[[5]] <- paste0(str_sub(top_5$critics[[5]], 1, 105), "<br>", str_sub(top_5$critics[[5]], 106, -1))

glimpse(top_5)

start_x <- 1.1

plot <- ggplot(top_5) +
  xlim(0, 5) +
  ylim(0, 4.7)

for (i in 1:5) {
  plot <- plot +
    geom_richtext(x = start_x, y = i - 0.55, label = top_5[i,]$full_title,
                  hjust = 0, vjust = 0,
                  family = "sans", fontface = "bold", size = 2.5,
                  color = "#FFFFFF", fill = "#FFFFFF", text.color = "#000000",
                  label.padding = unit(c(0, 0, 0, 0), "lines"),
                  label.margin = unit(c(0, 0, 0, 0), "lines"),
                  label.r = unit(0, "lines")) +
    geom_richtext(x = start_x, y = i - 0.85, label = top_5[i,]$critics,
                  hjust = 0, vjust = 0,
                  family = "sans", fontface = "italic", size = 2,
                  color = "#FFFFFF", fill = "#FFFFFF", text.color = "#000000",
                  label.padding = unit(c(0, 0, 0, 0), "lines"),
                  label.margin = unit(c(0, 0, 0, 0), "lines"),
                  label.r = unit(0, "lines"))
}

plot <- plot +
  labs(title = "Top 5 Classic (Before 1970) Horror Movies of All Time (Rotten Tomatoes)",
       caption = "Source: Rotten Tomatoes (https://editorial.rottentomatoes.com/guide/best-horror-movies-of-all-time/)") +
  theme_void() +
  theme(plot.title = element_text(color = "#000000", size = 8, face = "italic"),
        plot.caption = element_text(color = "#000000", size = 5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.margin = margin(4, 4, 4, 4, "mm"))

plot

for (i in 1:5) {
  plot <- plot +
    annotation_raster(as.raster(image_fill(image_read(poster[[201 - as.numeric(str_sub(rev(top_5$countdown)[i], 2, -1))]]), "none")), 
                      xmin = 0, xmax = 0.6, ymin = 5.0 - i, ymax = 5.7 - i, interpolate = TRUE)
}

plot

ggsave(filename = "horror.png", plot = plot, width = 8, height = 6, scale = 1, units = "in")
ggsave(filename = "horror.pdf", plot = plot, width = 8, height = 6, scale = 1, units = "in")

