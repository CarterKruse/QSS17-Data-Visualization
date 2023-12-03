## Data Visualization (GOVT16-QSS17) Fall 2022
## Project 3 - Dartmouth Data
## Median Grades for Undergraduate Courses (20F - 22S)
##
## Name: Carter Kruse
## Date: November 18th, 2022

## Data
## Dartmouth Median Grades: https://www.dartmouth.edu/reg/transcript/medians/index.html
## Dartmouth Enrollment: https://www.dartmouth.edu/oir/data-reporting/factbook/studentcensus.html

## Packages

library(tidyverse)
library(devtools)

library(stringr)
library(rvest)

library(ggtext)
library(ggrepel)

library(gganimate)
library(magick)

## Using the 'rvest' package, including the functions 'read_html', 'html_nodes',
## and 'html_table' to scrape data.

## Sub-setting to remove the first row of data, which is not relevant.

html_table <- data.frame(read_html(str_c("https://www.dartmouth.edu/reg/transcript/medians/", "20f", ".html")) %>%
                           html_nodes(css = "table") %>%
                           html_table(fill = TRUE))[-1,]

html_table

## Creating a list of the academic terms used for the visualization.

term_list <- c("21w", "21s", "21x", "21f", "22w", "22s")

## Using a 'for' loop, along with 'rbind' to update the data table.

for (term in term_list) {
  html_table <- rbind(html_table, data.frame(read_html(str_c("https://www.dartmouth.edu/reg/transcript/medians/", term, ".html")) %>%
                                               html_nodes(css = "table") %>%
                                               html_table(fill = TRUE))[-1,])
}

## Using the 'glimpse' function to quickly view the data frame.

glimpse(html_table)

## Printing the 'typeof' and 'class' of the data frame

typeof(html_table)
class(html_table)

## Renaming the columns of the data frame.

colnames(html_table) <- c("TERM", "COURSE", "ENRL", "MEDIAN")

## Identifying the terms included in the data set using the 'distinct' function.

html_table %>%
  distinct(TERM)

glimpse(html_table)

## Creating a new data frame that transforms/analyzes the data appropriate for
## the visualization.

## Using 'mutate' to create new data frame variables.
## Using 'stringr' to extract the character code that corresponds to the academic department.
## Using 'as.numeric' to change the variable type from a string to a numeric.
## Using 'factor' to ensure the academic term is a categorical variable.
## Using 'case_when' to set a variable depending on a different column.

base_dartmouth_data <- html_table %>%
  mutate(DEPT = str_extract(COURSE, "[:upper:]+"),
         ENRL = as.numeric(ENRL),
         TERM = factor(TERM, ordered = TRUE,
                       levels = c("20F", "21W", "21S", "21X", "21F", "22W", "22S")),
         SCORE = case_when(
           MEDIAN == "A" ~ 12 / 3,
           MEDIAN == "A/A-" ~ 11.5 /3,
           MEDIAN == "A-" ~ 11 / 3,
           MEDIAN == "A-/B+" ~ 10.5 / 3,
           MEDIAN == "B+" ~ 10 / 3,
           MEDIAN == "B+/B" ~ 9.5 / 3,
           MEDIAN == "B" ~ 9 / 3,
           MEDIAN == "B/B-" ~ 8.5 / 3,
           MEDIAN == "B-" ~ 8 / 3,
           MEDIAN == "B-/C+" ~ 7.5 / 3,
           MEDIAN == "C+" ~ 7 / 3,
           MEDIAN == "C+/C" ~ 6.5 / 3,
           MEDIAN == "C" ~ 6 / 3,
           MEDIAN == "C/C-" ~ 5.5 / 3,
           MEDIAN == "C-" ~ 5 / 3,
           MEDIAN == "C-/D+" ~ 4.5 / 3,
           MEDIAN == "D+" ~ 4 / 3,
           MEDIAN == "D+/D" ~ 3.5 / 3,
           MEDIAN == "D" ~ 3 / 3,
           MEDIAN == "D/D-" ~ 2.5 / 3,
           MEDIAN == "D-" ~ 2 / 3
           ))

glimpse(base_dartmouth_data)

## Creating a new data frame that further transforms/analyzes the data appropriate
## for the visualization.

## Using 'group_by' to calculate statistics over groups.
## Using 'mutate' to create new data frame variables.
## Using 'case_when' to set variables depending on a different columns.

partial_dartmouth_data <- base_dartmouth_data %>%
  group_by(TERM, DEPT) %>%
  mutate(AVG_GPA = mean(SCORE, na.rm = TRUE),
         AVG_ENRL = mean(ENRL, na.rm = TRUE),
         CLASSES = n()) %>%
  ungroup() %>%
  mutate(FIELD = case_when(
    DEPT == "ASTR" ~ "STEM",
    DEPT == "BIOL" ~ "STEM",
    DEPT == "CHEM" ~ "STEM",
    DEPT == "COSC" ~ "STEM",
    DEPT == "ENGS" ~ "STEM",
    DEPT == "MATH" ~ "STEM",
    DEPT == "PHYS" ~ "STEM",
    DEPT == "ANTH" ~ "Social Sciences",
    DEPT == "COGS" ~ "Social Sciences",
    DEPT == "ECON" ~ "Social Sciences",
    DEPT == "EDUC" ~ "Social Sciences",
    DEPT == "ENVS" ~ "Social Sciences",
    DEPT == "GEOG" ~ "Social Sciences",
    DEPT == "GOVT" ~ "Social Sciences",
    DEPT == "LING" ~ "Social Sciences",
    DEPT == "QSS" ~ "Social Sciences",
    DEPT == "PBPL" ~ "Social Sciences",
    DEPT == "PSYC" ~ "Social Sciences",
    DEPT == "SOCY" ~ "Social Sciences",
    TRUE ~ "Humanities"
  ),
  TERM_NUMBER = case_when(
    TERM == "20F" ~ 1,
    TERM == "21W" ~ 2,
    TERM == "21S" ~ 3,
    TERM == "21X" ~ 4,
    TERM == "21F" ~ 5,
    TERM == "22W" ~ 6,
    TERM == "22S" ~ 7
  ))

glimpse(partial_dartmouth_data)

## Creating a new data frame that further transforms/analyzes the data appropriate
## for the visualization.

## Using 'select' to simplify the data frame.
## Using 'distinct' to remove duplicate rows of the data frame.

final_dartmouth_data <- partial_dartmouth_data %>%
  select(TERM, TERM_NUMBER, DEPT, FIELD, AVG_GPA, AVG_ENRL, CLASSES) %>%
  distinct()

glimpse(final_dartmouth_data)

## Using 'head'/'tail' and 'str' to quickly view the data set.

head(final_dartmouth_data)
tail(final_dartmouth_data)
str(final_dartmouth_data)

## Using 'summary' to quickly view summary statistics of the data set.

summary(final_dartmouth_data)

## Using the %in% operator to determine if a given value is in a column, to demonstrate
## its purpose.

145 %in% final_dartmouth_data$AVG_ENRL

## Animated Plot 1 - This plot demonstrates the relationship between average course median,
## average course enrollment, and number of courses offered per term. The animation
## occurs over a time frame from "20F" to "22S" (Dartmouth terms).

## Basic Plot - Creating a scatter plot of average course median versus average
## course enrollment, with a third dimension: number of courses offered per term.

## Using '?' to look up the documentation for 'geom_label_repel'.
?geom_label_repel

## Using 'filter' to remove courses with an average course enrollment greater than 100.
## Using 'mutate' to create labels for departments with at least 18 courses in a term.
## Using 'ifelse' to determine such qualifications.
## Using 'geom_hline' with 'linetype' to create horizontal dotted lines.
## Using 'size' to vary the size of the points by the number of courses offered per term.
## Using 'alpha' to set the transparency of the points.
## Using 'geom_label_repel' with 'min.segment.length = 0' to create labels.
## Using 'scale_x/y_continuous' to scale the x/y axis.

plot_1_base <- final_dartmouth_data %>%
  filter(AVG_ENRL <= 100) %>%
  mutate(LABEL = ifelse(CLASSES >= 18, DEPT, "")) %>%
  ggplot(aes(x = AVG_ENRL, y = AVG_GPA, color = FIELD, group = DEPT)) +
  geom_hline(aes(yintercept = 12 / 3), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  geom_hline(aes(yintercept = 11 / 3), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  geom_hline(aes(yintercept = 10 / 3), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  geom_hline(aes(yintercept = 9 / 3), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  geom_point(aes(size = CLASSES), alpha = 0.8) +
  geom_label_repel(aes(x = AVG_ENRL, y = AVG_GPA, label = LABEL), color = "#000000",
                   force = 10, size = 3, min.segment.length = 0, max.overlaps = Inf,
                   seed = 10, alpha = 0.8) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_y_continuous(limits = c(3, 4),
                     breaks = c(9 / 3, 10 / 3, 11 / 3, 12 / 3),
                     labels = c("B", "B+", "A-", "A")) +
  scale_color_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(3)) +
  labs(title = "Dartmouth Grades - Term: {closest_state}", 
       subtitle = "Academic Departments' Median Grades\n-- By Course Enrollment and Number of Courses Offered --",
       x = "Average Course Enrollment", y = "Average Median Grade",
       color = "Field (Department Type)", size = "Number of Courses Offered") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2)) +
  guides(color = "none") +
  transition_states(TERM)

# plot_1_base

# Customizing the theme of the plot.

plot_1_final <- plot_1_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(color = "#000000", size = 12, family = "sans", hjust = 0.5),
        plot.subtitle = element_text(color = "#000000", size = 10, family = "sans", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

# plot_1_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_1 <- animate(plot_1_final, width = 550, height = 400, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_1

## Animated Plot 2 - This plot demonstrates the number of courses offered per term
## by field (department type): Humanities, Social Sciences, STEM. The animation
## occurs over a time frame from "20F" to "22S" (Dartmouth terms).

## Basic Plot - Creating a pie chart of the number of courses offered per term
## by field.

## Using 'group_by' to calculate statistics over groups (term and field).
## Using 'summarize' to determine the total number of courses.
## Using 'arrange' with 'fct_reorder' to arrange the data high to low.
## Using 'mutate' to create new variables, which determine the position of labels.
## Using 'geom_col' with 'position = stack' as the base for the pie chart.
## Using 'geom_label_repel' with 'as.character' to create distinct (rounded) labels.
## Using 'coord_polar' to shift the bar chart to a pie chart.
## Using 'theme_void' to remove the axes.

plot_2_base <- final_dartmouth_data %>%
  group_by(TERM, FIELD) %>%
  summarize(TOTAL_CLASSES = sum(CLASSES, na.rm = TRUE)) %>%
  arrange(TERM, fct_reorder(FIELD, -TOTAL_CLASSES)) %>%
  mutate(CUMSUM = rev(cumsum(rev(TOTAL_CLASSES))),
         POSITION = TOTAL_CLASSES / 2 + lead(CUMSUM, 1),
         POSITION = if_else(is.na(POSITION), TOTAL_CLASSES / 2, POSITION)) %>%
  ggplot() +
  geom_col(aes(x = 0, y = TOTAL_CLASSES, fill = FIELD), position = "stack", width = 1) +
  geom_label_repel(aes(x = 0, y = POSITION, label = as.character(TOTAL_CLASSES)),
                   size = 3, color = "#000000", nudge_x = 0.8, max.overlaps = Inf) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(3)) +
  labs(title = "Number of Courses Offered by Field", fill = "Field") +
  theme_void() +
  transition_states(TERM)

# plot_2_base

# Customizing the theme of the plot.

plot_2_final <- plot_2_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(color = "#000000", size = 12, family = "sans", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        axis.title = element_blank(),
        axis.text = element_blank(),
        # 
        legend.background = element_rect(color = "#000000", fill = "#FFFFFF"),
        legend.position = "left",
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 10, family = "sans"),
        legend.text = element_text(color = "#000000", size = 9, family = "sans"),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(c(5, 5, 5, 5)))

# plot_2_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_2 <- animate(plot_2_final, width = 400, height = 400, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_2

## Creating a new data frame that analyzes the data appropriate for the
## visualization.

## The overall enrollment data (for each Dartmouth term) was taken directly from
## the published census. Thus, the following demonstrates the manual creation of
## a data frame.

base_enrollment_data <- data.frame(TERM = c("20F", "21W", "21S", "21X", "21F", "22W", "22S"),
                                   TERM_NUMBER = c(1, 2, 3, 4, 5, 6, 7),
                                   ENRL_TOTAL = c(4169, 3851, 3784, 1430, 4462, 4131,  4252))

## Using the 'glimpse' function to quickly view the data frame.

glimpse(base_enrollment_data)

## Creating a new data frame that transforms the data appropriate for the
## visualization.

## Using 'mutate' to create new data frame variables.
## Using 'factor' to ensure the academic term is a categorical variable (ordered).

final_enrollment_data <- base_enrollment_data %>%
  mutate(TERM = factor(TERM, ordered = TRUE,
                       levels = c("20F", "21W", "21S", "21X", "21F", "22W", "22S")))

## Using the 'glimpse' function to quickly view the data frame.

glimpse(final_enrollment_data)

## Stats Plot - Creating a scatter plot with a smoothing line of total undergraduate
## enrollment versus time (terms at Dartmouth). This is not used for plotting,
## rather for extracting the x, y values for the smoothed line.

plot_3_stats <- final_enrollment_data %>%
  ggplot(aes(x = TERM_NUMBER, y = ENRL_TOTAL)) +
  geom_line() +
  geom_smooth(span = 0.9)

## Creating a new data frame that includes the 'smoothed' x and y coordinates for
## the visualization.

## Using 'ggplot_build' to extract the data from the plot.

smooth_data <- ggplot_build(plot_3_stats)$data[[2]] %>%
  mutate(TERM_NUMBER = x,
         ENRL_TOTAL = y) %>%
  select(TERM_NUMBER, ENRL_TOTAL)

## Using the 'glimpse' function to quickly view the data frame.

glimpse(smooth_data)

## Animated Plot 3 - This plot demonstrates the total undergraduate enrollment
## at Dartmouth. The animation occurs over a time frame from "20F" to "22S"
## (Dartmouth terms).

## Basic Plot - Creating a line chart (with smoothing) of the total undergraduate
## enrollment at Dartmouth.

## Using 'geom_vline' with 'linetype' to create a vertical dotted line.
## Using 'scale_x/y_continuous' to scale the x/y axis.

plot_3_base <- final_enrollment_data %>%
  ggplot(aes(x = TERM_NUMBER, y = ENRL_TOTAL)) +
  geom_line(color = "#000000", size = 1) +
  geom_line(data = smooth_data, color = "930D0D", size = 1, alpha = 0.8) +
  geom_vline(aes(xintercept = TERM_NUMBER), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  scale_x_continuous(limits = c(1, 7),
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("20F", "21W", "21S", "21X", "21F", "22W", "22S")) +
  scale_y_continuous(limits = c(1000, 5000),
                     breaks = c(1000, 2000, 3000, 4000, 5000)) +
  labs(title = "Undergraduate Enrollment", 
       x = "Term", y = "Undergraduate Enrollment") +
  theme_classic() +
  theme(legend.position = "none") +
  transition_reveal(TERM_NUMBER)

# plot_3_base

# Customizing the theme of the plot.

plot_3_final <- plot_3_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(color = "#000000", size = 12, family = "sans", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_blank(),
        # 
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"))

# plot_3_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_3 <- animate(plot_3_final, width = 250, height = 250, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_3

## Animated Plot 4 - This plot demonstrates the average course median by field 
## over time. The animation occurs over a time frame from "20F" to "22S"
## (Dartmouth terms).

## Basic Plot - Creating a line chart of the average course median by field at Dartmouth.

## Using 'group_by' to calculate statistics over groups (term and field).
## Using 'summarize' to determine the average course median by field.
## Using 'geom_vline' with 'linetype' to create a vertical dotted line.
## Using 'scale_x/y_continuous' to scale the x/y axis.

plot_4_base <- partial_dartmouth_data %>%
  group_by(TERM_NUMBER, FIELD) %>%
  summarize(AVG_SCORE_FIELD = mean(SCORE, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = TERM_NUMBER, y = AVG_SCORE_FIELD, color = FIELD), size = 1) +
  geom_vline(aes(xintercept = TERM_NUMBER), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  scale_x_continuous(limits = c(1, 7),
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("20F", "21W", "21S", "21X", "21F", "22W", "22S")) +
  scale_y_continuous(limits = c(10 / 3, 12 / 3),
                     breaks = c(10 / 3, 11 / 3, 12 / 3),
                     labels = c("B+", "A-", "A")) +
  scale_color_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(3)) +
  labs(title = "Average Course Median by Field", 
       x = "Term", y = "Average Course Median") +
  theme_classic() +
  theme(legend.position = "none") +
  transition_reveal(TERM_NUMBER)

# plot_4_base

# Customizing the theme of the plot.

plot_4_final <- plot_4_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(color = "#000000", size = 12, family = "sans", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_blank(),
        # 
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"))

# plot_4_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_4 <- animate(plot_4_final, width = 250, height = 250, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_4

## Animated Plot 5 - This plot demonstrates the average course median by field 
## over time as a bar chart. The animation occurs over a time frame from 
## "20F" to "22S" (Dartmouth terms).

## Basic Plot - Creating a bar chart of the average course median by field at Dartmouth.

## Using 'group_by' to calculate statistics over groups (term and field).
## Using 'summarize' to determine the average course median by field.
## Using 'coord_cartesian' to zoom in on the relevant section.
## Using 'scale_x/y_continuous' to scale the x/y axis.
## Using 'scale_fill_manual' with 'colorRampPalette' to set the colors.

plot_5_base <- partial_dartmouth_data %>%
  group_by(TERM_NUMBER, FIELD) %>%
  summarize(AVG_SCORE_FIELD = mean(SCORE, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = 0, y = AVG_SCORE_FIELD, fill = FIELD), position = "dodge", width = 1) +
  scale_x_continuous(limits = c(-1 / 2, 1 / 2),
                     breaks = c(-1 / 3, 0, 1 / 3),
                     labels = c(" ", " ", " ")) +
  coord_cartesian(ylim = c(10 / 3, 12 / 3)) +
  scale_y_continuous(breaks = c(10 / 3, 11 / 3, 12 / 3),
                     labels = c("B+", "A-", "A")) +
  scale_fill_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(3)) +
  labs(title = " ", x = " ", y = " ") +
  theme_classic() +
  theme(legend.position = "none") +
  transition_reveal(TERM_NUMBER)

# plot_5_base

# Customizing the theme of the plot.

plot_5_final <- plot_5_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        #
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

# plot_5_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_5 <- animate(plot_5_final, width = 100, height = 250, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_5

## Animated Plot 6 - This plot demonstrates the average course enrollment by field 
## over time. The animation occurs over a time frame from "20F" to "22S"
## (Dartmouth terms).

## Basic Plot - Creating a line chart of the average course enrollment by field at Dartmouth.

## Using 'group_by' to calculate statistics over groups (term and field).
## Using 'summarize' to determine the average course enrollment by field.
## Using 'geom_vline' with 'linetype' to create a vertical dotted line.
## Using 'scale_x/y_continuous' to scale the x/y axis.

plot_6_base <- partial_dartmouth_data %>%
  group_by(TERM_NUMBER, FIELD) %>%
  summarize(AVG_ENRL_FIELD = mean(ENRL, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = TERM_NUMBER, y = AVG_ENRL_FIELD, color = FIELD), size = 1) +
  geom_vline(aes(xintercept = TERM_NUMBER), linetype = "dashed", color = "#CCCCCC", size = 0.5) +
  scale_x_continuous(limits = c(1, 7),
                     breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("20F", "21W", "21S", "21X", "21F", "22W", "22S")) +
  scale_y_continuous(limits = c(20, 60),
                     breaks = c(20, 30, 40, 50, 60)) +
  scale_color_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(3)) +
  labs(title = "Average Course Enrollment by Field", 
       x = "Term", y = "Average Course Enrollment") +
  theme_classic() +
  theme(legend.position = "none") +
  transition_reveal(TERM_NUMBER)

# plot_6_base

# Customizing the theme of the plot.

plot_6_final <- plot_6_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        plot.title = element_text(color = "#000000", size = 12, family = "sans", hjust = 0.5),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_blank(),
        # 
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"))

# plot_6_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_6 <- animate(plot_6_final, width = 250, height = 250, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_6

## Animated Plot 7 - This plot demonstrates the average course enrollment by field 
## over time as a bar chart. The animation occurs over a time frame from 
## "20F" to "22S" (Dartmouth terms).

## Basic Plot - Creating a bar chart of the average course enrollment by field at Dartmouth.

## Using 'group_by' to calculate statistics over groups (term and field).
## Using 'summarize' to determine the average course enrollment by field.
## Using 'coord_cartesian' to zoom in on the relevant section.
## Using 'scale_x/y_continuous' to scale the x/y axis.
## Using 'scale_fill_manual' with 'colorRampPalette' to set the colors.

plot_7_base <- partial_dartmouth_data %>%
  group_by(TERM_NUMBER, FIELD) %>%
  summarize(AVG_ENRL_FIELD = mean(ENRL, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = 0, y = AVG_ENRL_FIELD, fill = FIELD), position = "dodge", width = 1) +
  scale_x_continuous(limits = c(-1 / 2, 1 / 2),
                     breaks = c(-1 / 3, 0, 1 / 3),
                     labels = c(" ", " ", " ")) +
  coord_cartesian(ylim = c(20, 60)) +
  scale_y_continuous(breaks = c(20, 30, 40, 50, 60)) +
  scale_fill_manual(values = colorRampPalette(c("#0D1C93", "#4DE374"))(3)) +
  labs(title = " ", x = " ", y = " ") +
  theme_classic() +
  theme(legend.position = "none") +
  transition_reveal(TERM_NUMBER)
  
# plot_7_base

# Customizing the theme of the plot.

plot_7_final <- plot_7_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        #
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

# plot_7_final

## Using 'gganimate' to animate the plot, with the appropriate dimensions, duration, and fps.
## The renderer is  'magick_renderer' to ensure the output is a 'magick-image'.

animation_7 <- animate(plot_7_final, width = 100, height = 250, duration = 21,
                       fps = 20, renderer = magick_renderer())
# animation_7

## Using 'Magick' to create the visualization, by appending the images.
## The length of each animation should be the same (number of frames).

length(animation_1)
length(animation_2)
length(animation_3)
length(animation_4)
length(animation_5)
length(animation_6)
length(animation_7)

## Top Section

partial_gif_1 <- image_append(c(animation_1[1], animation_2[1]))

## Using a for loop to append the GIFs.

for (i in 2:420) {
  partial_gif_1 <- c(partial_gif_1, image_append(c(animation_1[i], animation_2[i])))
}

partial_gif_1

## Bottom Section

partial_gif_2 <- image_append(c(animation_3[1], animation_4[1], animation_5[1], 
                                animation_6[1], animation_7[1]))

## Using a for loop to append the GIFs.

for (i in 2:420) {
  partial_gif_2 <- c(partial_gif_2, image_append(c(animation_3[i], animation_4[i], animation_5[i], 
                                                   animation_6[i], animation_7[i])))
}

# partial_gif_2

## The length of each animation should be the same (number of frames).

length(partial_gif_1)
length(partial_gif_2)

## Final GIF - All of the GIFs together... using 'image_append'.

final_gif <- image_append(c(partial_gif_1[1], partial_gif_2[1]), stack = TRUE)

for (i in 2:420) {
  final_gif <- c(final_gif, image_append(c(partial_gif_1[i], partial_gif_2[i]), stack = TRUE))
}

final_gif

image_write(final_gif, "Dartmouth_Data.gif")

