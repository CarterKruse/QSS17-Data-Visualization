## Data Visualization (GOVT16-QSS17) Fall 2022
## Project 1 - COVID Survey
## Washington Post-University of Maryland Coronavirus Poll
##
## Name: Carter Kruse
## Date: October 24th, 2022

## Data
## Roper Center
## Week 1 - https://ropercenter.cornell.edu/ipoll/study/31117335
## Week 2 - https://ropercenter.cornell.edu/ipoll/study/31117342

## Packages

library(tidyverse)
library(ggplot2)
library(devtools)

library(forcats)
library(colorBlindness)
library(grid)
library(gridExtra)
library(cowplot)

?devtools

## Using the 'read_csv' function to import the data.

week_1 <- read_csv("/Users/carterkruse/Project1/data/31117335.csv")
week_2 <- read_csv("/Users/carterkruse/Project1/data/31117342.csv")

## Printing the 'typeof' and 'class' of the data frames.

typeof(week_1)
typeof(week_2)
class(week_1)
class(week_2)

## Using the 'glimpse' function to quickly view the data frames. See the documentation
## for explanation of the column names.

glimpse(week_1)
glimpse(week_2)

## Creating new data frames based on several of the demographic variables, as
## well as the survey responses.

data_frame_1 <- week_1 %>%
  select(totalage, educ, income, race, polparty, polview, sex,
         covi1b, covi1c, covi6)

data_frame_2 <- week_2 %>%
  select(totalage, educ, income, race, polparty, polview, sex,
         covi1b, covi1c, covi2)

## Renaming the columns of the data frames

colnames(data_frame_1) <- c("age", "education", "income", "race", "political_party", "political_view", "sex",
                  "trump_response", "state_governor_response", "worry")

colnames(data_frame_2) <- c("age", "education", "income", "race", "political_party", "political_view", "sex",
                  "trump_response", "state_governor_response", "worry")

## Viewing the new data frames.

glimpse(data_frame_1)
glimpse(data_frame_2)

## Binding the data frames together (appending the second to the first).

data_frame_3 <- rbind(data_frame_1, data_frame_2)
glimpse(data_frame_3)

## Viewing the structure and head/tail of the data frame.

str(data_frame_3)
head(data_frame_3)
tail(data_frame_3)

## Viewing the summary of the data frame.

summary(data_frame_3)

## Viewing the distinct elements from the data frame for various columns.

distinct(data_frame_3, age)
distinct(data_frame_3, political_party)
distinct(data_frame_3, trump_response)
distinct(data_frame_3, state_governor_response)
distinct(data_frame_3, worry)
distinct(data_frame_3, sex)

## Filtering the data frame to eliminate the rows containing data that is nondeclarative.

data_frame_4 <- data_frame_3 %>%
  filter(age != "Refused",
         political_party != "Refused", political_party != "Don't know", political_party != "Other",
         trump_response != "Refused", trump_response != "Don't know",
         state_governor_response != "Refused", state_governor_response != "Don't know",
         worry != "Refused", worry != "Don't know", worry != "Respondent already infected")

glimpse(data_frame_4)

## Using the %in% operator to determine if a given value is in a column, to demonstrate
## its purpose.

"Refused" %in% data_frame_4$age

## Modifying the data using 'pivot_longer' to demonstrate its purpose.

data_frame_4 %>%
  pivot_longer(names_to = "response_type", values_to = "response",
                trump_response:state_governor_response) %>%
  glimpse()

## Creating 'new' variables to change existing character strings into factors.

data_frame_5 <- data_frame_4 %>%
  mutate(age = factor(age),
         education = factor(education),
         income = factor(income),
         race = factor(race),
         political_party = factor(political_party),
         political_view = factor(political_view),
         sex = factor(sex))

glimpse(data_frame_5)

## Using 'case_when' to create ('mutate') the existing categorical variables
## into numerics.

data_frame_6 <- data_frame_5 %>%
  mutate(trump_response = case_when(
    trump_response == "Poor" ~ 0,
    trump_response == "Not so good" ~ 1,
    trump_response == "Good" ~ 2,
    trump_response == "Excellent" ~ 3,
    TRUE ~ 0
  ),
  state_governor_response = case_when(
    state_governor_response == "Poor" ~ 0,
    state_governor_response == "Not so good" ~ 1,
    state_governor_response == "Good" ~ 2,
    state_governor_response == "Excellent" ~ 3,
    TRUE ~ 0
  ),
  worry = case_when(
    worry == "Not worried at all" ~ 0,
    worry == "Not too worried" ~ 1,
    worry == "Somewhat worried" ~ 2,
    worry == "Very worried" ~ 3,
    TRUE ~ 0
  ))

glimpse(data_frame_6)

## Modifying the data using 'arrange' to demonstrate it's purpose.

data_frame_6 %>%
  arrange(worry) %>%
  glimpse()

data_frame_6 %>%
  arrange(desc(trump_response)) %>%
  glimpse()

## ggplot

## Plot 1 - This plot demonstrates the correlation between peoples' rating of
## President Trump's overall response to the COVID outbreak and peoples' worry
## about becoming infected or seriously ill from COVID.

## Using 'group_by' and 'summarize' to create a line plot of 'avg_worry' vs. 'trump_response'.
## Using 'scale_x_continuous' and 'scale_y_continuous' to scale the axis.
## Using 'scale_color_gradient_n' to scale the colors.
## Including labels for x, y, and the title.

plot_1_base <- data_frame_6 %>%
  group_by(trump_response) %>%
  summarize(avg_worry = mean(worry, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = trump_response, y = avg_worry, color = avg_worry)) +
  scale_x_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_color_gradientn(colors = c("#FFBA42", "#B70909")) +
  labs(title = "Worry vs. Rating of Trump's Response",
       x = "Rating of Trump's Response", y = "Worry", color = "Worry") +
  theme_classic()

plot_1_base

# Customizing the theme of the plot.

plot_1_final <- plot_1_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

plot_1_final

## Checking the plot for color blindness awareness.

colorBlindness::cvdPlot(plot_1_final)

## Plot 2 - This plot demonstrates the correlation between peoples' rating of
## their state government's overall response to the COVID outbreak and peoples' worry
## about becoming infected or seriously ill from COVID.

## Using 'group_by' and 'summarize' to create a line plot of 'avg_worry' vs. 'state_governor'.
## Using 'scale_x_continuous' and 'scale_y_continuous' to scale the axis.
## Using 'scale_color_gradient_n' to scale the colors.
## Including labels for x, y, and the title.

plot_2_base <- data_frame_6 %>%
  group_by(state_governor_response) %>%
  summarize(avg_worry = mean(worry, na.rm = TRUE)) %>%
  ggplot() +
  geom_line(aes(x = state_governor_response, y = avg_worry, color = avg_worry)) +
  scale_x_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_y_continuous(limits = c(0, 3), breaks = c(1, 2, 3)) +
  scale_color_gradientn(colors = c("#FFBA42", "#B70909")) +
  labs(title = "Worry vs. Rating of State Governments's Response",
       x = "Rating of State Government's Response", y = "Worry", color = "Worry") +
  theme_classic()

plot_2_base

# Customizing the theme of the plot.

plot_2_final <- plot_2_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid.major = element_line(size = 0.5, color = "#F1F1F1"),
        panel.grid.minor = element_line(size = 0.3, color = "#F1F1F1"),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

plot_2_final

## Plot 3 - This plot demonstrates the correlation between peoples' political
## party affiliation and peoples' worry about becoming infected or seriously ill from COVID.

## Using 'group_by' and 'summarize' to create a column/bar plot of 'avg_worry' vs. 'political_party'.
## Using 'fct_reorder' to reorder the x-axis based on the y-axis.
## Using 'alpha' to adjust the transparency of the bars.
## Using 'scale_y_continuous' to scale the axis.
## Using 'scale_color_gradient_n' to scale the colors.
## Including labels for x, y, and the title.

plot_3_base <- data_frame_6 %>%
  group_by(political_party) %>%
  summarize(avg_worry = mean(worry, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(political_party, -avg_worry), y = avg_worry, fill = avg_worry),
           alpha = 0.8) +
  scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_fill_gradientn(colors = c("#FFBA42", "#B70909")) +
  labs(title = "Worry vs. Political Party",
       x = "Political Party", y = "Worry", fill = "Worry") +
  theme_classic()

plot_3_base

# Customizing the theme of the plot.

plot_3_final <- plot_3_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

plot_3_final

## Plot 4 - This plot demonstrates the correlation between peoples' age and peoples'
## worry about becoming infected or seriously ill from COVID.

## Using 'group_by' and 'summarize' to create a column/bar plot of 'avg_worry' vs. 'age'.
## Using 'alpha' to adjust the transparency of the bars.
## Using 'scale_y_continuous' to scale the axis.
## Using 'scale_color_gradient_n' to scale the colors.
## Including labels for x, y, and the title.

plot_4_base <- data_frame_6 %>%
  group_by(age) %>%
  summarize(avg_worry = mean(worry, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = age, y = avg_worry, fill = avg_worry),
           alpha = 0.8) +
  scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_fill_gradientn(colors = c("#FFBA42", "#B70909")) +
  labs(title = "Worry vs. Age",
       x = "Age", y = "Worry", fill = "Worry") +
  theme_classic()

plot_4_base

# Customizing the theme of the plot.

plot_4_final <- plot_4_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

plot_4_final

## Plot 5 - This plot demonstrates the correlation between peoples' age and sex
## and peoples' worry about becoming infected or seriously ill from COVID using faceting.

## Using 'group_by' and 'summarize' to create a faceted column/bar plot of 
## 'avg_worry' vs. 'sex' and 'age'.
## Using 'scale_y_continuous' to scale the axis.
## Using 'scale_color_gradient_n' to scale the colors.
## Including labels for x, y, and the title.

plot_5_base <- data_frame_6 %>%
  group_by(age, sex) %>%
  summarize(avg_worry = mean(worry, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = sex, y = avg_worry, fill = avg_worry),
           alpha = 0.8) +
  scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3)) +
  scale_fill_gradientn(colors = c("#FFBA42", "#B70909")) +
  facet_wrap(~ age, nrow = 1) +
  labs(title = "Worry vs. Age & Sex",
       x = "Sex", y = "Worry", fill = "Worry") +
  theme_classic()

plot_5_base

# Customizing the theme of the plot.

plot_5_final <- plot_5_base +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF", color = "#FFFFFF"),
        panel.grid = element_blank(),
        # 
        title = element_text(color = "#000000", size = 10, family = "sans"),
        axis.title = element_text(color = "#000000", size = 9, family = "sans"),
        axis.text.x = element_text(color = "#000000", size = 8, vjust = 1, family = "sans"),
        axis.text.y = element_text(color = "#000000", size = 8, hjust = 1, family = "sans"),
        # 
        legend.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.key = element_rect(color = "#FFFFFF", fill = "#FFFFFF"),
        legend.title = element_text(color = "#000000", size = 9, family = "sans"),
        legend.text = element_text(color = "#000000", size = 8, family = "sans"),
        legend.key.size = unit(5, "mm"))

plot_5_final

## Joining all of the plots into a single plot, with an appropriate title and caption.

grid.arrange(arrangeGrob(plot_1_final + theme(legend.position = "none"),
            plot_2_final + theme(legend.position = "none"),
            plot_3_final + theme(legend.position = "none"),
            plot_4_final + theme(legend.position = "none"),
            nrow = 2),
            plot_5_final, nrow = 2, heights = c(1.5, 1),
            top = textGrob("Washington Post-University of Maryland COVID Survey", 
                           gp = gpar(fontsize = 14, fontface = "bold")),
            bottom = textGrob("            Worry is a measure of respondents concern of becoming infected and seriously ill from COVID from 1 (not worried) to 4 (very worried).
            Rating of Trump's Response is a measure of respondents perception of President Trump's response to COVID from 1 (negative) to 4 (positive).
            Rating of State Government's Response is a measure of respondents perception of state governors' response to COVID from 1 (negative) to 4 (positive).",
                              x = 0, y = 0.6, just = "left", gp = gpar(fontsize = 9, fontface = "italic")))

## Using 'arrangeGrob' over 'grid.arrange' so the final plot may be saved.

arranged_plot <- arrangeGrob(arrangeGrob(plot_1_final + theme(legend.position = "none"),
                                         plot_2_final + theme(legend.position = "none"),
                                         plot_3_final + theme(legend.position = "none"),
                                         plot_4_final + theme(legend.position = "none"),
                                         nrow = 2),
                             plot_5_final, nrow = 2, heights = c(1.5, 1),
                             top = textGrob("Washington Post-University of Maryland COVID Survey", 
                                            gp = gpar(fontsize = 14, fontface = "bold")),
                                            bottom = textGrob("             'Worry' is a measure of respondents concern of becoming infected and seriously ill from COVID from 1 (not worried) to 4 (very worried).
             'Rating of Trump's Response' is a measure of respondents perception of President Trump's response to COVID from 1 (negative) to 4 (positive).
             'Rating of State Government's Response' is a measure of respondents perception of state governors' response to COVID from 1 (negative) to 4 (positive).",
                                                              x = 0, y = 0.6, just = "left", gp = gpar(fontsize = 9, fontface = "italic")))

## Setting the background of the final plot to white.

final_plot <- ggdraw(arranged_plot) + theme(plot.background = element_rect(fill = "#FFFFFF", color = NA))
final_plot

## Saving the final plot.

ggsave(filename = "Washington_Post_University_of_Maryland_COVID_Survey.png", plot = final_plot, device = "png",
       width = 10, height = 8, units = "in")
ggsave(filename = "Washington_Post_University_of_Maryland_COVID_Survey.pdf", plot = final_plot, device = "pdf",
       width = 10, height = 8, units = "in")

