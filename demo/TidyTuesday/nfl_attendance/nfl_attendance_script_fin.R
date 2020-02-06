# Tidy Tuesday NFL Stadium Attendance from 2000 - 2019
# Feb 4, 2020
# @abiyugiday
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-04/readme.md

# Load the library
library(tidyverse)
library(scales)
library(showtext)  # for google font

# Import the data
attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

# load the font from google # https://fonts.google.com/specimen/Courgette
font_add_google("Courgette", "courgette")


# data frame for calculating the average for each teacm over the last 20 years
df <- 
        attendance %>% 
        select(team_name, year, total) %>% 
        distinct() %>% 
        group_by(team_name) %>% 
        summarize(tot = sum(total),
                  avg_20 = mean(total/16),
                  avg_atnd = paste0(round(avg_20/1000), "k")
        ) %>%
        mutate(team_name = factor(team_name, levels = df$team_name)) %>%
        arrange(desc(tot)) %>% 
        as.data.frame() 

# data frame that includes average attendace for each year and each team -
df_2 <-
        attendance %>% 
        select(team_name, year, total) %>% 
        distinct() %>% 
        mutate(avg_attnd = total/16,
               team_name = factor(team_name, levels = df$team_name)) 


# Small multiple plot for each team - Plot ordred from the most Cowboys to the least Texans
# Texans join the team 2002 

gg_2 <-
        ggplot(data = df_2, aes(x = year, y = avg_attnd, group = team_name)) + 
        geom_line() +
        geom_line(data = (df_2 %>% filter(team_name == "Cowboys")), aes(x = year, y = avg_attnd, group = team_name), col = "darkolivegreen3", size =2) +
        geom_hline(dat = df, mapping = aes(yintercept = avg_20), color="red", size = .5, linetype = "dotdash") +
        #geom_text(data = df , aes(x = 2009, y = avg_20, label = avg_atnd)) +
        facet_wrap(~ team_name, scales = "free_y") +
        scale_y_continuous(labels = scales::number_si) +
        scale_x_continuous(breaks = seq(2000, 2019, 3)) +
        theme_minimal() +
        theme( axis.text.x = element_text(angle = 90, hjust = 1),
               strip.text.x = element_text(size = 12, colour = "royalblue1", face = "bold"),
               plot.title   = element_text(size = 20, colour = "gray48"),
               plot.subtitle = element_text(size = 12, colour = "gray48")) #lightskyblue4

gg_2 <- gg_2 + labs(x ="", 
                    y ="Average weekly attendance",
                    title = "Dallas Cowboys lead the league in stadium attendance",
                    subtitle = "200-2019 NFL stadium attendance for Each team.  The red line is the average for the respective team.",
                    caption = "Note: Plot order most (Cowboys) to the least (Texans)\nData Source: TidyTuesday via Pro Football Reference\n Analytic/Viz by: @abiyugiday") 

gg_2 <- gg_2 + theme(text = element_text(size = 10, family = "courgette"))
gg_2

