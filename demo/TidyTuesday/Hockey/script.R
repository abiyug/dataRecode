# Date: March 03, 2020
#
# The script compares the cumulative goal scores of NHL's 250 hockey greats. To make the comparison,
# we will consider only the first 15 seasons of each players goals while playing in the 'NHL'. The 
# Analysis also limit the players who started playing after 1960. The 15 year cap is examine and compare
# Alex Overchin's 700 goal achievements in the first 15 season.
#
# For: TidyTuesday Challenge

# Load the library
library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggthemes)
library(magick)
library(grid)


#~~~~~~~~~~~~~get the data
season_goals <- read_csv("data/season_goals.csv")
img <- image_read("fig/hockey_player.png")


#~~~~~~~~~~~~~transform the data
df_season <- 
        season_goals %>% 
        filter(yr_start >= 1960, league == "NHL") %>%
        select(player, season, goals ) %>%
        group_by(player, season) %>%
        summarize(tot_gols = sum(goals)) %>%
        mutate(cum_goal = cumsum(tot_gols), row_num = row_number(),
               sesn = as.numeric(str_extract(season, "[0-9]{1,4}"))) %>% 
        filter(row_num >= 1, row_num <= 15) %>% 
        ungroup()  

df_label <-
        df_season %>% 
        filter( player %in% c("Wayne Gretzky", "Alex Ovechkin", "Mike Gartner")) %>% 
        group_by(player) %>% 
        slice(which.max(cum_goal)) %>%
        inner_join(season_goals) %>%
        select(player, season, tot_gols, cum_goal, row_num, sesn, headshot) %>%
        distinct()


#~~~~~~~~~~~~~Data Viz
gg <-
        ggplot(data = df_season, aes(x = as.numeric(sesn), y = cum_goal, group = player)) +
        annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
        geom_line(col= "gray56")  +
        geom_line(data = (df_season %>% filter( player %in% c("Wayne Gretzky", "Alex Ovechkin", "Mike Gartner"))), aes(x = as.numeric(sesn), y = cum_goal, group = player, col = player), alpha = .8, size = 1.5) +
        geom_point(data = df_point, aes(x = as.numeric(sesn), y = cum_goal, col= player), size = 2) +
        geom_text_repel(data = df_season %>% filter( player %in% c("Wayne Gretzky", "Alex Ovechkin", "Mike Gartner")) %>% group_by(player) %>% slice(which.max(cum_goal)), aes(x = as.numeric(sesn), y = cum_goal, label = paste0(player,": ",cum_goal), col= player, vjust = 1, hjust = 1)) +
        geom_hline(aes(yintercept=700),  color="cornflowerblue", linetype = "dashed")  + 
        geom_image(data = df_point, aes(x = as.numeric(sesn) + 5, y = cum_goal + 5 , image = headshot), size = .05) +  
        scale_x_continuous(breaks = seq(1950, 2020, 5)) 


# plot text
gg <- gg + labs(x ="", 
                y ="Goals Scored",
                title = "The 700/15 club",
                subtitle = "How does Overchkin's goal scoring accomplishment compare to Hockey grates?\nOnly 3 NHL players are eligible for the 700 goal scores on their first 15 NHL seasons*",
                caption = "Note: The first 15 NHL seasons for players who started playing after 1960.\nData Source: https://www.hockey-reference.com/ \nAnalytic/Viz by: abiyu.giday[at]dataRecode.com") 

# plot style
gg <- gg + theme(text = element_text(size = 10, family = "kelly"),
                 plot.title   = element_text(size = 20, colour = "darkorange3"),
                 plot.subtitle = element_text(size = 12, colour = "gray48"),
                 plot.caption  = element_text(size =  10, colour = "gray38"),
                 legend.position = "none"   
)
gg
