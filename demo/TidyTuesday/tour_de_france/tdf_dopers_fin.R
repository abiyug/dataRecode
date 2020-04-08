# April 8, 2020
#
# The script examines dopping history of Tour de France winners from 1903 to 2020.
# The dopping data comes from  https://en.wikipedia.org/wiki/Doping_at_the_Tour_de_France
# THe raw data comes from Alastair Rushworth's Data Package tdf and Kaggle.
#


#~~~~~~~~~~~~~~~~~~~~~~
# load library
library(tidyverse)
library(png)
library(ggimage)
library(tidyr)
library(ggthemes)

#~~~~~~~~~~~~~~~~~~~~~~
# Import data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

dopers_list <- read_csv("Tour_de_France/data/dopers_list.csv",  col_types = cols(Years = col_character()))

#~~~~~~~~~~~~~~~~
#Wrangle/Analyiss
# Note: 2019 dopooing status is TBD so it is left out
df_dopers <-          
dopers_list %>%
          mutate(doped = ifelse(Status == "Never failed tests", "no", "yes")) %>%
          filter(Years != "2019")  %>%#TBD
          select(Name, doped) 

# Merge the raw data with dopping data
# 38 dopped(some repeat 65.5% were doppers!) - no dope
df <-
  tdf_winners %>% 
  filter(edition >= 47) %>%
  #filter(date > ymd(19600626))
  left_join(df_dopers, by = c("winner_name" = "Name")) %>%
  mutate( km_hr = distance / time_overall)  %>% 
  select(edition, start_date, winner_name, doped, km_hr, everything()) 

# df used for annotation and image addition
df_2 <-  data.frame(xmin = as.Date(c('1961-01-01','1969-01-01','1997-01-01')),
                    xmax = as.Date(c('1967-01-01','1986-01-01','2006-01-01')),
                    ymin = c(-Inf,-Inf, -Inf),
                    ymax = c(Inf,Inf, Inf),
                    url = c("Tour_de_France/fig/dop_bikers.png", "Tour_de_France/fig/dop_bikers.png", "Tour_de_France/fig/dop_bikers.png"))


#~~~~~~~~~~~~~~~~
# Viz
gg<- 
  ggplot() +
  geom_point(data = df, aes(x = start_date, y = km_hr), shape = 21, fill = "forestgreen", col = "gray58", size = 4)+
  geom_point(data = (df %>% filter(doped =="yes")), aes(x = start_date, y = km_hr), shape = 21, fill = "firebrick3", col = "gray58", size = 4) +
  scale_x_date(expand=c(0,0), date_breaks = "5 year", date_labels = "%Y") +
  geom_rect(data=df_2,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill="gray58",
            alpha=0.2,inherit.aes=FALSE) +
  geom_image(data = df_2, aes(x = as.Date('2000-01-01'), y = 35, image = url), size = .40)  

# Customize the theme
gg <- gg + theme_hc() +
  theme(
        plot.title    = element_text(size = 24, colour = "darkolivegreen", family = "Batang"),
        plot.subtitle  = element_text(size = 14, colour = "gray48", family = "Batang"),
        plot.caption  = element_text(size =  10, colour = "gray48", family = "Batang"),
        strip.background = element_rect(colour = "grey20", fill = "grey85"),
        legend.position = "none")

# Add labels
gg <- gg + labs(x ="", 
                y ="Average KM/hr for the entire course",
                title = "65% of Tour de France Winners Cuaght or Admited to Dopping",
                subtitle = "1960-2018 Dopping history from https://en.wikipedia.org/wiki/Doping_at_the_Tour_de_France\nRED = Doppers",
                caption = "Analysis/graphics: @abiyugiday\nSource: https://github.com/alastairrushworth/tdf\nBickers Image: https://www.nytimes.com/2012/08/12/opinion/sunday/how-to-get-doping-out-of-sports.html") 
gg