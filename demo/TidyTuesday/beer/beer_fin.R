# Date: April 3, 2020
#
# This script generate US state-level beer production trend for 2008-2019. We also calculate 
# the anual growth rate for each state. The Formula for the Average Annual Growth Rate (GR_1+GR_2+…+GR_n) / N.
# where:GR_1 = Growth rate in period 1; GR_2 = Growth rate in period 2; GR_n =Growth rate in period n; N = Number of years
#


#~~~~~~~~~~~~~~~~~~~~~
# load library
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggthemes)
library(geofacet)



#~~~~~~~~~~~~~~~~~~~~~~~~~
# get the data

beer_states <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv", 
                        col_types = cols(year = col_datetime(format = "%Y")))

# format the year variable 
beer_states <- beer_states %>% mutate(year = ymd(year))


#~~~~~~~~~~~~~~~~~~~~~~~~~
# wrangle the data

# There are some missing data  - impute the missing

# identify the missing 
beer_states %>%
        #summarise_all(funs(sum(is.na(.))))  # 19 missing in barrels
        filter(is.na(barrels))  # who are they

# calc the avg for each state that and use to fill the missing values
df_avg_barrel_state <-
        beer_states %>%
        filter(state %in% c("MS", "AL", "ND", "SD", "DC", "ND")) %>% 
        mutate_at(vars(barrels), ~replace(., is.na(.), 0)) %>%
        group_by(state) %>%
        summarise(avg = mean(barrels))

# get the data ready and impute the missing
df_beer_state_imtd <-
beer_states %>%
        group_by(state) %>%
        mutate(barrels = case_when(
                is.na(barrels) == TRUE & state == "AL" ~ "8794",
                is.na(barrels) == TRUE & state == "DC" ~ "5446",
                is.na(barrels) == TRUE & state == "MS" ~ "5419",
                is.na(barrels) == TRUE & state == "ND" ~ "1438",
                is.na(barrels) == TRUE & state == "SD" ~ "1826",
                is.na(barrels) == FALSE ~ as.character(barrels)
        )) %>%
        mutate(barrels = as.numeric(barrels)) %>%
        filter(state != "total")


# Beer production by barrel for each state and year; calculate aggregate trend.
df_grwthRate <-
df_beer_state_imtd %>%
        group_by(state, year) %>%
        summarise(tot_barl = sum(barrels)) %>%
        arrange(state, year) %>%
        mutate(yearOverYear = tot_barl/lag(tot_barl,1)) %>% # y/y 
        mutate_at(vars(yearOverYear), ~replace(., is.na(.), 0)) %>%
        ungroup() %>%
        group_by(state) %>%
        mutate(growth_rate = sum(yearOverYear) /  12) %>% #Average Annual Growth Rate
        arrange(desc(growth_rate)) %>% 
        ungroup() 
        

# prepare the label for the y/y growth rate
df_label <-
        df_grwthRate %>% 
        group_by(state) %>% 
        mutate(avg_barl = mean(tot_barl)) %>%
        filter(year == ymd(20090101))

# ggplot
gg_grwthRate <-       
ggplot(df_grwthRate, aes(year, tot_barl, group = state)) + 
        geom_line(col = "white") + #
        geom_text(data = df_label, aes(x = year, y = avg_barl, label = round(growth_rate,2)), size = 3, col = "darkgoldenrod1", family = "Batang") +  
        geofacet::facet_geo(~ state, scales = "free_y") + # facet wrap it by state
        scale_y_continuous(labels = scales::number_si) 

# Customize the theme
gg <- gg_grwthRate + theme_hc(style = "darkunica") +
                     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=10),
                           plot.title    = element_text(size = 28, colour = "gray48", family = "Batang"),
                           plot.subtitle  = element_text(size = 14, colour = "gray48", family = "Batang"),
                           plot.caption  = element_text(size =  12, colour = "gray48", family = "Batang"),
                           strip.background = element_rect(colour = "grey20", fill = "grey85"))

# Add labels
gg <- gg + labs(x ="", 
                y ="Number of Barrels per year",
                title = " 2008-2019 State-level Average Annual Growth Rate production by Barrel",
                subtitle = "Beer production has grown across all states, the fastest growth seen from TN, CT, AL and slowest in NC, NJ.",
                caption = "Source:  US Alcohol and Tobacco Tax and Trade Bureau (TTB) | https://www.ttb.gov/beer/statistics | graphics: @abiyugiday") 
