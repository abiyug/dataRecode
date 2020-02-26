# "This repository contains immunization rate data for schools across the U.S., 
# as compiled by The Wall Street Journal. The dataset includes the overall and 
# MMR-specific vaccination rates for 46,412 schools in 32 states. As used in 
# “What’s the Measles Vaccination Rate at Your Child’s School?“.
#
# Vaccination rates are for the 2017-18 school year for Colorado, Connecticut, 
# Minnesota, Montana, New Jersey, New York, North Dakota, Pennsylvania, South 
# Dakota, Utah and Washington. Rates for other states are 2018-19."

# Load the library
library(tidyverse)
library(datasets)
library(showtext)
library(patchwork)



#~~~~~~~~~~get the raw data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

df_state <- ggplot2::map_data("state") 

df_abb <-
        data.frame(state = state.name, state.abb)

#~~~~~~~~~~transorm the data

df_1 <-
        measles %>%
        mutate(year = case_when(
                year == "2018-19" ~ "2018-19",
                year == "null" ~ "2018-19",
                year == "2017-18" ~ "2017-18",
                year == "2017" ~ "2017-18")
        ) %>%
        filter(mmr != -1) %>% 
        select(index, year, state, county, enroll, mmr) %>%
        mutate(vcntd = enroll * (mmr/100),
               not_vcntd = enroll - vcntd) %>% 
        group_by(state, county, year) %>%
        summarise_at(vars(enroll:not_vcntd), na.rm = TRUE, mean) %>% 
        filter(!is.nan(enroll)) %>%
        mutate(prcnt_not_vcntd = not_vcntd / enroll)

df_2 <- df_1 %>% 
        ungroup() %>% 
        filter(prcnt_not_vcntd > 0.19) %>% 
        select(state, county)

df_3 <-        
        measles %>%
        mutate(year = case_when(
                year == "2018-19" ~ "2018-19",
                year == "null" ~ "2018-19",
                year == "2017-18" ~ "2017-18",
                year == "2017" ~ "2017-18")
        ) %>%
        filter(mmr != -1) %>% 
        select(state, county, lng, lat) 

df_4 <-
        df_2 %>% 
        inner_join(df_3) %>% 
        group_by(state, county) %>%
        group_modify(~ head(.x, 1L)) %>%
        left_join(( df_1 %>% ungroup() %>% filter(prcnt_not_vcntd > 0.19))) %>%
        left_join(df_abb)


#~~~~~~~~~~~Vizualize
gg_1 <- 
        ggplot(data = df_1, aes(x = log(enroll), y = prcnt_not_vcntd)) +
        geom_jitter(alpha = 0.5, col = "gray44") +
        geom_jitter(data = (df_1 %>% filter(prcnt_not_vcntd > 0.19)), 
                    aes(x = log(enroll), y = prcnt_not_vcntd),  shape=21, color= "red", size=3, stroke=1) +
        
        geom_smooth(se = FALSE) +
        geom_text_repel(data = df_4, size = 3,
                        aes(x = log(enroll), y = prcnt_not_vcntd, label = paste0(county,", ",state.abb,"\n" ,scales::percent(prcnt_not_vcntd)))) + 
        facet_wrap(~ year, scales = "free", ncol = 1) +
        scale_y_continuous(labels = scales::percent_format()) 


gg_2 <-
        ggplot() +
        geom_path(data = df_state,  aes(long, lat, group = group), col = "gray 58", alpha = 0.7) +
        geom_jitter(data = df_4, aes(x = lng, y = lat, group = 1), col = "red") +
        geom_text_repel(data = df_4, size = 3,
                        aes(x = lng, y = lat, group = 1, label = paste0(county,", ",state.abb," " ,scales::percent(prcnt_not_vcntd)))) +
        facet_wrap(~ year, ncol = 1) 
        

#~~~~~~~~add theme
# Import google font
font_add_google("audiowide", "audiowide")


gg_1 <- gg_1 + labs(x ="Enrollment - log(Enrollment)",
                    y = "Not Vacinated Student %",
                     caption = "Note: Measles, Mumps, and Rubella (MMR), counties that didn't report enrollment are not included.\nData Source: The Wallstreet Journal\n Analytic/Viz by: abiyu.giday[at]dataRecode.com"
                    ) +
        theme_minimal() +
        theme( axis.text.x = element_text(angle = 0, hjust = 1),
               strip.text.x = element_text(size = 12, colour = "darkorange3", face = "bold"),
               plot.title   = element_text(size = 20, colour = "darkorange3"),
               plot.subtitle = element_text(size = 12, colour = "gray48"),
               text = element_text(size = 10, family = "audiowide"),
               legend.position = "none")

gg_2 <- gg_2 + labs(x ="", 
                    y = "",
                    title = "US Schools None Vaccination enrollment rates from 2017-2019",
                    subtitle = "The number of counties with more than 20% none vaccination rate has gone down from 11 to 2 between 2017 and 2019."
                    )  +
        #theme_classic() +
        theme( strip.text.x = element_text(size = 12, colour = "darkorange3", face = "bold"),
               plot.title   = element_text(size = 20, colour = "darkorange3"),
               plot.subtitle = element_text(size = 12, colour = "gray48"),
               text = element_text(size = 10, family = "audiowide"),
               axis.text = element_blank(), 
               axis.text.x = element_blank(),
               axis.ticks = element_blank(),
               legend.position = "none")


gg_2 + gg_1

#~~~~~~~~~~~~~~~End~~~~~~~~


