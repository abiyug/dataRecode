####################
# The script wrangle the hotel booking data and pulls out the top 10 booking cancellation by country of origin,
# and  generates a graphic plot that highlights the countries with thematic map and labels. 
#
# Fen 14, 2020
#
# For Tidytuesday submission
####################

# load libraryies
library(tidyverse)
library(scales)
library(maps)
library(showtext)
library(gridExtra)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')


# Import google font
font_add_google("Kelly Slab", "kelly")

# Grab 3 leter ISO code from maps
data("iso3166")

# rename var to help for downstream merge
df_iso <- 
iso3166 %>% 
        select(COUNTRY = 3, iso_aw = 1, un_a3 = 2) 

# Tabulate the cancle rate, not cancle rate for each country of origin
df_cntry <- 
        hotels %>%
        filter(arrival_date_year == 2016) %>%
        group_by(is_canceled, country) %>%
        tally() %>%
        pivot_wider(names_from = is_canceled, values_from = n, values_fill = list(n = 0)) %>%
        select(country, not_cancld = 2, cancld = 3) %>%
        mutate(cncl_rate = cancld/(cancld + not_cancld)) %>%
        select(un_a3 = 1, everything())

# get simple world map adm 0 lvl from maps
df_world <- ggplot2::map_data("world")

# Replace USA with 'United States of America' to prep for merge 
df_world <-
        df_world %>% 
        mutate(country = stringr::str_replace(region, "USA", "United States")) %>%
        select(-5) # 5 is region - redundunt info

# merge the hotel tables with 3 letter iso code
df <- 
        df_world %>%
        left_join(df_iso, by = c("country" = "COUNTRY")) %>%
        filter(country != "Antarctica") %>%
        left_join(df_cntry, by = "un_a3")

# table booking cancnel more than 40  by country of origin
# grab the top 10 to display
df_table <- 
        df %>% 
        filter(cancld > 40) %>% # morocco is for countries with at least 50 cancelation
        select(country, cncl_rate) %>%
        distinct() %>%
        mutate(prcnt_cncl = scales::percent(cncl_rate)) %>% 
        arrange(desc(cncl_rate)) %>%
        select(-cncl_rate) %>% 
        head(10) 


# Prepare the lable data frame
# grab cities from maps library
data("world.cities")
df_cities <- world.cities

# Get the names of the city for the top 10
df_label <- 
        df_table %>%
        inner_join((df_cities %>% filter(capital ==1)), by = c("country" = "country.etc")) %>%
        mutate(rank = 1:n())


# Data Viz
gg <-        
        ggplot()  +
        geom_polygon(data = df, aes(long, lat, group = group), fill = NA, linetype = "solid", color = "gray56") +
        geom_polygon(data = (df %>% filter(cancld > 40)), aes(long, lat, group = group, fill = cncl_rate), linetype = "solid", color = "gray56") +
        theme_gray() +
        #theme(legend.position = "none") +
        theme_minimal() +
        scale_fill_distiller(name = "percent Cancle Rate", palette = "YlOrRd", 
                             direction = 1) + theme(legend.position = "bottom")



#gg + geom_text(data = df_label, aes(x = long, y = lat , label = rank), col = "gray44")
gg_3 <- gg + geom_label(data = df_label , aes(x = long, y = lat, label = rank), nudge_y = 1, fontface = "bold", size = 3, col = "dodgerblue4") 
#gg + ggrepel::geom_text_repel(data = df_label, aes(x = long, y = lat, label= country))  

# table placment
gg_3 <- 
        gg_3 +  ggplot2::annotation_custom(tableGrob(df_table), xmin=-200, xmax=-150, ymin=-50, ymax=-40)


# plot text
gg_3 <- gg_3 + labs(x ="", 
                    y ="",
                    title = "2016 top 10 Hotel booking Cancelation by country of origin",
                    subtitle = "For countries of origin with more than 40 cancelation.",
                    caption = "Data Source: TidyTuesday via Antonio, Almeida & Nunes 2019\n Analytic/Viz by: abiyu.giday[at]dataRecode.com") 

# plot style
gg_3 <- gg_3 + theme(text = element_text(size = 10, family = "kelly"),
                     plot.title   = element_text(size = 20, colour = "darkorange3"),
                     plot.subtitle = element_text(size = 12, colour = "gray48"),
                     panel.background = element_rect(fill = "lightblue")) 
gg_3


        
