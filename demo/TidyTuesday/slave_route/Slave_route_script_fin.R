# June 18, 2020

# This graphic is generated for the week 25 tidytuesday data challenge.
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-16/readme.md

# The data from slavevoyages.org shows that  most number of slaves were traded from the port of Lunada Angola.
# The grpahics include table for the top 10 destination from Angola, the slave trade trend from 1588-1848 and
# the path from Luanda to each of the top 10 destinations to the Americas.
#
#

# Load the library
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(santoku)
library(gridExtra)
library(grid)
library(ggrepel)
library(ggmap)
library(ggspatial)


# get the data
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

# get the world map
df_world <- map_data("world")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#** Question: What are the top t10 Purchase origin in the data

slave_routes %>%
        count(place_of_purchase, sort = TRUE)

# "Africa unspecified is #1 second is Luanda, Angola

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#** Question: What are the top 10 route destination for for the most number of abducted people from Lunanda

# Clean up the data 
# impute missing number by averag for each location and for binned (20 year period)
# Bin the count every 20 years (2 decades)

df_luanda <-
slave_routes %>%
        filter(place_of_purchase == "Luanda") %>% 
        mutate(port_arrival = recode(port_arrival,
                                     "Pernambuco, port unspecified" = "Pernambuco",
                                     "Bahia, port unspecified" = "Bahia")) %>%
        dplyr::select(voyage_id, port_origin, year_arrival, port_arrival, place_of_purchase, n_slaves_arrived) %>%
        mutate(decade = chop_width(year_arrival, 20)) %>%
        group_by(decade, port_arrival) %>% 
        mutate(n_slave = ifelse(is.na(n_slaves_arrived), mean(n_slaves_arrived, na.rm = TRUE),n_slaves_arrived)) %>%
        ungroup()


# Id the top 10 by number of abducted by port of arrival from Lunada
# Add geo locaton for the top 10 for mapping {require geocode key from google}

# To request geocode from google api register/obtain key
#ggmap::register_google(key = "xxxx")

df_lunada_top_10 <-
df_luanda %>%
        group_by(port_arrival, place_of_purchase) %>%
        summarize(tot = sum(n_slave, na.rm = TRUE)) %>%
        ungroup() %>%
        distinct() %>%
        arrange(desc(tot)) %>%
        top_n(10) %>%
        # Specify the county for Campos & Bhahi because there are other cities in diffren location with the same name
        mutate(port_arrival = recode(port_arrival,
                                     "Campos" = "Campos, Brazil",
                                     "Bahia" = "Bahia, Brazi")) %>%
        mutate_geocode(port_arrival) %>%
        mutate_geocode(place_of_purchase) %>%
        #remove the country location after getting the geo location
        mutate(port_arrival = recode(port_arrival,
                             "Campos, Brazil" = "Campos",
                             "Bahia, Brazil" = "Bahia"))

#write.csv(df_lunada_top_10, file = "slave_route/data/top_10_arrival.csv", row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Question: What wasthe overall active abduction decades?

# clean the decade range for line line plot of the most abduction decades
df_top10_by_decade <-
df_luanda %>% 
        group_by(decade) %>%
        summarise(tot = sum(n_slaves_arrived, na.rm = TRUE)) %>%
        ungroup %>%
        arrange(desc(tot)) %>%
        mutate_all(~str_replace_all(., "\\[", "")) %>%
        mutate_all(~str_replace_all(., "\\]", "")) %>%
        mutate_all(~str_replace_all(., "\\)", "")) %>%
        mutate_all(~str_replace_all(., ", ", "-")) %>%
        mutate_each(list(as.numeric), tot ) 


        

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Table for the top 10 by arrilval location decades from "Luanda"
df_decade_table_luanda <-
df_luanda %>%
        semi_join(df_lunada_top_10, by = c("port_arrival", "place_of_purchase")) %>%
        group_by(port_arrival,decade) %>%
        summarise(tot = sum(n_slave, na.rm = TRUE)) %>%
        ungroup %>%
        arrange(desc(tot)) %>%
        mutate_all(~str_replace_all(., "\\[", "")) %>%
        mutate_all(~str_replace_all(., "\\]", "")) %>%
        mutate_all(~str_replace_all(., "\\)", "")) %>%
        mutate_all(~str_replace_all(., ", ", "-")) %>%
        mutate_each(list(as.numeric), tot ) %>%
        pivot_wider(names_from = port_arrival, values_from = tot, values_fill = list(tot = 0)) %>%
        janitor::clean_names()
        

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Viz
# Trend
gg_trend <-
ggplot(df_top10_by_decade, aes(x = decade, y = tot)) +
        geom_point() +
        annotate(geom = "rect", xmin = 10, xmax = 13, ymin=-Inf, ymax=+Inf, fill = "green2", alpha = 0.2) +
        geom_line(aes(group = 1), col = "green2", size = 1) +
        theme(  plot.title    = element_text(size = 14, colour = "gray48", face = "bold"),
                axis.text.x = element_text(angle = 90, size= 10,  vjust = 0.5, colour = "gray48", face = "bold"),
                axis.text.y = element_text(size= 10, vjust = 0.5, colour = "gray48", face = "bold"),
                axis.title.y  = element_text(size = 10, colour = "gray48", face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA)
        ) +
        labs(x = "", y = "Number of slaves traded",
             title = "From 1768-1848 was most Active") 

# get the map
df_world <- map_data("world") %>% filter(region != "Antarctica")


# Add the map and segments from Lunanda
gg <-
ggplot() + 
        geom_polygon(data =df_world, aes(long, lat, group = group) , linetype = "solid", color = "gray56", fill = "cornsilk3") +
        # add curves
        geom_curve(data = df_lunada_top_10 %>% filter(!port_arrival %in% c("Rio de Janeiro","Ilha Grande", "Buenos Aires")), aes(x = lon1, y = lat1, xend = lon, yend = lat, group = 1), col = "green2",
                   curvature = .5, arrow = arw_opt) +     #size = log(df_from_Luanda$tot) , 
        geom_curve(data = df_lunada_top_10 %>% filter(port_arrival %in% c("Rio de Janeiro")), aes(x = lon1, y = lat1, xend = lon, yend = lat, group = 1), col = "green2", 
                   curvature = -.5, arrow = arw_opt) +
        geom_curve(data = df_lunada_top_10 %>% filter(port_arrival %in% c("Ilha Grande", "Buenos Aires")), aes(x = lon1, y = lat1, xend = lon, yend = lat, group = 1), col = "green2",
                   curvature = -.7, arrow = arw_opt) +
        # point
        geom_point(data = df_lunada_top_10, aes(x = lon1, y = lat1, group = 1), col = "red",  alpha = .5, size = 2) + 
        geom_point(data = df_lunada_top_10, aes(x = lon, y = lat, group = 1), col = "gray20", alpha = .5, size = 2) + #,  size = df_from_Luanda$tot/1000
        xlim(-200, 60) + ylim(-60, 75) +
        labs(title = "1588-1848 Slave Trade Route From Luanda, Angola to the Americas was the Busiest ", 
             caption = "Source: slavevoyages.org | Graphic: @abiyugiday",
             x = "", y = "") +
        theme( 
                plot.title    = element_text(size = 24, colour = "gray48",  hjust = 0.5),
                plot.caption  = element_text(size =  12, colour = "gray48", hjust = 0.5))

# Add text
gg_map <-
        gg + 
        # up direction segments
        geom_text_repel(data = (df_lunada_top_10 %>% filter(port_arrival %in% c("Macae", "Rio de Janeiro", "Ilha Grande"))), aes(x = lon, y = lat, label = port_arrival), 
                        col = "black", 
                        size = 4, #segment.color = NA, 
                        #family = "Staatliches",
                        nudge_x       = 2.7,
                        segment.size  = 0.4,
                        segment.color = "red",
                        direction     = "y",
                        hjust         = 1
        ) +
        # down direction segments
        geom_text_repel(data = (df_lunada_top_10 %>% filter(!port_arrival %in% c("Macae", "Rio de Janeiro", "Ilha Grande"))), aes(x = lon, y = lat, label = port_arrival), 
                        col = "black", 
                        size = 4,  
                        nudge_x       = -2.7,
                        segment.size  = 0.4,
                        segment.color = "red",
                        direction     = "x",
                        vjust         = 0
        ) +
        # Luanda
        geom_text_repel(data = df_lunada_top_10 %>% filter(port_arrival == "Rio de Janeiro"), aes(x = lon1, y = lat1, label = place_of_purchase), col = "black", size =4, segment.color = NA, hjust = 0, nudge_x = 0.9)  

gg_map <- gg_map + theme(plot.background = element_rect(fill = "dodgerblue4"),
                       panel.background = element_blank(), 
                       panel.grid =  element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title = element_blank(),
                       legend.position = c(.1,.2),
                       legend.spacing.x = unit(0.2, "cm"),
                       legend.text = element_text(colour="white", size = 10, face = "bold"), 
                       legend.background = element_rect(fill = "dodgerblue4",size= 1, linetype="solid", colour ="white"),
                       legend.key = element_blank(), # removed the back ground of the legend key
                       legend.title=element_blank()) 


# patch the plots together

# theme for tableGrob
theme_2 <- gridExtra::ttheme_minimal(base_size = 9, base_colour = "gray65")

# plot Grob
gg_trend1 <- ggplotGrob(gg_trend)

# Putting all of it together/position
gg_map2 <-
        gg_map +  
        annotation_custom(tableGrob(df_decade_table_luanda,theme = theme_2, rows=NULL ), 
                          xmin = -200,  xmax = -100,
                          ymin = -70,  ymax = -10) +
        annotation_custom(grob = gg_trend1, xmin = -200,  xmax = -120,
                                            ymin = 0,  ymax = 40) 

# Add teh N/compass
gg_map2 <- gg_map2 + ggspatial::annotation_north_arrow(which_north = "grid", 
                                             location = "br",
                                             style = north_arrow_fancy_orienteering )


#ggsave(file = "slave_route/fig/gg_map2.png", dpi = 320, width = 12, height = 5.9)












#~~~~~~~~~~~~~~~~~~~end
