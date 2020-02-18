#load the library
library(tidyverse)
library(ggrepel)
library(showtext)


# get the data
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

#font_add_google("Courgette", "courgette")
font_add_google("Audiowide", "audiowide")


# Over all Prcnt co2 emmision brake down
df_prcnt <-
        food_consumption %>%
        group_by(food_category) %>%
        summarize(tot_co2 = sum(co2_emmission),
                  tot_cons = sum(consumption)) %>%
        mutate(prcnt_co2 = scales::percent(tot_co2/sum(tot_co2)),
               prcnt_cons = scales::percent(tot_cons/sum(tot_cons))) %>% 
        arrange(desc(tot_co2)) %>%
        ungroup() %>%
        select(food_category, prcnt_co2, tot_co2)


# Top 5 countries of each category
df_co2 <-
        food_consumption %>% 
        pivot_longer(cols = consumption:co2_emmission) %>%
        filter(name == "co2_emmission") %>%
        group_by(food_category) %>% 
        arrange(desc(value)) %>% 
        group_modify( ~ head(.x, 5L)) %>% 
        inner_join(food_consumption, by = c("country", "food_category")) %>%
        select(-value) %>% 
        mutate(avg_emsn = mean(co2_emmission)) %>%
        left_join(df_prcnt, by = "food_category") %>%
        mutate(food_prcnt_emsn = paste0(food_category," ", prcnt_co2))

# factor level - order by emmission lvl
df_co2$food_prcnt_emsn <- factor(df_co2$food_prcnt_emsn)

df_co2$food_prcnt_emsn <- 
        fct_reorder(df_co2$food_prcnt_emsn, df_co2$tot_co2,.desc = TRUE ) 

# Viz the top 5 emmision emmitter for each food category
gg <- 
        ggplot(df_co2, aes(x = reorder(consumption,avg_emsn), y = round(co2_emmission), fill = food_category)) +
        geom_point(shape=21, color="black", size=4, stroke=1) +
        facet_wrap(~ food_prcnt_emsn , scales = "free") +
        geom_text_repel(aes(label = paste0(country," ",co2_emmission)), 
                        vjust = -1,
                        colour = "gray38",
                        size = 3,
                        fontface = "bold") 

gg <- gg + labs(x ="Consumption", 
                y = (expression(paste(CO[2]," ", " Emmission"))),
                title = (expression(paste("4% beef consumption genrates 46% of the ", CO[2], " Emmission"))), 
                subtitle = "Each small multiple shows the top 5 Co2 emmitters by food category.",
                caption = "Note: Plot order most (Beef) to the least (Syoybeans)\nData Source: https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018 Reference\n Analytic/Viz by: abiyu.giday[at]dataRecode.com") 

gg <- gg +
        theme_minimal() +
        theme( axis.text.x = element_text(angle = 0, hjust = 1),
               strip.text.x = element_text(size = 12, colour = "darkorange3", face = "bold"),
               plot.title   = element_text(size = 20, colour = "darkorange3"),
               plot.subtitle = element_text(size = 12, colour = "gray48"),
               text = element_text(size = 10, family = "audiowide"),
               legend.position = "none") #


gg