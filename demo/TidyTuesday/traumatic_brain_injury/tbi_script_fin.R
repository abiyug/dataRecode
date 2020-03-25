# March 25, 2020
#
# Descriptoin: The three charts summarize the overall US Military Traumatic Brain Injury (TBI) for
# years 2014-2016. The charts summarize top 10% TBI severity by year, and by service - component - severity. 
#
# hashtags #ChangeYourMind, #braininjuryawarenessmonth  
# More details can be found at the Brain Injury Association Website  https://dvbic.dcoe.mil/dod-worldwide-numbers-tbi

# The data comes from the CDC and Veterans Brain Injury Center. Additional stats can be found at CDC.gov.
#


# load libary
library(tidyverse)
library(tidyr)
library(scales)
library(skimr)
library(ggrepel)
library(lemon)

# on-board data
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

#~~~~~~~~~~1/
# data
df_mil <-
        tbi_military %>%
        group_by(service, component, severity ) %>%
        summarise(tot_diag = sum(diagnosed)) %>%
        mutate(prcnt = tot_diag/sum(tot_diag)) %>%
        ungroup() %>%
        mutate(severity = forcats::fct_reorder(as.character(severity), prcnt, .desc= TRUE)) %>%
        filter(severity != "Reserve") 
#viz
gg_mil <-        
        ggplot(df_mil, aes(x = reorder(severity, tot_diag), y = tot_diag, fill = service)) +
        geom_bar(stat="identity", position = "dodge",color="gray56") +
        facet_wrap(service ~ component, scales = "free", ncol = 2) +
        scale_y_log10() +
        coord_flip() +
        geom_text(aes(label = scales::percent(prcnt)), 
                  position = position_dodge(0.9), 
                  hjust = 1,
                  colour = "white",
                  size = 3,
                  fontface = "bold") 

# apply service colors
gg_mil <- gg_mil + scale_fill_manual(values=c("#5d8aa8", "#4b5320", "#A77C29", "#000048")) 
gg_mil <- gg_mil + guides(fill=guide_legend(title=""))

# Add title/caption
gg_mil <- gg_mil + labs(x = "", 
                        y = "",
                        title = "2006-2014 Military Traumatic Brain Injury (TBI) by sevirty",
                        subtitle = "",
                        caption = "Data Source: https://dvbic.dcoe.mil/dod-worldwide-numbers-tbi | graphics: @abiyugiday") 

# Add/modify theme
gg_mil <- gg_mil + theme_solarized() +
        theme(legend.position = "bottom",
              plot.title    = element_text(size = 22, colour = "royalblue1", family = "Batang"),
              plot.subtitle  = element_text(size = 14, colour = "gray48"),
              plot.caption  = element_text(size =  12, colour = "gray48"),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank(),
              legend.text  = element_text(colour = "gray48", size = 12, face = "bold"),
              axis.text    = element_text(size = 10, colour = "gray48", face = "bold"),  # changes both x&y
              strip.text.x = element_text(size = 12, colour = "gray46", face = "bold")) 

# reposition guide
reposition_legend(gg_mil, 'center', panel='panel-2-5')

#~~~~~~~~~~2/
# Top 10%
# data
df_tbi_top10 <-
tbi_military %>%
        mutate_all(~str_replace_all(., "Air ", "Air_")) %>%
        mutate(diagnosed = as.numeric(diagnosed)) %>%
        group_by(service, component, severity, year) %>%
        summarise(tot_diag = sum(diagnosed)) %>%
        mutate(prcnt = tot_diag/sum(tot_diag),
               prcntile = scales::percent(prcnt)) %>%
        ungroup() %>%
        mutate(vars = paste0(service,"_",component,"_",severity)) %>% 
        group_by(year) %>% 
        arrange(desc(prcnt)) %>% 
        filter(prcnt > quantile(prcnt, probs = .90))

gg_tbi_top10 <-
ggplot(df_tbi_top10, aes(x = prcnt , y =reorder(factor(vars), prcnt ))) +
        geom_point() +
        geom_segment(aes(yend=vars,xend=0)) +
        geom_text(aes(x = prcnt, label = prcntile), hjust = -.2, size = 3) +
        facet_wrap(year ~ ., scales = "free_y", ncol = 1) +
        scale_x_log10() 

# Add title/caption
gg_tbi_top10 <- gg_tbi_top10 + labs(x = "", 
                        y = "",
                        title = "Top 10% Military Traumatic Brain Injury (TBI) by year",
                        subtitle = "",
                        caption = "Data Source: https://dvbic.dcoe.mil/dod-worldwide-numbers-tbi | graphics: @abiyugiday") 

# Add/modify theme
gg_tbi_top10 <- gg_tbi_top10 + theme_solarized() +
        theme(legend.position = "bottom",
              plot.title    = element_text(size = 22, colour = "royalblue1", family = "Batang"),
              plot.subtitle  = element_text(size = 14, colour = "gray48"),
              plot.caption  = element_text(size =  12, colour = "gray48"),
              axis.text.x  = element_blank(),
              axis.ticks.x = element_blank(),
              legend.text  = element_text(colour = "gray48", size = 12, face = "bold"),
              axis.text    = element_text(size = 10, colour = "gray48", face = "bold"),  # changes both x&y
              strip.text.x = element_text(size = 12, colour = "gray46", face = "bold")) 
gg_tbi_top10


#~~~~~~~~~~3/
# all
# from 2006-2014
#data
df_tbi_all <-
tbi_military %>%
        mutate_all(~str_replace_all(., "Air ", "Air_")) %>%
        mutate(diagnosed = as.numeric(diagnosed)) %>%
        group_by(service, component, severity) %>%
        summarise(tot_diag = sum(diagnosed)) %>%
        mutate(prcnt = tot_diag/sum(tot_diag),
               prcntile = scales::percent(prcnt)) %>%
        ungroup() %>%
        mutate(vars = paste0(service,"_",component,"_",severity)) %>% 
        select(vars, tot_diag, prcnt, prcntile ) %>%
        arrange(desc(prcnt)) 

#Viz
gg_tbi_all <-
ggplot(df_tbi_all, aes(x = prcnt , y =reorder(factor(vars), prcnt ))) +
        geom_point() +
        geom_segment(aes(yend=vars,xend=0)) +
        geom_text(aes(x = prcnt, label = prcntile), hjust = -.2, size = 3) +
        scale_x_log10() 


# Add title/subtutle/caption
gg_tbi_all <- gg_tbi_all + labs(x = "", 
                                y = "",
                                title = "2014-2016Over all Military TBI by Service & Sevirty",
                                subtitle = "Traumatic Brain Injury (TBI)",
                                caption = "Data Source: https://dvbic.dcoe.mil/dod-worldwide-numbers-tbi | graphics: @abiyugiday") 

# Add/modify theme
gg_tbi_all <- gg_tbi_all + theme_solarized() +
                theme(legend.position = "bottom",
                      plot.title    = element_text(size = 22, colour = "royalblue1", family = "Batang"),
                      plot.subtitle  = element_text(size = 14, colour = "gray48"),
                      plot.caption  = element_text(size =  12, colour = "gray48"),
                      axis.text.x  = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.text  = element_text(colour = "gray48", size = 12, face = "bold"),
                      axis.text    = element_text(size = 10, colour = "gray48", face = "bold"),  # changes both x&y
                      strip.text.x = element_text(size = 12, colour = "gray46", face = "bold")) 
gg_tbi_all
