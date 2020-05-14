# May 13, 2020
# Volcanic activity over the last 2020 years

# A volcano is a rupture in the crust of a planetary-mass object, such as Earth, that allows hot lava, 
# volcanic ash, and gases to escape from a magma chamber below the surface.
# 
# Earth's volcanoes occur because its crust is broken into 17 major, rigid tectonic plates that 
# float on a hotter, softer layer in its mantle. Therefore, on Earth, volcanoes are generally found where
# tectonic plates are diverging or converging, and most are found underwater.
# 
# Erupting volcanoes can pose many hazards, not only in the immediate vicinity of the eruption. 
# One such hazard is that volcanic ash can be a threat to aircraft, in particular those with jet engines where
# ash particles can be melted by the high operating temperature; the melted particles then adhere to the turbine
# blades and alter their shape, disrupting the operation of the turbine. Large eruptions can affect temperature
# as ash and droplets of sulfuric acid obscure the sun and cool the Earth's lower atmosphere (or troposphere); 
# however, they also absorb heat radiated from the Earth, thereby warming the upper atmosphere (or stratosphere). 
# Historically, volcanic winters have caused catastrophic famines.
#
#
#
# load the library
library(tidyverse)
library(ggthemes)
library(colorspace)
library(lubridate)

# load the data
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
View(eruptions)


df_erup2 <-
        eruptions %>%
        filter(start_year >= 0000) %>% 
        arrange(start_year) %>% 
        mutate(year = as.Date(as.character(start_year), format = "%Y"),
               year_lbl = format(year, "%Y")) %>% 
        mutate(year_chunk = santoku::chop_width(start_year, 10)) %>% 
        group_by(year_chunk) %>%
        mutate(number_of_volcano = n()) %>%  
        ungroup()

gg_1 <-        
ggplot(df_erup2, aes(x =year, y = number_of_volcano)) + 
        geom_point(aes(size = number_of_volcano, fill = number_of_volcano), shape = 21, show.legend = TRUE) +  
        geom_vline(xintercept = as.numeric(ymd("1700-01-01")), size = 1.5, col = "cadetblue3", linetype="dashed")  


# color the data points
gg_1 <- gg_1 + guides(fill = 'legend') 

gg_1 <-
        gg_1  + scale_fill_continuous_sequential(palette = "SunsetDark", 
                                                 guide = guide_legend(title = "Number of Volcanos",
                                                                      title.position = "top",
                                                                      label.position = "bottom",
                                                                      title.hjust = 0.5
                                                 ))

# Add text
gg_1 <- 
        gg_1 + annotate(geom = "text", x = ymd(14000101), y = 300, label = "Exponantial number of volcanic\n actvity after 1700", hjust = "center", col = "ivory3", family = "Batang")


# Add Arrow
gg_1 <- 
        gg_1 + annotate(
        geom = "curve", x = ymd(16000101), y = 320, xend =ymd(17000101), yend = 370, 
        curvature = -.2, col = "ivory3", arrow = arrow(length = unit(4, "mm"))) 


gg_1 <- 
        gg_1 + labs(title = "Number of Eruption Every 10 Years From 0000-2020 AD", 
                    subtitle = "The number of volcanic activity around the world has shown 'exponantial' growth since 1700 to present.",
                    caption  = paste0("Source: https://volcano.si.edu | graphics: @abiyugiday | ", Sys.Date()),
                    x = "years", 
                    y = "Number of Volcano") 
gg_1 <-
        gg_1 +  theme_hc(bgcolor = "darkunica")  +
        theme(legend.direction = "horizontal",
              legend.position = c(.3,.4),
              plot.title    = element_text(size = 22, colour = "ivory3", family = "Batang"),
              plot.subtitle = element_text(size = 12, colour = "ivory3"),
              plot.caption  = element_text(size =  12, colour = "ivory3"),
              axis.text= element_text(size = 12, colour = "ivory3", face = "bold") )

gg_1
# Saved as "scatter_plot_volcano_0000_2020.png"

