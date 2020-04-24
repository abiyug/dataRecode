# April 24, 2020
#
#HW challenge to ggplot the lady_in_red https://petapixel.com/assets/uploads/2017/09/distractedboyfriend-800x534.jpg
#
# Data = 7 colors total -   skin (1 color  - 1-brown), 
#                           hair (3 colors - 3-black , 5-brown), 
#                          dress (3 colors - 2-red, 4-blue, 6-stealblue)
#                  everythingelse(1 color  - 1-white)
# image pixle - h x w = 30 x 43  
# use Excel to drag the data

# load library
library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)

# get the data
lady_in_red <- read_csv("~/data/lady_in_red.csv")

# clean the data
df <- melt(as.matrix(lady_in_red)) %>% 
                 slice(31:1290) %>% 
                 select(x = 2, y = 1, value)

# plot the data
ggplot(df, aes(x = x, y = y, fill = value)) + #geom_tile()
        geom_tile(color = "gray78", show.legend = FALSE) + 
        geom_tile(data = (df %>% filter(value == 1)), aes(x = x, y = y, fill = value), show.legend = FALSE) + 
        geom_tile(data = (df %>% filter(value == 2)), aes(x = x, y = y, fill = value), show.legend = FALSE) + 
        geom_tile(data = (df %>% filter(value == 3)), aes(x = x, y = y, fill = value), show.legend = FALSE) + 
        geom_tile(data = (df %>% filter(value == 5)), aes(x = x, y = y, fill = value), show.legend = FALSE) + 
        geom_tile(data = (df %>% filter(value == 6)), aes(x = x, y = y, fill = value), show.legend = FALSE) + 
        geom_tile(data = (df %>% filter(value == 4)), aes(x = x, y = y, fill = value), color = "black", size = 1, show.legend = FALSE) + 
        scale_y_reverse() +
        scale_fill_manual(values = c('white','#EBB1A6','red','grey10','blue','saddlebrown','#A4B9C3')) +
        theme_void()
        
        

        
