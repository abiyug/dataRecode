# May 27, 2020
#
# Classifying and labeling the cocktails dataset with the KNN(nearest neighbor algo)
#
# For this, the feature selection were limited to cocktails that contain alcohol and and ingredients
# that contains Vodka and Orange-Juice (because  they are the most commonly found ingredients in the dataset). 
# Three categories of cocktails were also selected for labeling and clustering, including 
# "Cocktail, Ordinary Drink, Shot"; The KNN algorithm is applied with optimal number of 
# clusters with R's kmeans function
# 

# 
# library load
library(tidyverse)
library(ggforce)
library(ggthemes)
library(colorspace)

# load the data
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')

# Feature selection/data cleaning
# Select cocktails that contain 'alcohol only; clean up the measurment variable that use ounce (oz) as metic
# convert the measurment to numeric class

df_vod_sug <-
  cocktails %>% 
  filter(alcoholic == "Alcoholic") %>%
  select(category, ingredient, measure) %>%
  separate(measure, c("amount", "metric"), sep = " ") %>%
  filter(metric == "oz") %>%
  mutate(amount = case_when(str_detect(amount, "1/2") ~ ".5",
                            str_detect(amount, "1/4") ~ ".25",
                            str_detect(amount, "1/3") ~ ".33",
                            str_detect(amount, "2/3") ~ ".67",
                            str_detect(amount, "3/4") ~ ".75",
                            str_detect(amount, "2-3") ~ "3",
                            str_detect(amount, "3-4") ~ "4",
                            str_detect(amount, "8-10") ~ "10",
                            str_detect(amount, "10-12") ~ "12",
                            str_detect(amount, "70ml/2fl") ~ "2.4",
                            TRUE ~  amount)) %>%
  select(-metric)

# Vodka and Orange-Juice are two of the most common ingredients that are used in coktail making
# The following script prepares the data set that will be used for KNN Clustering for cocktail 
# categories that include only the following: "Cocktail, Ordinary Drink, Shots"; 
# some cleanup of the categorical names
df_km <-        
  df_vod_sug %>% 
  filter(ingredient %in% c("Vodka","Orange juice")) %>% 
  mutate(amount = as.numeric(amount)) %>%
  pivot_wider(names_from = ingredient, values_from = amount) %>% 
  janitor::clean_names() %>%
  unnest(vodka) %>% 
  unnest(orange_juice) %>% 
  select(category, vodka, orange_juice) %>%
  mutate(category = str_replace(category,"\\/", ""),
         category = str_squish(category)) %>%
  mutate_if(is.character, as.factor) 

# use the kmeans function to build the cluster with optimal center selected (how to select not included here)
k_means <-
  df_km %>%
  select(-category) %>%
  kmeans(centers = 6)  # 5 cluster 87.5% ; 6 cluster = 89.1%; 7 cluster 91.4; 8 cluster = 90.0

# Prepare the dataset to build the visualization 
df_three_clustered <- data.frame(df_km, cluster = factor(k_means$cluster))

# Centroid data set
df_center <- as_tibble(k_means$centers)

# Vizualize
gg_clust <-
  ggplot(df_three_clustered, aes(vodka, orange_juice, color = cluster , shape = category)) + #, shape = category
  geom_jitter() +
  geom_mark_ellipse(aes(fill = category, 
                        label = category, 
                        filter = category == c('Cocktail',  'Ordinary Drink', 'Shot'))) +
  geom_point(data = df_center, aes(x = vodka, y = orange_juice), col = "red", size = 5, shape = 4) +
  scale_x_continuous(limits = c(0, 3.5))

# title/axis text
gg_clust <- 
  gg_clust + labs(title = "KNN Classification for Cocktails That Contain Vodka & Orange Juice", 
              subtitle = "X mark the spot of the respective cluster's centroid for each cluster",
              caption  = paste0("Source: https://www.kaggle.com/ai-first/cocktail-ingredients?select=all_drinks.csv\ngraphics: @abiyugiday | ", Sys.Date()),
              x = "Vodka", 
              y = "Orange Juice") 

# theme/customize
gg_clust <-
  gg_clust + theme_gdocs() +
  theme(#legend.direction = "horizontal",
        #legend.position = c(.3,.4),
        plot.title    = element_text(size = 22, colour = "gray48", family = "Batang"),
        plot.subtitle = element_text(size = 12, colour = "gray48"),
        plot.caption  = element_text(size =  12, colour = "gray48"),
        axis.text= element_text(size = 12, colour = "gray48", face = "bold") )

# plot
gg_clust
