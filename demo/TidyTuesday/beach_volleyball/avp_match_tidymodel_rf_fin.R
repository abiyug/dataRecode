# May 20, 2020
#
# About AVP tournament from Wikipedia
# The Association of Volleyball Professionals (AVP) is the biggest and longest-running 
# professional beach volleyball tour in the United States. Founded in 1983, the AVP 
# is headquartered in Newport Beach, California. The AVP operates as a 3-tiered 
# development system with AVPFirst, a youth program; AVPNext, a developmental circuit;
# and the AVP Pro Beach Volleyball Tour itself. 
# 
# About the data used for modeling
# Although the raw data contains both FIVB and AVP match results, for this analysis the data
# includes only AVP matches from 2005 to 2019 and only contains Semifinals, Finals, Bronze 
# Medal, Gold Medal matches. The main reason for the filter is because the variables used
# for the classification model starts at 2005 and the data show only AVP tournament has a 
# more complete set.seed()
#
# The 'tidymodel' frame work is used to split/train/validate/predict with the data. RandomeForest
# classification algorithm is used to train the model.
#



# Load libraries used
library(tidyverse)
library(tidymodels)
library(ranger)     # RF model engine
library(randomForest)
library(hms)
library(vip)
library(skimr)
library(colorspace)
library(patchwork)

#~~~~~~~~~~~~~~~~~~~~~~
# Custom theme for Viz
theme_abiyu <- function () { 
  theme( # heading title/sub/cap
    plot.title    = element_text(size = 16, colour = "gray48", family = "Batang", hjust = 0.5),
    plot.subtitle  = element_text(size = 14, colour = "gray48"),
    plot.caption  = element_text(size =  12, colour = "gray48"),
    # x axis text 
    axis.text     = element_text(size = 10, colour = "gray48", face = "bold"),  # changes both x&y
    axis.title.x  = element_text(size = 10, colour = "tan1", face = "bold"),
    axis.title.y  = element_text(size = 10, colour = "tan1", face = "bold"),
    # strip - facet heading
    strip.text.x = element_text(size = 12, colour = "chocolate4", face = "bold"),
    strip.text.y = element_text(size = 12, colour = "chocolate4", face = "bold"),
    strip.background = element_rect(fill = "transparent", colour = NA), 
    #legend 
    legend.title = element_text(size = 12, colour = "gray40", face = "bold" ),
    legend.text = element_text(size = 12, colour = "gray40", face = "bold"),
    # the panel
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_line(colour = "grey92"),
    panel.grid.minor = element_line(size = rel(0.5)),
    panel.border = element_rect(fill = NA, colour = "NA"),
    legend.position = "none")}
    

#~~~~~~~~~~~~~~~~~~~~~

# Get the data
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Transform/clean the data for ML
# Win/Loss is the target variable and is converted to factor
df_ml <- 
    vb_matches %>% 
    mutate(key = row_number()) %>%#,
           #time_min = as.numeric(hms(duration))/60) %>%
    select(key, circuit, year, bracket, starts_with("w_"), starts_with("l_")) %>% 
    filter(bracket %in% c("Semifinals", "Finals", "Bronze Medal", "Gold Medal"),
           circuit == "AVP",
           year >= 2005) %>%
    select(-contains("birthdate") , 
           -contains("player"), 
           -contains("country") ,
           -contains("rank"),
           -key, -circuit, -bracket) %>% 
    pivot_longer(c(-year)) %>% #, cols = w_p1_age:l_p2_tot_digs) %>%
    separate(name,c("win_loss","var2", "var3", "var4"),sep="_") %>% 
    filter(!is.na(var4)) %>% #remove age/height
    pivot_wider(names_from = var2, values_from = value, values_fill = list(values = 0)) %>%
    unnest() %>% 
    mutate(avg_value = (p1 + p2) / 2 ) %>%  #find tidyvers way of doin this
    select(-p1, -p2, - var3) %>%
    pivot_wider(names_from = var4, values_from = avg_value, values_fill = list(values = 0)) %>%
    unnest() %>% 
    mutate_if(is.character, as.factor)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1/ split the data training/test  80% training 20 % test
set.seed(seed = 1122) 

train_test_split <-
  rsample::initial_split( data = df_ml, prop = 0.80 ) 

# 2/ Prep the recipe to be used 
volbl_recipe <- 
  training(train_test_split) %>% #skim()
  drop_na() %>%                # skim()
  recipe(win_loss ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

# 3/ Bake step = apply the recipie to the 

volbl_testing_baked <- 
volbl_recipe %>%
      bake(testing(train_test_split)) %>% 
      drop_na()
#glimpse(volbl_testing_baked)

# 4/ Prep the data for ML
volbl_training <- juice(volbl_recipe)

glimpse(volbl_training)

# 5/ Model Training
# In the example below, the rand_forest() function is used to initialize a 
# Random Forest model. To define the number of trees, the trees argument 
# is used. To use the ranger version of Random Forest, the set_engine() 
# function is used.

volbl_rforest <-
  rand_forest(trees = 250, mode = "classification") %>%
  set_engine("randomForest") %>%  # ranger returns ->Error: Missing data in columns
  fit(win_loss ~ ., data = volbl_training)

volbl_rforest

# 
# 6/ Viz the confusion matrix
# Extract the confution matrix and plot the hard way
gg_confusion <- 
  as_tibble(volbl_rforest$fit$confusion) %>%
  select("Loss" = 1, "Win" = 2, "Class_error" = 3) %>%
  pivot_longer(everything()) %>%
  filter(name != "Class_error") %>%
  mutate(ss = c("loss", "loss", "win", "win")) %>%
ggplot(aes(name, ss, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label =value), colour = "white", alpha = 1, size = 8) +
  theme_abiyu() +
  theme(legend.position = "none") +
  scale_fill_continuous_diverging(palette = " Purple-Brown") +
  labs(x = "", y ="", title = "True Positive (Sensetivity) Rate 85%\nTrue Negative (Specificity) Rate 82%")

# 7/ Model Validation 
## Accuracey(AUC) and KAP !!
volbl_rforest %>%
  predict(volbl_testing_baked) %>%
  bind_cols(volbl_testing_baked) %>%
  metrics(truth = win_loss, estimate = .pred_class)

## Per classifier metrics
volbl_rforest %>%
  predict(volbl_testing_baked, type = "prob") %>%
  glimpse()

# 8/  append the predictions to the baked testing data se
volbl_probs <- 
  volbl_rforest %>%
  predict(volbl_testing_baked, type = "prob") %>%
  bind_cols(volbl_testing_baked)

glimpse(volbl_probs)

# 9/ Extract the Sensetivity/Specificty for both winning and loosing predictions
rf_auc_win <- 
  volbl_probs %>%
  roc_curve(win_loss, .pred_w) %>% 
  mutate(rf_w_l = "rf_auc_win")

rf_auc_loss <-
  volbl_probs %>%
  roc_curve(win_loss, .pred_l) %>% 
  mutate(rf_w_l = "rf_auc_loss")

## Visualize the Area Under the CUrve for both AUC

gg_auc <-
  bind_rows(rf_auc_win, rf_auc_loss) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = rf_w_l)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  #coord_equal() + 
  facet_wrap(~ rf_w_l) +
  scale_color_viridis_d(option = "plasma", end = .6) +
  labs(title = "Aaccuracy 0.827 and Kap 0.654",
       x = "False Posetive",
       y = "True Posetive") +
  theme_abiyu()

# 10 Variable IMportance

df_imp <- 
  vip::vi(volbl_rforest) %>%
  mutate(prcnt = scales::percent(Importance))

# Viz
gg_vip <-
  ggplot(data = df_imp, aes(x = reorder(Variable,Importance) , y = Importance, fill = ..x..)) +
  geom_bar(stat = "identity") +
  scale_fill_continuous_diverging(palette = " Purple-Brown") +
  coord_flip() +
  geom_text(aes(label = prcnt), 
            position = position_dodge(0.9), 
            hjust = 1.2,
            colour = "white",
            size = 4,
            fontface = "bold") +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "", y = "", title = "variable importance") +
  theme_abiyu()

#  Patch the visualizations together with patchwork

# patc configuration
gg_all <- (gg_vip | gg_confusion) / gg_auc

# apply theme
gg_all + 
  plot_annotation(
  title   = 'Classification Modeling for Win/Loss prediction of AVP Beach Vollyball Matches',
  caption = 'Source: @tidytuesday beach vollyball | graphics: @abiyugiday | May 20, 2020\nNote: The data includes AVP matches from 2005 to 2019 and only icludes Semifinals, Finals, Bronze Medal, Gold Medal matches.',
  theme = theme(plot.title    = element_text(size = 20, colour = "saddlebrown", family = "Batang"),
                plot.caption  = element_text(size =  12,colour = "saddlebrown", family = "Batang")),
    tag_levels = 'A'
) &
  theme(plot.tag = element_text(size = 16, , face = "bold", colour = "gray46"))

