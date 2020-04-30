# April 30, 2020
#
# A TidyTuesday for Week APril 
#
# This data comes from Playbill. Weekly box office grosses comprise data on revenue and 
# attendance figures for theatres that are part of The Broadway League, an industry 
# association for, you guessed it, Broadway theatre.
#
# The objective for this script is to identify the top 10 highest grossing broadway shows
# From 1985-2019, and fit regression model that will predict the price of tikcet for each 
# of the top 10 shows. Findings are Visualized with ggplot 2. The small multiple scatter plots
# are ordred with R^2 (googness-Of-fit value) from highest, reliable model, to the least.
#
# Working with the "broom" & "tidymodels" package to fit multiple regression to nested data, 
# and extract coeficient values in tidy table for efficient comparison and more..
#


library(tidyverse)
library(tidymodels)
library(broom)

# Get the data
grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')

# how much each show gross per year from 1985-2019
show_yr_gross <- 
  grosses %>% 
  mutate(year = year(week_ending)) %>%
  filter(year <= 2019) %>% 
  ungroup() %>% 
  select(show, year, week_ending, weekly_gross) %>%
  group_by(show, year) %>%
  summarise(by_year_gross = sum(weekly_gross)) %>% 
  arrange(desc(by_year_gross, year)) %>%
  ungroup() 

# Which show gorssed the most - Hamilton is head and shoulders above..
ggplot(show_yr_gross, aes(year, by_year_gross, group = show)) +
  geom_line(col = "gray56") +
  geom_line(data = (show_yr_gross %>% filter(show == "Hamilton")),aes(year, by_year_gross, group = show), col = "red") +
  labs(title = "Hamilton Grossed The Most out of the Gate!")

# top 10 by revenew by year - with at least 5 year run distict()
top_10 <-
  show_yr_gross %>%
  group_by(show) %>%
  arrange(desc(by_year_gross)) %>%
  mutate(count =  n()) %>% 
  filter(count >= 5) %>% 
  select(show) %>%
  distinct() %>%
  ungroup() %>%
  slice(1:10)


# Let us prepare the data we will use to fit the linear regression model (2020 Not included)
# This chunk will FE year/month, select variables used to predict 'average' avt_ticket_price
df_regression_year_Month <-
  grosses %>% 
  mutate(year = year(week_ending),
         month = month(week_ending)) %>%
  filter(year != 2020) %>%
  inner_join(top_10, by = "show") %>%
  select(show, year, month, weekly_gross, avg_ticket_price,seats_in_theatre, pct_capacity ) %>%
  pivot_longer(cols = c("weekly_gross", "avg_ticket_price",
                        "seats_in_theatre", "pct_capacity")) %>%
  group_by(show, name, year, month) %>%
  summarise(avg = mean(value)) %>%
  pivot_wider(names_from = name, values_from = avg) %>%
  ungroup() %>%
  mutate_if(is.character, as.factor)

# This chunk nest selected variables for each show, and fit linear model for each nested data
# and add variables for the model and extract coef and other model parameteres
# The year and pct_capacity are selected regressors for the avg_tkt_price

grosses_regression_2 <-
  df_regression_year_Month %>%
  nest(data = c(year, month, avg_ticket_price, 
                pct_capacity, seats_in_theatre, 
                weekly_gross)) %>% #pluck("data") %>% pluck(1)
  mutate(fit = map(data, ~lm(avg_ticket_price ~ year + pct_capacity, data = .x)),
         tidied = map(fit, broom::tidy),
         glanced = map(fit, glance),
         augmented = map(fit, augment)) 

# Extract values that will be used for data Viz
# Inparticular looking for variables with siginficance - pvalue less than 0.05 ; and high r.sqr adj.r.sqr
df_fits_2 <-
  grosses_regression_2 %>% 
  ungroup() %>% 
  select(show, augmented, glanced) %>%
  unnest(augmented) %>% 
  unnest(glanced) %>% 
  select(show, year, avg_ticket_price, avg_ticket_price,.fitted , r.squared, p.value ) %>%
  janitor::clean_names() %>% 
  mutate(show_2 = paste0(show,"  ","R^2: ", format(round(r_squared, 2), nsmall = 3))) %>%
  mutate(show_2 = forcats::fct_reorder(as.character(show_2), r_squared, .desc= TRUE)) 

# Viz
gg <- 
ggplot(df_fits_2, aes(x = avg_ticket_price, y = fitted, col = factor(show))) +
  geom_point(size = 2, alpha = 0.8, stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, col = "royalblue4") +
  facet_wrap(~ show_2, scales = "free", ncol = 3) 



# Customize the theme
gg <- gg +
  theme(
    plot.title    = element_text(size = 18, colour = "gray40", family = "Batang"),  #Center title with hjust.
    plot.subtitle = element_text(size = 14, colour = "gray48", family = "Batang"),
    plot.caption  = element_text(size =  12, colour = "gray48", family = "Batang"),
    strip.text    = element_text(size = 10, colour = "gray40", , family = "Batang", face = "bold"),
    axis.text     = element_text(size = 9, colour = "gray48", face = "bold"),  
    axis.title.x  = element_text(size = 9, colour = "gray48", face = "bold"),
    axis.title.y  = element_text(size = 9, colour = "gray48", face = "bold"),
    legend.position = "none") 

# Add labels
gg <- gg + labs(x = "Actual price for the show in US $", 
                y = "Predicted show price in US $",
                title = "Fitting Linear Regression Model to Predict Top 10 Broadway show ticket prices.",
                subtitle = "Small Multiple plots are ordred with highest R^2 to the lowest. The higher the R^2 value\nthe more accurate the prediction.",
                caption = "Data Source: https://www.playbill.com/grosses | Analysis/graphics: @abiyugiday ") 
gg