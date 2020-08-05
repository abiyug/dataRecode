# Expore the European Power Generation data for 2016-2018
# As part of the @tidytuesday weekly share. Data source is Eeurostat
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-04/readme.md
# Aug 5, 2020

# Load library
library(tidyverse)
library(tidyr)
library(ggrepel)
library(colorspace)
library(ggthemes)
library(patchwork)
library(showtext)

# Emport font
font_add_google("Orbitron", "orbitron")

# Use font
showtext_auto()


# Get the data
df_eu_power  <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv")
df_eu_type  <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data wrangle

df_pwr <-
df_eu_power %>%
     pivot_longer(5:7) %>%
     pivot_wider(names_from = type, values_from = value) %>%
     janitor::clean_names() %>%
     select(-country, - level, year = name) 

df_type <-
df_eu_type %>%
     filter(level == "Level 1") %>% 
     pivot_longer(5:7) %>%
     pivot_wider(names_from = type, values_from = value, values_fill = list(value = 0)) %>%
     janitor::clean_names() %>%
     select(-country, - level, year = name) %>%
     inner_join(df_pwr, by = c("country_name", "year"))

df_1 <-
df_type %>% 
     pivot_longer(3:14) %>%
     group_by(country_name, name) %>% 
     summarise(avg = mean(value, na.rm = TRUE)) %>%
     ungroup() %>%
     pivot_wider(names_from = name, values_from = avg) %>%
     mutate( gwh_size = abs(exports - imports),
             net_exp_imp = ifelse(exports > imports, "net_export", "net_import")) 


df_2 <- 
df_1 %>% 
     select(-country_name, -gwh_size, -net_exp_imp) %>%
     scale() %>%
     as.data.frame() %>%
     cbind(cntry_name = df_1$country_name) %>% 
     tidyr::pivot_longer(conventional_thermal:wind) %>%
     na.omit %>%
     mutate(name = recode(name,"energy_absorbed_by_pumping" = "energy_absrbd\npumping", 
                                   "total_net_production"   =  "total\nproduction",
                                     "conventional_thermal" = "convent\nthermal",
                                          "energy_supplied" = "energy\nsupplied")) 

# fct rev for the heat map
df_2$cntry_name <- df_2$cntry_name %>% fct_rev()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Viz
# Heat map
gg_heat <-
  ggplot(df_2,  aes(x = name ,y = cntry_name, fill = value)) +
  geom_tile(color = "white", size = .6, show.legend = FALSE) + 
  ##scale_fill_gradient(low = "white",high = "red") +
  scale_fill_continuous_sequential(palette = "Peach") +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = round(value, 2)), colour = "gray30",size = 3.5,fontface = "bold") +
  #theme_gray() #+
  #theme(axis.text.x = element_text(angle = 30, vjust = -1))
  theme_fivethirtyeight() +
  labs(title = "Europe Conventional, Renewable and Nuclear Power Landscape (2016-2018)",
       subtitle = "I: Power Source Use Normalized comparison Heat Map", caption = "Source: Eurostat | graphics: @abiyugiday | https://datarecode.com/") +
  theme(plot.title    = element_text(size = 18, colour = "coral3", hjust = 0),
        plot.subtitle  = element_text(size = 14, colour = "darkcyan"), 
        plot.caption = element_text(hjust = 1, colour = "gray48", size = 12, family = "Batang"))



# Import Export 
gg_imp_exp <-
ggplot(df_1, aes(x = energy_absorbed_by_pumping, y = exports)) + 
     geom_point(aes(col = net_exp_imp, shape = net_exp_imp, size = gwh_size), show.legend = FALSE) +
     scale_color_manual(values = c("chartreuse4", "coral3")) +
     scale_y_continuous(labels = scales::comma) +
     geom_smooth(method = "loess",se = FALSE, alpha = 0.4, col = "coral2") + 
     facet_wrap(~ net_exp_imp, scale = "free", ncol = 1) +
     geom_text_repel(aes(label = country_name, family = "orbitron", fontface = "bold"), col = "gray38", size = 3) +
     labs(title = "", 
          subtitle = "II: Net Importer and Exporter Countries",
          caption = "Note: Energy values are GWh -Gigawatt hours",
          x = "Energy Absorbed Pumping (EAP)", y = "Power Exported (PE)") +
     theme_fivethirtyeight() +
     theme(
           plot.title    = element_text(size = 16, colour = "coral3", hjust = 1),
           plot.subtitle  = element_text(size = 14, colour = "darkcyan"),
           plot.caption = element_text(hjust = 0, family = "Batang"),
           strip.text.x = element_text(size = 12, colour = "gray46", face = "bold"),
         legend.direction = "horizontal", 
          legend.position = "bottom",
               legend.box = "horizontal")
    # theme(panel.background = element_rect(fill = "white"))

# Distribution by type
gg_density <-
ggplot(df_2, aes(x = value)) + 
     ##geom_density(aes(col = name), size = 1, show.legend = FALSE) + 
     geom_density(col = "gray40", size = 1, show.legend = FALSE) + 
     facet_wrap(~name, scales = "free") +
     #theme_gray()
     labs(subtitle = "III: Power Source Distribution by Type") +
     theme_fivethirtyeight() +
     theme(plot.subtitle  = element_text(size = 14, colour = "darkcyan"),
           strip.text.x = element_text(size = 12, colour = "gray46", face = "bold")) 


# Update global theme            
gg_1 <- gg_imp_exp + theme(text=element_text(size= 9, family="orbitron", colour = "gray50")) 
gg_2 <- gg_density + theme(text=element_text(size= 9, family="orbitron", colour = "gray50")) 
gg_3 <-    gg_heat + theme(text=element_text(size= 10, family="orbitron", colour = "gray40",face = "bold" )) 

# Patch and plot
( gg_3 | (gg_1/gg_2)) + plot_layout(widths = c(1.8, 1.2))

ggsave(file = "fig/EU_power_08052020_fin.png", dpi = 320, width = 19, height = 12)
