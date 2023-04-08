################################################################################
#
#  2021-2022 English Premier League data Exploration
#  For TidyTuesday data Viz Challenge
#  Date: April 4, 2023
#
#################################################################################

# Libraries used in this analysis and graphic genration
libs <- c(
        "tidyverse", "lubridate", "scales",
        "gghighlight", "ggimage", "showtext"
          )

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
        install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# get the fonts to be used from google
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sysfonts::font_add_google("Orbitron", "orbitron")
sysfonts::font_add_google("Outfit", "outfit")
sysfonts::font_add_google("Roboto", "roboto")
sysfonts::font_add_google("Vesper Libre", "vesper")


# load the font
showtext::showtext_auto()
font_families()

#font set
fam_1 <- "orbitron"
fam_2 <- "outfit"
fam_3 <- "roboto"
fam_4 <- "vesper"

# set repeat aesthetic values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hj = 0   
txt_sz = 5.5
txt_clr = "#5f0f4e"



# 1/  import the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Primer League data
soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

# club colors and logo
club_logo_colr <- read_csv("~/tidyTeusday/data/club_logo_colr.csv") %>%
        select(Team  = 1, everything())

# Primer Leagu trophy image
trophy_img <- "https://w7.pngwing.com/pngs/492/150/png-transparent-silver-barclays-trophy-premier-league-uefa-champions-league-manchester-city-f-c-trophy-leicester-city-f-c-premier-league-trophy-uefa-champions-league-manchester-city-f-c-leicester-thumbnail.png"


# 2/  data cleaning and feature engineering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.a/ change the date to POSIXct, get the week, year ang generate a unique id with the two vars
df_1 <-
soccer %>% 
        as_tibble() %>%  
        mutate( Date = as.POSIXct(Date, format = "%d/%m/%Y"), 
                wk_no = format(Date, '%V'),
                year  = year(Date), 
                id = paste0(year,"_",wk_no))  %>%
        select(id, everything()) #%>%

# 2.b/  a cunction that will accumulate total points, wins, loss and
acumlt_season_points <- function(df, x) {
        
        library(stringr)
        
        # Subset the first six columns of the input data frame
        df_1 <- df[,1:6]
        
        # Filter rows where any variable matches the given pattern
        df_2 <- df_1[apply(df_1, 1, function(row) any(str_detect(row, pattern = x))), ]
        
        # Create a new column called "win_loss" using case_when
        df_3 <- transform(df_2, win_loss = 
                                  ifelse(HomeTeam == x & FTHG > FTAG, "win",
                                         ifelse(AwayTeam == x & FTAG > FTHG, "win",
                                                ifelse(HomeTeam == x & FTHG < FTAG, "loss",
                                                       ifelse(AwayTeam == x & FTAG < FTHG, "loss", "draw")))))
        
        # Create a new column called "points" using ifelse
        df_4 <- transform(df_3, points = 
                                  ifelse(win_loss == "loss", 0,
                                         ifelse(win_loss == "win", 3, 1)))
        
        # Summarize weekly points by grouping by "id"
        df_5 <- aggregate(points ~ id, data = df_4, FUN = sum)
        
        # Create a new column called "cumulative_point" using cumsum
        df_6 <- transform(df_5, cumulative_point = cumsum(points))
        
        # Create a new column called "game_num" using seq_along
        df_7 <- transform(df_6, game_num = seq_along(cumulative_point))
        
        # Add a new column called "Team" with the value of x
        df_8 <- transform(df_7, Team = x)
        
        # Remove grouping variables
        df_9 <- ungroup(df_8)
        
        # Return the final data frame
        return(df_9)
}

# 2.c/  use the function to build tpoints, wins, loss and

df_all_team_wkly_point <- 
        map_dfr(team_name, acumlt_season_points)

# 2.d/  cobmbine result,logo and adjust colors

soccer_2021_2022 <-
df_all_team_wkly_point %>%
        group_by(Team) %>% 
        summarise(tot_point = max(cumulative_point)) %>%
        ungroup() %>% 
        arrange(desc(tot_point)) %>%
        mutate(ranks = 1:n()) %>%
        inner_join(df_all_team_wkly_point) %>%
        inner_join(club_logo_colr, by = "Team") %>%
        mutate(Team = fct_inorder(Team, ranks),
               hex_code = recode(hex_code, "#FFFFFF" = "#132257"), # totenham colors
               hex_code = recode(hex_code, "#FBEE23" = "#11210C"), # watford color 
               hex_code = recode(hex_code, "#FFCD00" = "#AC944D")  # Leeds
        )


# 2.e/ get the team logo and prepare the x/y coordinates where the logos will reside (top/left)
team_logo <-
soccer_2021_2022 %>%
        filter(game_num == 5) %>% 
        select(Team, game_num, logo_link, cumulative_point) %>% 
        mutate(cumulative_point = 75)

# 2.e Build a table with win, draw, loss, Goals Scored, Goal Allowed and win percentage for labels 
df_w_d_l <-
df_win_tie_loss %>%
        select(Team, win, draws, loss) %>%
        inner_join(soccer_2021_2022) %>%
        inner_join(df_win_tie_loss %>% select(Team, GoalsScored, GoalsAllowed, tot_points, win_prcnt)) %>% 
        mutate(Team = fct_inorder(Team, ranks),
               W_D_L =  paste0(win,"_",draws,"_",loss),
               GS_GA =  paste0(GoalsScored,"_",GoalsAllowed),
               win_prcnt = scales::percent(win_prcnt),
        ) %>%
        select(Team, W_D_L, GS_GA, win_prcnt, ranks, win_prcnt) %>%
        distinct()

# 3/ geneate the data Viz with ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(soccer_2021_2022, aes(x = game_num, y = cumulative_point, group = Team)) +
        geom_line(aes(col = hex_code), show.legend = FALSE, size = 1) +
        gghighlight(label_params = list(fontface = "bold", size = 8, alpha = 0.8)) +
        geom_image(data = team_logo, aes(x = game_num, y = cumulative_point, image = logo_link), 
                   size = .30)  +
        geom_image(data =  team_logo %>% 
                           filter(Team == "Man City") %>%
                           mutate(trophy = trophy_img), 
                   aes(x = 30, y = 12, image = trophy ), size = .2) +
        geom_text(data = soccer_2021_2022 %>% 
                          group_by(Team) %>% 
                          slice_max(cumulative_point, n = 1),
                  aes(x = 5.8, y  = 52, label =  paste0("Points: ", cumulative_point)), 
                    col = txt_clr, size = txt_sz + 2, fontface = "bold", vjust = hj) +
        geom_text(data = df_w_d_l, aes(x = 5.8, y = 46, label = paste0("W_D_L: ", W_D_L)), 
                   col = txt_clr, size = txt_sz, fontface = "bold", vjust = hj) +
        geom_text(data = df_w_d_l, aes(x = 5.8, y = 40, label = paste0("GS_GA: ", GS_GA)), 
                   col = txt_clr, size = txt_sz, fontface = "bold", vjust = hj) +
        geom_text(data = df_w_d_l, aes(x = 5.8, y = 34, label = paste0("Win_%: ", win_prcnt)), 
                   col = txt_clr, size = txt_sz, fontface = "bold", vjust = hj) +
        facet_wrap(~ Team) +
        labs(x = "Game Numbers (1-38)", 
             y = "Cumulative Points",
             title = "30th Season (2021-2022) English Professional Premier League Results",
             subtitle = str_wrap("In the league there were 20 teams, each team played the other 19 teams twice for the total of 38 games. A win gets a team 3 points, a draw 1 point and loss zero point. For each team, a small multiple plot shows Win, Draw, Loss (W_D_L) record - Goals Scored/Goals Allowed (GS_GA) - total points at the end of the season and the wining percentage for the season.",125),
             #caption  = "Data Source:  Evan Gower/Kaggle | Graphics: @abiyugiday | Tidytuesday: 2023-04-04"
             caption  = "Data Source:  Evan Gower/Kaggle | Graphics: @dataRecode"

        ) +
        scale_color_identity() +
        scale_x_continuous(breaks = seq(0, 38, by = 8)) +
        scale_y_continuous(breaks = seq(0, 94, by = 15)) +
        theme(legend.position = "none") +
        #theme_void() +
        theme_light(base_family = fam_2) +
        theme(   plot.title = element_text(family = fam_1, size = 52, colour = "#02558b", face = "bold", hjust = 0.5),
              plot.subtitle = element_text(family = fam_4, lineheight = 0.35, size = 30, colour = "gray44", face = "bold", hjust = 0.5),
               plot.caption = element_text(family = fam_3, size = 20, colour = "#02558b", face = "bold"),
            plot.background = element_rect(colour = NA, fill = "#eddbc0"), #eddbc0"),
                  axis.text = element_text(family = fam_4, size = 16, colour = "gray44", face = "bold"), 
                 axis.title = element_text(family = fam_4, size = 20, colour = "gray44", face = "bold"), 
           strip.background = element_blank(),
                 strip.text = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_line(linetype = "dashed"))

