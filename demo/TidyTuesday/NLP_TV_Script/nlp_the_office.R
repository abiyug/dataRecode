# Date March 18, 2020
#
# The analysis  identify the top three writers with the highest imdb ratings for the TV show "The office". 
# And used the tidtytext R package to examine the frequently used two word terms and their relationships, and identify (in a wordcloud) 
# the unique word used by each writer that are not used by the other two.
#
#Data Source: The schrute R package for The Office transcripts and data.world for IMDB ratings.

#load the library

library(tidyverse)     # tidy/wrangle data
library(tidytext)      # nlp analysis
library(schrute)       # office data
library(xkcd)          # font
library(ggwordcloud)   # word cloud
library(igraph)        # to build the network graphs
library(ggraph)      
library(tidyr)         # wrangle data
library(patchwork)     # Plot multiple grahics


# get the data
the_office <- schrute::theoffice

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


#~~~~ pre-prep clean up
#regex helper - removes punct, digis
remove_regex <- "[:punct:]|[:digit:]|parts |part |the |and"

ratings <- office_ratings %>%
        transmute(
                episode_name = str_to_lower(title),
                episode_name = str_remove_all(episode_name, remove_regex),
                episode_name = str_trim(episode_name, side = "both"), #remove wht space at edge
                episode_name = str_squish(episode_name),              # remove wht space between
                imdb_rating
        )

clean_office <- the_office %>%
        select(season, episode, episode_name, writer, text) %>%
        mutate(
                season = as.numeric(season),
                episode = as.numeric(episode),
                text = str_to_lower(text),
                text = str_remove_all(text, remove_regex),
                text = str_trim(text, side = "both"), #remove wht space at edge
                text = str_squish(text),              # remove wht space between
        ) 

# tokenize the document. Use tidytext to a two word terms for each writer
# bigram for the three writers

token_bigrams <- clean_office %>%
                      filter(writer %in% c("Mindy Kaling", "B.J. Novak", "Brent Forrester")) %>%
                      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                      separate(bigram, c("word1", "word2"), sep = " ") %>%
                      filter(!word1 %in% stop_words$word,
                             !word2 %in% stop_words$word,
                             !is.na(word1)) %>%
                      #count(word1, word2, sort = TRUE)  
                      unite(bigram, word1, word2, sep = " ") %>%
                      select(writer, bigram)

## term frequency for the top 15 bigram strings per writer

bigram_tf_idf <- 
token_bigrams %>% #ungroup() %>% 
        count(writer, bigram) %>%
        bind_tf_idf(bigram, writer, n) %>%
        arrange(desc(tf_idf)) %>%
        group_by(writer) %>% 
        group_modify( ~ head(.x,15L))

gg_bigrm <-
        ggplot(data = bigram_tf_idf, aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
        geom_bar(stat = "identity", aes(fill = writer), col = "black") +
        coord_flip() +
        facet_wrap(~ writer, scales = "free_y") +
        theme(text=element_text(family = "xkcd", colour = "goldenrod")) +
        theme_abiyu() +
        theme(legend.position = "none") +
        labs(y = "term frequency/inverse document frequency", x = "two word terms")



###igraph
set.seed(2017)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

gg_bigram_network <- 
        clean_office %>%
        filter(writer %in% c("Mindy Kaling", "B.J. Novak", "Brent Forrester")) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word, # filter out stop words
               !word2 %in% stop_words$word,
               !str_detect(word1, "\\d"),   #filter out digits
               !str_detect(word2, "\\d"),
               !is.na(word1)) %>% 
        count(word1, word2, sort = TRUE) %>%
        filter(n > 10) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "gray48", alpha = 0.5, size = 4) +
        geom_node_text(aes(label = name), col = "red", vjust = 1, hjust = 1)


# id the unique word that was used by one writer and not the others
# Unique word per writer not shared by the other 2/finger print
unique_word <- 
        clean_office %>%
        filter(writer %in% c("Mindy Kaling", "B.J. Novak", "Brent Forrester")) %>%
        unnest_tokens(unigram, text, token = "ngrams", n = 1) %>%
        filter(!unigram %in% stop_words$word,
               !str_detect(unigram, "\\d")) %>%
        group_by(writer) %>% 
        count(unigram, sort = TRUE) %>% 
        ungroup() %>%
        group_by(unigram) %>% 
        pivot_wider(names_from = writer, values_from = n, values_fill = list(n = 0)) %>% 
        select(unigram, mk = 2, bjv =3, bf = 4) %>% 
        mutate(who =  case_when ( 
                mk >= 1 & bjv == 0 & bf == 0  ~ "mk_100",
                mk == 0 & bjv >= 1 & bf == 0  ~ "bjv_010",
                mk == 0 & bjv == 0 & bf >= 1  ~ "bf_001")) %>% 
        filter(who %in% c("mk_100", "bjv_010", "bf_001")) %>% 
        select(-who, Mindy_Kaling = mk, B_J_Novak = bjv, Brent_Forrester = bf) %>% 
        pivot_longer(-unigram) %>%
        arrange(desc(value)) %>%
        group_by(name) %>%
        group_modify( ~ head(.x,20L)) 

gg_unigrm_cloud <- 
        ggplot(data = unique_word,aes(label = unigram, size = value, col = name)) +
        geom_text_wordcloud_area() +
        scale_size_area(max_size = 20) +
        facet_wrap(~name) +
        theme_minimal() +
        theme(strip.text.x = element_text(size = 12, colour = "gray46", face = "bold"))


patchwork <- (gg_bigrm | gg_bigram_network) / gg_unigrm_cloud 

patchwork + plot_annotation(
        title = "NLP analysis for highest rated episodes writers of \"The office\" TV show",
        subtitle = "The bar chart show two word term frequency by writer. The second graph shows the two words relations, \nand the word cloud shows the unique words used by one writer and not the others in the highest rated episodes. ",
        caption = "Data Source: The schrute R package for The Office transcripts and data.world for IMDB ratings. | DataViz @abiyugiday",
        theme = theme(
                plot.title    = element_text(size = 22, colour = "yellow4", family = "Batang"),
                plot.subtitle  = element_text(size = 14, colour = "gray48"),
                plot.caption  = element_text(size =  12, colour = "gray48")
        ))
