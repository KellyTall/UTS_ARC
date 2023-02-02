library(tidytext) 
library(tidyverse)
library(scales)

wp_format <-  theme(
  legend.background = element_rect(fill = "white", size = 4, colour = "white"),
  axis.ticks = element_line(colour = "grey70", size = 0.2),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_blank()
)


##need data objects generated from import_clean to run analysis

##trans females have been classified here as females

View(wikipedia)

text_data_prep_prepost <- wikipedia %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  filter(gender=="F") %>% 
  select(citation, new_honours_date, wp_creation_date) %>% 
  mutate(before_after = case_when (wp_creation_date > new_honours_date ~ "After Honours",
                                   TRUE ~ "Before Honours")) %>% 
  group_by(before_after) 

data(stop_words)

filter_words <- c("aust", "service","qb", "day", "recognition", "oam", "am", "gaz", "ao", "s13", "division", "list")

text_data_prepost <-  text_data_prep_prepost %>% 
  unnest_tokens(word,citation) %>% 
  anti_join(stop_words) %>%
  filter(!word %in% filter_words) %>%
  count(word, sort=TRUE) %>% 
  pivot_wider(names_from = before_after, values_from = n)

  # View(text_data_prepost_tf)


  
# citation_tf_idf <- text_data_prepost %>%
#   bind_tf_idf(word, before_after, n) %>% 
#   pivot_wider(names_from = before_after, values_from = tf_idf)

scatter_pre_post <- ggplot(text_data_prepost, aes(`Before Honours`, `After Honours`)) +
  geom_jitter(alpha = 0.4, size = 2.5, width = 0.25, height = 0.25, shape= 21, 
              fill="mediumpurple", colour="white", stroke=2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "cornflowerblue", alpha=.9)+
  wp_format+
  theme_minimal()+
  labs(title = "Wikipedia Biographies of Order of Australia Recipients", 
       subtitle = "Pages created before or after award received. Chart shows frequency of words 
       used in citation")

 scatter_pre_post


##with page / without page

text_data_prep_wiki <- recipient %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  filter(gender=="F") %>% 
  select(citation, wikipedia_page) %>% 
  group_by(wikipedia_page) 

data(stop_words)

filter_words <- c("aust", "service","qb", "day", "recognition", "oam", "am", "gaz", "ao", "s13", "division", "list")

text_data_wiki <-  text_data_prep_wiki %>% 
  unnest_tokens(word,citation) %>% 
  anti_join(stop_words) %>%
  filter(!word %in% filter_words) %>%
  count(word, sort=TRUE) %>% 
  pivot_wider(names_from = wikipedia_page, values_from = n) %>% 
  rename(`No Wikipedia Biography` = No,
         `Has Wikipedia Biography` = Yes)

# View(text_data_prepost_tf)



# citation_tf_idf <- text_data_prepost %>%
#   bind_tf_idf(word, before_after, n) %>% 
#   pivot_wider(names_from = before_after, values_from = tf_idf)

scatter_pre_post <- ggplot(text_data_wiki, aes(`Has Wikipedia Biography`,`No Wikipedia Biography`)) +
  geom_jitter(alpha = 0.4, size = 2.5, width = 0.25, height = 0.25, shape= 21, 
              fill="mediumpurple", colour="white", stroke=2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "cornflowerblue", alpha=.9)+
  wp_format+
  theme_minimal()+
  labs(title = "Order of Australia reciptients and Wikipedia Biographies", 
       subtitle = "Those with and without a Wikipedia Biography. Chart shows frequency of words 
       used in citation")

scatter_pre_post

    