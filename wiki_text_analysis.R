library(tidytext) 
library(tidyverse)
library(scales)


##need data objects generated from import_clean to run analysis

##trans females have been classified here as females

View(wikipedia)

text_data_prep_prepost <- wikipedia %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  filter(gender=="F") %>% 
  select(citation, new_honours_date, wp_creation_date) %>% 
  mutate(before_after = case_when (wp_creation_date > new_honours_date ~ "after",
                                   TRUE ~ "before")) %>% 
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

scatter_pre_post <- ggplot(text_data_prepost, aes(before, after)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")
  
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
  rename(no_wiki = No,
         yes_wiki = Yes)

# View(text_data_prepost_tf)



# citation_tf_idf <- text_data_prepost %>%
#   bind_tf_idf(word, before_after, n) %>% 
#   pivot_wider(names_from = before_after, values_from = tf_idf)

scatter_pre_post <- ggplot(text_data_wiki, aes(yes_wiki, no_wiki)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

scatter_pre_post

    