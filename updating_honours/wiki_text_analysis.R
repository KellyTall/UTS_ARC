library(tidytext) 
library(tidyverse)
library(scales)
library(janitor)

wp_format <-  theme(
  legend.background = element_rect(fill = "white", size = 4, colour = "white"),
  axis.ticks = element_line(colour = "grey70", size = 0.2),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_blank()
)


##need data objects generated from import_clean to run analysis

##trans females have been classified here as females

View(wikipedia)
# wikipedia <- read.csv("wikipedia.csv")

text_data_prep_prepost <- wikipedia %>% 
  mutate(gender = case_when(gender == "TF" ~"Female",
                            gender == "F" ~"Female",
                            gender == "M" ~"Male",
                            TRUE ~ as.character(gender))) %>% 
  filter(gender=="Female") %>% 
  select(honours_date, citation, new_honours_date, wp_creation_date) %>% 
  mutate(before_after = case_when (wp_creation_date > new_honours_date ~ "After Honours",
                                   TRUE ~ "Before Honours")) %>% 
  filter(honours_date>="2001-01-15") %>% 
  group_by(before_after) 

data(stop_words)

filter_words <- c("aust", "service","qb", "day", "recognition", "oam", "am", "gaz", "ao", "s13", "division", "list")

text_data_prepost <-  text_data_prep_prepost %>% 
  unnest_tokens(word,citation) %>% 
  anti_join(stop_words) %>%
  filter(!word %in% filter_words) %>%
  count(word, sort=TRUE) %>% 
  pivot_wider(names_from = before_after, values_from = n) %>% 
  clean_names()


  # View(text_data_prepost_tf)


  
# citation_tf_idf <- text_data_prepost %>%
#   bind_tf_idf(word, before_after, n) %>% 
#   pivot_wider(names_from = before_after, values_from = tf_idf)

View(text_data_prepost)
write.csv(text_data_prepost, "text_data_prepost.csv")

scatter_pre_post <- ggplot(text_data_prepost, aes(before_honours, after_honours)) +
  geom_abline(color = "cornflowerblue", alpha=.9)+
  geom_jitter(alpha = 0.4, size = 2.5, width = 0.25, height = 0.25, shape= 21, 
              fill="mediumpurple", colour="white", stroke=2) +
  geom_text(aes(label = word), check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(color = "cornflowerblue", alpha=.9)+
  wp_format+
  theme_minimal()+
  labs(
    # title = "Citations describing achievements of female recipients of Order of Australia who have Wikipedia Page", 
    #    subtitle = "Showing difference in frequency of words used in Order citation between pages created before or after Order received.",
       x=NULL,
       y=NULL,
       caption = "Citations of n=1,002 women who received Order of Australia after 15 January 2001. Note: Log10 scale")+
  annotate("text", x=.8, y=100, label="Words more associated with people who \nhave a page created after receiving Order", hjust=0, size=4, colour="mediumpurple4")+
  annotate("text", x=200, y=1, label="Words more associated with people who \nhave a page created before receiving Order", hjust=1, size=4, colour="mediumpurple4")

 scatter_pre_post
 
 ggsave("scatter_pre_post.png", width= 25, height = 15, units=c("cm") )


##with page / without page

text_data_prep_wiki <- recipient %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  filter(gender=="F") %>% 
  select(citation, wikipedia_page) %>% 
  group_by(wikipedia_page) 

data(stop_words)

filter_words <- c("aust", "service","qb", "day", "recognition", "oam", "am", "gaz", "ao", "s13", "division", "list", "cw", "birthday", "s242")

text_data_wiki <-  text_data_prep_wiki %>% 
  unnest_tokens(word,citation) %>% 
  anti_join(stop_words) %>%
  filter(!word %in% filter_words) %>%
  count(word, sort=TRUE) %>% 
  pivot_wider(names_from = wikipedia_page, values_from = n) %>% 
  rename(`No Wikipedia Biography` = No,
         `Has Wikipedia Biography` = Yes)

# View(text_data_prepost_tf)

View(text_data_wiki)

# citation_tf_idf <- text_data_prepost %>%
#   bind_tf_idf(word, before_after, n) %>% 
#   pivot_wider(names_from = before_after, values_from = tf_idf)

write.csv(text_data_wiki, "text_data_wiki.csv")

scatter_wp <- ggplot(text_data_wiki, aes(`Has Wikipedia Biography`,`No Wikipedia Biography`)) +
  geom_abline(color = "cornflowerblue", alpha=.9)+
  geom_jitter(alpha = 0.4, size = 2.5, width = 0.25, height = 0.25, shape= 21, 
              fill="mediumpurple", colour="white", stroke=2) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10() +
  scale_y_log10() +
  
  wp_format+
  theme_minimal()+
  labs(
    # title = "Citations describing achievements of female recipients of Order of Australia", 
    #    subtitle = "Showing difference in frequency of words for those with and without a Wikipedia Biography.",
       x=NULL,
       y=NULL,
       caption = "Citations of n=13,002 Orders for women who received Order of Australia. Note: Log10 scale")+
  annotate("text", x=.8, y=380, label="Words more associated with people who \nhave have an Order of Australia but no Wikipedia Biography", hjust=0, size=4, colour="mediumpurple4")+
  annotate("text",  x=450, y=2, label="Words more associated with people who \nhave an Order of Australia and a Wikipedia Biography", hjust=1, size=4, colour="mediumpurple4")


scatter_wp

ggsave("scatter_wp.png", width= 25, height = 15, units=c("cm") )

    