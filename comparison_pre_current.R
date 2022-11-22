library(dplyr)
library(janitor)
library()



prev_import <- read_csv("producing_distinction.csv") %>% 
  clean_names()


View(prev_import)

number_wiki_prev <- prev_import %>% 
  select(wikipedia_page, gender) %>% 
  filter(wikipedia_page =="Yes" & gender !="U") %>% 
  add_tally(name="all_pages") %>% 
  group_by(gender, all_pages) %>% 
  tally(name="gender_pages") %>% 
  mutate(stage = "one") %>% 
  mutate(prop = gender_pages/all_pages)


number_wiki_current <- wikipedia %>% 
  select(wikipedia_page, gender) %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  filter(wikipedia_page =="Yes" & gender !="U") %>% 
  add_tally(name="all_pages") %>% 
  group_by(gender, all_pages) %>% 
  tally(name="gender_pages") %>% 
  mutate(stage = "two") %>% 
  mutate(prop = gender_pages/all_pages)



number_wiki_combine <- rbind(number_wiki_current, number_wiki_prev)

all_pages_chart <- number_wiki_combine %>% 
  ungroup() %>% 
  select(all_pages, stage) %>% 
  group_by(stage, all_pages) %>% 
  summarise()

ggplot(all_pages_chart, aes(stage, all_pages))+
  geom_point()+
  geom_path()


