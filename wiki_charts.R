
library(tidyverse)

##need data objects from import_clean 

##charts

##those that have wikipedia page / wiki + honours

##men and women over time (by year)

View(all_data)


gender_split <- all_data %>% 
  filter(gender!="U") %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender)))

gender_check <- gender_split %>% 
  filter( gender=="TF")

View(gender_check)

award_by_year <- ggplot(gender_split, aes(honours_year, fill=gender))+
  geom_bar()


##proportion of order holders who have an honour

View(recipient)
prop_page <- recipient %>%
  filter(gender!="U") %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  select(award_comb,wikipedia_page ) %>% 
  group_by_all() %>% 
  tally() %>% 
  mutate(total = sum(n)) %>% 
  mutate(award_prop = n/total) %>% 
  filter(wikipedia_page=="Yes")


prop_page_chart <- ggplot(prop_page, aes(award_comb, award_prop))+
  geom_col()

prop_page_chart


##proportion of level by gender

prop_page_gender <- recipient %>%
  filter(gender!="U") %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  select(award_comb,wikipedia_page,gender) %>% 
  group_by(award_comb,gender) %>% 
  add_tally(name="award") %>% 
  group_by(award_comb,gender, wikipedia_page, award) %>% 
  tally(name="gender_award") %>% 
  mutate(prop_gender_award = gender_award/award) %>% 
  filter(wikipedia_page=="Yes") %>% 
  filter(award_comb!="ADK")

prop_page_chart <- ggplot(prop_page_gender, aes(gender, prop_gender_award))+
  geom_col()+
  facet_wrap(~award_comb,nrow=1)


prop_page_chart  

##announcements by week
View(wikipedia)

week <- wikipedia %>% 
  select(week_diff) %>% 
  group_by(week_diff) %>% 
  tally(name="week_num")

week_chart <- ggplot(week, aes(week_diff, week_num)) +
  geom_col()+
  geom_point(alpha=.2)
week_chart

##announcements by week and by order level


week_award <- wikipedia %>% 
  select(week_diff, award_comb) %>% 
  group_by(week_diff, award_comb) %>% 
  tally(name="week_num") %>% 
  filter(week_diff >-52 & week_diff <53)

week_award_chart <- ggplot(week_award, aes(week_diff, award_comb,fill=week_num)) +
  geom_tile()

week_award_chart