
library(tidyverse)
library(scales)

gender_colour <- c("mediumpurple", "darkturquoise")

wp_format <-  theme(
  legend.background = element_rect(fill = "white", size = 4, colour = "white"),
  axis.ticks = element_line(colour = "grey70", size = 0.2),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_blank()
)

##need data objects from import_clean 

##charts

##those that have wikipedia page / wiki + honours

##men and women over time (by year)

View(all_data)
View(wikipedia)

gender_split <- all_data %>% 
  filter(gender!="U") %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  select(honours_year, gender) %>% 
  group_by(honours_year) %>% 
  add_tally(name="total_year") %>% 
  group_by(honours_year, gender, total_year) %>% 
  tally(name = "gender_total") %>% 
  mutate(gender_prop_year = gender_total/total_year)

View(gender_split)

gender_check <- all_data %>%
  filter( gender=="TF")

year_check <- all_data %>% 
  group_by(honours_year) %>% 
  tally()

View(year_check)

##proportion over time

award_by_year <- ggplot(gender_split, aes(honours_year, gender_prop_year, fill=gender))+
  geom_col()+
  # scale_fill_manual(values = gender_colour)+
  wp_format+
  theme_minimal()+
  labs(title = "Order of Australia Honours over time: Male and Female Recipients",
       x= "Year Honours awarded",
       y="")+
  scale_y_continuous(labels = percent)+
  geom_hline(yintercept = .75, alpha=.25) +
  geom_hline(yintercept = .5, alpha=.25) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = gender_colour)

##add in 50% and 75% reference line
##remove background
##make y scale percentage
##add heading and subheading and bottom annotation of data and number etc
  
award_by_year



##proportion of wikipedia pages created for women / 

gender_split_wiki <- wikipedia %>% 
  filter(gender!="U") %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  select(wikipedia_creation_year, gender) %>% 
  group_by(wikipedia_creation_year) %>% 
  add_tally(name="total_year") %>% 
  group_by(wikipedia_creation_year, gender, total_year) %>% 
  tally(name = "gender_total") %>% 
  mutate(gender_prop_year = gender_total/total_year)
  
WP_by_year <- ggplot(gender_split_wiki, aes(wikipedia_creation_year, gender_prop_year, fill=gender))+
  geom_col()+
  wp_format+
  theme_minimal()+
  labs(title = "Proportion of Wikipedia Pages created for Order of Australia Honours 
       recipients over time: Male and Female",
       x= "Year Honours awarded",
       y="")+
  scale_y_continuous(labels = percent)+
  geom_hline(yintercept = .75, alpha=.25) +
  geom_hline(yintercept = .5, alpha=.25) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = gender_colour)

WP_by_year

##add in 50% and 75% reference line
##remove background
##make y scale percentage
##add heading and subheading and bottom annotation of data and number etc

WP_by_year_num <- ggplot(gender_split_wiki, aes(wikipedia_creation_year, gender_total, fill=gender))+
  geom_col()+
  wp_format+
  theme_minimal()+
  labs(title = "Number of Wikipedia Pages created for Order of Australia Honours 
       recipients over time: Male and Female",
       x= "Year Honours awarded",
       y="")+
  # geom_hline(yintercept = .75, alpha=.25) +
  # geom_hline(yintercept = .5, alpha=.25) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = gender_colour)

WP_by_year_num

##remove background / fix up
##add heading and subheading and bottom annotation of data and number etc


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
