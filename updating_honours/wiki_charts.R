
library(tidyverse)
library(scales)
library(ggthemes)

gender_colour <- c("mediumpurple", "darkturquoise")
award_colour <- c( "gold2", "darkolivegreen4")

wp_format <-  theme(
  legend.background = element_rect(fill = "white", linewidth = 4, colour = "white"),
  axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_blank()
)

##need data objects from import_clean 

##charts

##those that have wikipedia page / wiki + honours

##men and women over time (by year)

View(all_data)

date_check <- all_data %>% 
  select(honours_date) %>% 
  summarise(min=min(honours_date), max=max(honours_date))

View(year_check)

year_check <- all_data %>% 
  select(honours_year, gender) %>% 
  group_by_all() %>% 
  tally()

View(wikipedia)


all_data <- read_csv("all_data.csv")

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

##prop by order

View(prop_by_type)


View(recipient)
recipient <- read_csv("recipient.csv")

prop_by_type <- recipient %>% 
  select(award_comb, wikipedia_page) %>% 
  mutate(award_comb = as_factor(award_comb),
         award_comb = fct_relevel(award_comb, c("ADK", "AC", "AO", "AM", "OAM"))
         ) %>% 
  group_by(award_comb) %>% 
  add_tally(name="award_total") %>% 
  group_by(award_comb, wikipedia_page) %>% 
  add_tally(name="wp_total") %>% 
  group_by_all() %>% 
  summarise() %>% 
  mutate(prop=wp_total/award_total) %>% 
  mutate(award = as_factor(case_when(award_comb == "ADK" ~ "Dame or Knight",
                           award_comb == "AC" ~ "Companion",
                           award_comb == "AO" ~ "Officer",
                           award_comb == "AM" ~ "Member",
                           award_comb == "OAM" ~ "Medal"))) %>% 
  mutate(award = fct_relevel(award, c("Dame or Knight", "Companion", "Officer", "Member", "Medal"))) %>% 
  ungroup() 
  
year_check <- recipient %>% 
  select(first_honours_date) %>% 
  summarise(max(first_honours_date),
            min(first_honours_date))

  
prop_by_type_chart <- ggplot(prop_by_type, aes(award, prop, fill=wikipedia_page))+
  geom_col()+
  scale_y_continuous(labels = percent)+
  geom_text(aes(label = percent(round(prop,3))),
            position = position_fill(vjust = 0.5)) +
  scale_x_discrete(labels = prop_by_type %>% 
                     group_by(award) %>% 
                     summarize(n = sum(wp_total)) %>%
                     mutate(lab = paste0(award, " (", n, ")")) %>%
                     pull(lab))+
  labs(title = "Proportion of Order of Australia Recipients with Wikipedia page",
       caption = "n=43,705 Order of Australia Recipients (Feb 1975 - June 2022) Showing highest Order received by an individual",
       x=NULL,
       y=NULL)+
  scale_fill_manual(values =award_colour)+
  theme_minimal()+
    guides(fill = guide_legend(title = "Wikipedia Page"))
  
  
                     


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

wikipedia <- read_csv("wikipedia.csv")

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
  filter(week_diff >-52 & week_diff <53) %>% 
  filter(award_comb!="ADK") %>% 
  mutate(award = as_factor(case_when(award_comb == "AC" ~ "Companion",
                                     award_comb == "AO" ~ "Officer",
                                     award_comb == "AM" ~ "Member",
                                     award_comb == "OAM" ~ "Medal"))) %>% 
  mutate(award = fct_relevel(award, c("Companion", "Officer", "Member", "Medal"))) %>% 
  ungroup() 

week_0 <- week_award %>% 
  filter(week_diff==0) %>% 
  ungroup() %>% 
  summarise(sum(week_num))

week_average <- week_award %>% 
  ungroup() %>% 
  summarise(mean(week_num))


week_award_chart <- ggplot(week_award, aes(week_diff, award,fill=week_num)) +
  geom_tile()+
  theme_minimal()+
  scale_fill_continuous(high="darkolivegreen", low="cornsilk",
                        breaks=c(2,4,6,8,10,12,14,16,18,20))+
  scale_y_discrete(labels = week_award %>% 
                     group_by(award) %>% 
                     summarize(n = sum(week_num)) %>%
                     mutate(lab = paste0(award,"\n(",n, ")")) %>%
                     pull(lab))+
  labs(title = "Wikipedia Pages created for Order recipients 52 weeks pre- and post-announcement",
       subtitle = "49 pages were created on week of Order announcement compared to an average of 1.89 in other weeks ",
       caption = "\n\nn=388 Wikipedia pages were created for Order of Australia recipients in the 52 weeks prior and post announcement",
       x="Number of weeks pre- and post-Order announcement",
       y=NULL)+
  theme_minimal()+
  guides(fill = guide_legend(title = "No. Wikipedia\npages created\nin week"))
  



