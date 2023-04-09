
library(tidyverse)
library(scales)
library(ggthemes)
library(purrr)
library(ggforce)

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

View(date_check)

date_check <- all_data %>% 
  
  select(honours_date) %>% 
  summarise(min=min(honours_date), max=max(honours_date))

View(year_check)

year_check <- all_data %>% 
  select(honours_year, gender) %>% 
  group_by_all() %>% 
  tally()

View(wikipedia)


# all_data <- read_csv("all_data.csv")

gender_split <- all_data %>% 
  filter(gender!="U") %>% 
  mutate(gender = case_when(gender == "TF" ~"F",
                            TRUE ~ as.character(gender))) %>% 
  select(honours_year, gender) %>% 
  group_by(honours_year) %>% 
  add_tally(name="total_year") %>% 
  group_by(honours_year, gender, total_year) %>% 
  tally(name = "gender_total") %>% 
  mutate(gender_prop_year = gender_total/total_year) %>% 
  print(n=98)

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
  labs(
    # title = "Proportion of Order of Australia Recipients with Wikipedia page",
       caption = "n=45,166 Order of Australia Recipients (Feb 1975 - January 2023) Showing highest Order received by an individual",
       x=NULL,
       y=NULL)+
  scale_fill_manual(values =award_colour)+
  theme_minimal()+
    guides(fill = guide_legend(title = "Wikipedia Page"))+
  theme(legend.position = "bottom",
        # legend.position = c(0.05, .9),
        legend.justification = c("left", "top"),
        legend.direction="horizontal",
        legend.box.just = "right",
        legend.margin = margin(2, 6, 2,0),
        # legend.title=element_blank(),
        legend.spacing.x = unit(.2, 'cm'))
prop_by_type_chart
  
ggsave("prop_by_type_chart.png", width= 20, height = 10, units=c("cm") )
                     

prop_by_type_gender <- recipient %>% 
  ungroup() %>% 
  select(award_comb, wikipedia_page,gender) %>% 
  mutate(gender = case_when(gender == "TF" ~"Female",
                            gender == "F" ~"Female",
                            gender == "M" ~"Male",
                            TRUE ~ as.character(gender))) %>% 
  mutate(award_comb = as_factor(award_comb),
         award_comb = fct_relevel(award_comb, c("ADK", "AC", "AO", "AM", "OAM"))
  ) %>% 
  group_by(gender, award_comb) %>% 
  add_tally(name="award_total_gender") %>% 
  group_by(gender, award_comb, wikipedia_page) %>% 
  add_tally(name="wp_total_gender") %>% 
  group_by_all() %>% 
  summarise() %>% 
  mutate(prop_gender=wp_total_gender/award_total_gender) %>% 
  mutate(award = as_factor(case_when(award_comb == "ADK" ~ "Dame or Knight",
                                     award_comb == "AC" ~ "Companion",
                                     award_comb == "AO" ~ "Officer",
                                     award_comb == "AM" ~ "Member",
                                     award_comb == "OAM" ~ "Medal"))) %>% 
  mutate(award = fct_relevel(award, c("Dame or Knight", "Companion", "Officer", "Member", "Medal"))) %>% 
  ungroup() %>% 
  group_by(gender) %>% 
  mutate(gender_label=paste0(gender," (",sum(wp_total_gender),")")) %>% 
  mutate(award_label=as_factor(paste0(award,"\n(",award_total_gender,")")))



# gender_award_label <- prop_by_type_gender %>% 
#   select(award, gender, award_label ) %>% 
#   arrange(gender) %>% 
#   group_by(gender, award, award_label) %>% 
#   tally()


prop_by_type_chart_gender <- ggplot(prop_by_type_gender, aes(award_label, prop_gender, fill=wikipedia_page))+
  geom_col()+
  facet_wrap(~gender_label, scales="free_x", ncol = 1)+
  scale_y_continuous(labels = percent)+
  geom_text(aes(label = percent(round(prop_gender,3))),
            position = position_fill(vjust = 0.5)) +
  labs(
    # title = "Proportion of Order of Australia Recipients with Wikipedia page:\nFemale and Male",
       caption = "n=45,166 Order of Australia Recipients (Feb 1975 - January 2023) Showing highest Order received by an individual",
       x=NULL,
       y=NULL)+
  scale_fill_manual(values =award_colour)+
  theme_minimal()+
  guides(fill = guide_legend(title = "Wikipedia Page"))+
  theme(legend.position = "bottom",
        # legend.position = c(0.05, .9),
        legend.justification = c("left", "top"),
        legend.direction="horizontal",
        legend.box.just = "right",
        legend.margin = margin(2, 6, 2,0),
        # legend.title=element_blank(),
        legend.spacing.x = unit(.2, 'cm'),
        strip.text.x = element_text(size = 12, hjust = .5)) 

prop_by_type_chart_gender

ggsave("prop_by_type_chart_gender.png", width= 20, height = 15, units=c("cm") )



##proportion over time

award_by_year <- ggplot(gender_split, aes(honours_year, gender_prop_year, fill=gender))+
  geom_col()+
  # scale_fill_manual(values = gender_colour)+
  wp_format+
  theme_minimal()+
  labs(
    # title = "Order of Australia Honours over time: Proportion of Male and Female Recipients",
       caption = "n=45,606 Order of Australia Recipients (Feb 1975 - January 2023) Showing highest Order received by an individual.\nNote: 2023 only includes January honours",
       x= NULL,
       y=NULL)+
  scale_y_continuous(labels = percent)+
  geom_hline(yintercept = .75, alpha=.25) +
  geom_hline(yintercept = .5, alpha=.25) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = gender_colour)+
  theme(legend.position = "bottom",
        # legend.position = c(0.05, .9),
        legend.justification = c("left", "bottom"),
        legend.direction="horizontal",
        legend.box.just = "right",
        legend.margin = margin(2, 6, 2,0),
        legend.title=element_blank(),
        legend.spacing.x = unit(.2, 'cm'))


award_by_year

ggsave("award_by_year.png", width= 20, height = 10, units=c("cm") )




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
  mutate(gender_prop_year = gender_total/total_year) %>% 
  print(n=46)
  
WP_by_year <- ggplot(gender_split_wiki, aes(wikipedia_creation_year, gender_prop_year, fill=gender))+
  geom_col()+
  wp_format+
  theme_minimal()+
  labs(title = "Wikipedia Pages created for Order of Australia Honours recipients over time:\nProportion of Male and Female",
       caption = "n=4,882 Wikipedia Biographies created for Order of Australia recipients",
       x= NULL,
       y=NULL)+
  scale_y_continuous(labels = percent)+
  geom_hline(yintercept = .75, alpha=.25) +
  geom_hline(yintercept = .5, alpha=.25) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = gender_colour)+
  theme(legend.position = "top",
        # legend.position = c(0.05, .9),
        legend.justification = c("left", "top"),
        legend.direction="horizontal",
        legend.box.just = "right",
        legend.margin = margin(2, 6, 2,0),
        legend.title=element_blank(),
        legend.spacing.x = unit(.2, 'cm'))

WP_by_year
ggsave("wp_year.png", width= 20, height = 10, units=c("cm") )

##add in 50% and 75% reference line
##remove background
##make y scale percentage
##add heading and subheading and bottom annotation of data and number etc

WP_by_year_num <- ggplot(gender_split_wiki, aes(wikipedia_creation_year, gender_total, fill=gender))+
  geom_col()+
  wp_format+
  theme_minimal()+
  labs(
    # title = "Number of Wikipedia Pages created for Order of Australia Honours recipients over time: \nMale and Female",
       caption = "n=4,882 Wikipedia Biographies created for Order of Australia recipients",
       x=NULL,
       y=NULL)+
  # geom_hline(yintercept = .75, alpha=.25) +
  # geom_hline(yintercept = .5, alpha=.25) +
  scale_fill_manual(name = "Gender", labels = c("Female", "Male"), values = gender_colour)+
  theme(
        # legend.position = c(0.02, .99),
        legend.position = "bottom",
        legend.justification = c("left", "top"),
        legend.direction="horizontal",
        legend.box.just = "right",
        legend.margin = margin(2, 6, 2,0),
        legend.title=element_blank(),
        legend.spacing.x = unit(.2, 'cm'))

WP_by_year_num
ggsave("wp_by_year_num.png", width= 20, height = 10, units=c("cm") )

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
  filter(wikipedia_page=="Yes") %>% 
  


# prop_page_chart <- ggplot(prop_page, aes(award_comb, award_prop))+
#   geom_col()

# prop_page_chart


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


explore <- wikipedia %>% 
  filter(week_diff==558)

View(explore)

week <- wikipedia %>% 
  select(week_diff) %>% 
  group_by(week_diff) %>% 
  tally(name="week_num")

creation_clusters <- wikipedia %>% 
  filter(week_diff==) %>% 
  add_tally()

View(creation_clusters)

week_chart <- ggplot(week, aes(week_diff, week_num)) +
  geom_vline(xintercept = 0, alpha=.2, linewidth=.2) +
  geom_linerange(aes(x=week_diff, ymax=week_num, ymin=0),colour="black", linewidth=.1)+
  geom_point(shape=21, alpha=.5, colour="grey", size=2, fill="gold")+
  geom_curve(aes(x = -140, y = 46, xend = -15, yend = 53),
             arrow = arrow(length = unit(0.01, "npc")), linewidth=.01, curvature = -0.2)+
  
  annotate("text", x = -60, y = 43, label = "53 Wikipedia biographies were created \nduring the week of honours announcements", hjust=1, size=4)+

    geom_curve(aes(x = 660, y = 30, xend = 580, yend = 34),
             arrow = arrow(length = unit(0.01, "npc")), linewidth=.01, curvature = 0.2)+
  
  annotate("text", x = 600, y = 27, label = "Approx 34 biographies were created for\nAustralian Paralympians\non 28-30 September 2011", hjust=0, size=4)+
  
  geom_curve(aes(x = -160, y = 15, xend = -95, yend = 18),
             arrow = arrow(length = unit(0.01, "npc")), linewidth=.01, curvature = -0.2)+
  
  annotate("text", x = -102, y = 12, label = "Approx 16 biographies were created for\nAustralian Paralympians on 7-11 July 2012", hjust=1, size=4)+
  
  wp_format+
  theme_minimal()+
  labs(
    # title = "Number of Wikipedia Pages created for Order of Australia Honours over week periods:\nWeek 0 is week of order announcements",
       caption = "n=4,882 Wikipedia Biographies created for Order of Australia recipients",
       x=NULL,
       y=NULL)+
  geom_segment(aes(x = -5, y = 55, xend = -500, yend = 55), arrow = arrow(length = unit(0.01, "npc")), linewidth=.01)+
  geom_segment(aes(x = 5, y = 55, xend = 500, yend = 55), arrow = arrow(length = unit(0.01, "npc")), linewidth=.01)+
  annotate("text", x = -10, y = 57, label = "Honours awarded before Wikipedia biography created ", hjust=1, size=4)+
  annotate("text", x = 20, y = 57, label = "Honours awarded after Wikipedia biography created ", hjust=0, size=4)

week_chart

ggsave("week_chart.png", width= 25, height = 15, units=c("cm") )


##stick chart showing male and female pages

week_award_all <- wikipedia %>% 
  select(week_diff, gender) %>% 
  mutate(gender = case_when(gender == "TF" ~"Female",
                            gender == "F" ~"Female",
                            gender == "M" ~"Male",
                            TRUE ~ as.character(gender))) %>% 
  group_by(week_diff, gender) %>% 
  tally(name="week_num") %>% 
  ungroup() %>% 
  mutate(total_gender=sum(week_num),
         gender_prop = week_num/total_gender) 


week_range_gender <- ggplot(week_award_all, aes(week_diff, week_num))+
  geom_linerange(aes(x=week_diff, ymax=week_num, ymin=0),colour="black", linewidth=.1)+
  geom_point(shape=21, alpha=.5, colour="grey", size=2, fill="gold")+
  facet_wrap(~gender)+
  wp_format+
  theme_minimal()+
  labs(
    # title = "Number of Wikipedia Pages created for Order of Australia Honours over week periods by gender:\nWeek 0 is week of order announcements",
       caption = "n=4,882 Wikipedia Biographies created for Order of Australia recipients",
       x=NULL,
       y=NULL)
  
week_range_gender

ggsave("week_range_gender.png", width= 25, height = 15, units=c("cm") )



##prop
ggplot(week_award_all, aes(week_diff, gender_prop))+
  geom_linerange(aes(x=week_diff, ymin=0, ymax=gender_prop), alpha=.2)+
  geom_point(alpha=.2)+
  facet_wrap(~gender)



##announcements by week and by order level




week_award <- wikipedia %>% 
  select(week_diff, award_comb) %>% 
  group_by(week_diff, award_comb) %>% 
  tally(name="week_num") %>% 
  filter(week_diff >-53 & week_diff <53) %>% 
  filter(award_comb!="ADK") %>% 
  mutate(award = as_factor(case_when(award_comb == "AC" ~ "Companion",
                                     award_comb == "AO" ~ "Officer",
                                     award_comb == "AM" ~ "Member",
                                     award_comb == "OAM" ~ "Medal"))) %>% 
  mutate(award = fct_relevel(award, c("Companion", "Officer", "Member", "Medal"))) %>% 
  ungroup() 

weeks_total <- wikipedia %>% 
  select(week_diff, award_comb) %>% 
  group_by(week_diff, award_comb) %>% 
  tally(name="week_num") %>% 
  filter(week_diff >-53 & week_diff <53) %>% 
  ungroup() %>% 
  summarise(sum(week_num))

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
  labs(
    # title = "Wikipedia Pages created for Order recipients 52 weeks pre- and post-announcement",
       # subtitle = "53 pages were created on week of Order announcement compared to an average of approximatley 1.89 in other weeks ",
       caption = "\n\nn=403 Wikipedia pages were created for Order of Australia recipients in the 52 weeks prior and post announcement",
       x="Number of weeks pre- and post-Order announcement",
       y=NULL)+
  theme_minimal()+
  guides(fill = guide_legend(title = "No. Wikipedia\npages created\nin week"))
  
week_award_chart

ggsave("week_award.png", width= 25, height = 15, units=c("cm") )


week_award_gender <- wikipedia %>% 
  select(week_diff, award_comb, gender) %>% 
  group_by(week_diff, award_comb, gender) %>% 
  tally(name="week_num") %>% 
  filter(week_diff >-52 & week_diff <53) %>% 
  filter(award_comb!="ADK") %>% 
  mutate(award = as_factor(case_when(award_comb == "AC" ~ "Companion",
                                     award_comb == "AO" ~ "Officer",
                                     award_comb == "AM" ~ "Member",
                                     award_comb == "OAM" ~ "Medal"))) %>% 
  mutate(award = fct_relevel(award, c("Companion", "Officer", "Member", "Medal"))) %>% 
  ungroup() 

week_award_chart_gender <- ggplot(week_award_gender, aes(week_diff, award,fill=week_num)) +
  geom_tile()+
  facet_wrap(~gender, ncol=1)+
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






pre_post_data <- wikipedia %>% 
  mutate(gender = case_when(gender == "TF" ~"Female",
                            gender == "F" ~"Female",
                            gender == "M" ~"Male",
                            TRUE ~ as.character(gender))) %>% 
  select(week_diff, gender) %>% 
  mutate(pre_post = case_when(week_diff < 0 ~ "Before",
                              TRUE ~ "After")) %>% 
  group_by(gender) %>% 
  add_tally(name="total_gender") %>% 
  group_by(gender, pre_post) %>% 
  add_tally(name="total_gender_when") %>% 
  mutate(prop=total_gender_when/total_gender) %>% 
  group_by(gender, pre_post, prop, total_gender_when) %>% 
  summarise() 
  
  
  
  # View(pre_post)
  
ggplot(pre_post_data, aes(gender, prop, fill=pre_post))+
  geom_col()+
  scale_y_continuous(labels = percent)+
  geom_text(aes(label = percent(round(prop,2))),
            position = position_fill(vjust = 0.5)) +
  scale_x_discrete(labels = pre_post_data %>% 
                     group_by(gender) %>% 
                     summarize(n = sum(total_gender_when)) %>%
                     mutate(lab = paste0(gender, " (", n, ")")) %>%
                     pull(lab))+
  labs(
    title = "Order of Australia Recipients with Wikipedia page: Page created before or after award",
       caption = "n=4,883 Order of Australia Recipients with wikipedia page",
       y=NULL,
       x=NULL)+
  scale_fill_manual(values =award_colour)+
  theme_minimal()+
  guides(fill = guide_legend(title = "Wikipedia Page Creation"))


pre_post_chart <- ggplot(pre_post_data, aes(gender, total_gender_when, fill=pre_post))+
  geom_col(width=.75)+
  # scale_y_continuous(labels = percent)+
  geom_text(aes(label = percent(round(prop,2))),
            position = position_stack(vjust = .5)) +
  scale_x_discrete(labels = pre_post_data %>% 
                     group_by(gender) %>% 
                     summarize(n = sum(total_gender_when)) %>%
                     mutate(lab = paste0(gender, " (", n, ")")) %>%
                     pull(lab))+
  labs(
    # title = "Order of Australia Recipients with Wikipedia page:\nPage created before or after Order received",
       caption = "n=4,883 Order of Australia Recipients with Wikipedia page",
       y=NULL,
       x=NULL)+
  scale_fill_manual(values =award_colour)+
  theme_minimal()+
  guides(fill = guide_legend(title = "Wikipedia Page Creation"))+
  theme(
    legend.position = c(0.05, .9),
    legend.justification = c("left", "top"),
    legend.direction="vertical",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

pre_post_chart
ggsave("pre_post_chart.png", width= 20, height = 10, units=c("cm") )


pre_post_filter <- wikipedia %>% 
  filter(honours_date >= "2001-01-15") %>% 
  mutate(gender = case_when(gender == "TF" ~"Female",
                            gender == "F" ~"Female",
                            gender == "M" ~"Male",
                            TRUE ~ as.character(gender))) %>% 
  select(week_diff, gender) %>% 
  mutate(pre_post = case_when(week_diff < 0 ~ "Before",
                              TRUE ~ "After")) %>% 
  group_by(gender) %>% 
  add_tally(name="total_gender") %>% 
  group_by(gender, pre_post) %>% 
  add_tally(name="total_gender_when") %>% 
  mutate(prop=total_gender_when/total_gender) %>% 
  group_by(gender, pre_post, prop, total_gender_when) %>% 
  summarise() 

View(wikipedia)


  
  



pre_post_filter_chart <- ggplot(pre_post_filter, aes(gender, total_gender_when, fill=pre_post))+
  geom_col(width=.75)+
  # scale_y_continuous(labels = percent)+
  geom_text(aes(label = percent(round(prop,2))),
            position = position_stack(vjust = .5)) +
  scale_x_discrete(labels = pre_post_data %>% 
                     group_by(gender) %>% 
                     summarize(n = sum(total_gender_when)) %>%
                     mutate(lab = paste0(gender, " (", n, ")")) %>%
                     pull(lab))+
  labs(
    # title = "Order of Australia Recipients post 15 January 2001 with Wikipedia page:\nPage created before or after Order received",
       caption = "n=3,000 Order of Australia Recipients post 15 January 2001 with Wikipedia page",
       y=NULL,
       x=NULL)+
  scale_fill_manual(values =award_colour)+
  theme_minimal()+
  guides(fill = guide_legend(title = "Wikipedia Page Creation"))+
  theme(
    legend.position = c(0.05, .9),
    legend.justification = c("left", "top"),
    legend.direction="vertical",
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

pre_post_filter_chart
ggsave("pre_post_filter_chart.png", width= 20, height = 10, units=c("cm") )



##recenecy

pre_post_recency <- wikipedia %>% 
  mutate(order_when = case_when(honours_date >= "2001-01-15" ~ "Order after Wikipedia",
                                honours_date < "2001-01-15" ~ "Order before Wikipedia")) %>% 
  select(order_when) %>% 
  add_tally() %>% 
  group_by(order_when) %>% 
  add_tally(name="order_when_total") %>% 
  group_by(order_when, n, order_when_total) %>% 
  summarise(prop = sum(order_when_total)/sum(n))



# pre_post_recency <- recipient %>% 
#   mutate(order_when = case_when(honours_date >= "2001-01-15" ~ "Order after Wikipedia",
#                                 honours_date < "2001-01-15" ~ "Order before Wikipedia")) %>% 
#   select(order_when, wikipedia_page) %>% 
#   group_by(order_when) %>% 
#   add_tally(name="order_when_total") %>% 
#   group_by(order_when, wikipedia_page) %>% 
#   add_tally() %>% 
#   group_by(order_when, n, order_when_total, wikipedia_page) %>% 
#   summarise(prop = sum(n)/sum(order_when_total))
# 
# pre_post_recency_chart <- ggplot(pre_post_recency, aes(order_when, n, fill=wikipedia_page)) +
#   geom_col()


View(gender_cum_sum)

gender_cum_sum <- wikipedia %>% 
  mutate(gender = case_when(gender == "TF" ~"Female",
                            gender == "F" ~"Female",
                            gender == "M" ~"Male",
                            TRUE ~ as.character(gender))) %>% 
  group_by(gender) %>% 
  # mutate(gender = fct_relevel(gender, c("Male", "Female"))) %>% 
  select(wp_creation_date, gender) %>% 
  add_tally(name="gender_tally") %>% 
  mutate(gender_label=paste0(gender,"\n(",gender_tally,")")) %>% 
  group_by(wp_creation_date, gender_label) %>% 
  tally() %>% 
  group_by(gender_label) %>% 
  arrange(desc(gender_label),wp_creation_date) %>% 
  mutate(cum = cumsum(n)) %>% 
  mutate(cum_prop = cum/sum(n)) 
  
  
gender_total_data <- gender_cum_sum %>%
  # mutate(gender = case_when(gender == "TF" ~"Female",
  #                           gender == "F" ~"Female",
  #                           gender == "M" ~"Male",
  #                           TRUE ~ as.character(gender))) %>% 
  group_by(gender_label) %>% 
  tally()

write.csv(gender_cum_sum, "gender_cum_sum.csv")


dates <- as.Date("01/01/1900", "%d/%m/%Y") + floor( 36500 * runif(100000) )

quantile(dates, probs = c(0.001, 0.025, 0.975, 0.999), type = 1)

View(quarters)

quarters <- wikipedia %>% 
  ungroup() %>% 
  select(wp_creation_date, gender) %>%
  group_by(gender) %>% 
  add_tally(name="gender_tally") %>% 
  mutate(gender_label=paste0(gender,"\n(",gender_tally,")")) %>% 
  select(wp_creation_date, gender_label) %>% 
  ungroup() %>% 
  group_by(gender_label) %>% 
  # mutate(wp_creation_date = as.Date(wp_creation_date)) %>% 
  reframe(qt = quantile(wp_creation_date, c(0.25, 0.5, .75, 1), type=1))  %>% 
  mutate(quartile = c(0.25, 0.5, .75,1, 0.25, 0.5, .75, 1)) %>% 
  mutate(text = c("25%: Dec 2006 ",
                  "50%: Jan 2012",
                  "75%: Sept 2017",
                  "100%: 15 Jan 2023",
                  
                  "25%: Jan 2006 ",
                  "50%: Jun 2008",
                  "75%: May 2013",
                  "100%: 30 Jan 2023")) 

gedner_check <- quar

gender_cum_sum_chart <- ggplot(gender_cum_sum, aes(wp_creation_date, cum_prop, fill=gender_label))+
  geom_area(alpha=.7, show.legend = FALSE)+
  facet_wrap(~gender_label)+
  geom_segment(data=quarters, aes(x=ymd(qt), y=0, xend=qt, yend=quartile+.15))+
  scale_fill_manual(values = gender_colour)+
  theme_minimal()+
  scale_y_continuous(labels = percent, breaks = c(.25, .5, .75, 1))+
  geom_point(data=quarters,aes(x=ymd(qt), y=quartile),shape=21, fill="white")+
  geom_text(data=quarters, aes(x=ymd(qt), y=quartile+0.2, label=text), hjust=1, size=3)+
  labs(
    # title = "Pace of cumulative creation of Wikipedia pages for Order recipients",
       caption = "n=4,833 Order of Australia recipients with Wikipedia pages",
       x=NULL,
       y=NULL)

gender_cum_sum_chart
  
  
ggsave("gender_cum_sum_chart.png", width= 20, height = 10, units=c("cm") )

  
  