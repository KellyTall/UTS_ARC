# install.packages("gender")
# install.packages("remotes")
# remotes::install_github("lmullen/genderdata")
library(tidyverse)
library(gender)
library(stringr)
library(janitor)


order <- c("Member of the Order of Australia", "Companion of the Order of Australia", "Officer of the Order of Australia", "Medal of the Order of Australia")

gender_test_import <- read_csv("aus_day_2023_additions.csv") %>% 
  clean_names() %>% 
  filter(award_name  %in% order) %>% 
  mutate(given_name=gazette_given_name) %>% 
  separate(given_name, c("first", "second")) 


gender_test_import_22 <- read_csv("aus_day_2022_additions.csv") %>% 
  clean_names() %>% 
  filter(award_name  %in% order) %>% 
  mutate(given_name=gazette_given_name) %>% 
  separate(given_name, c("first", "second")) 


# View(gender_test_import_22)  
View(gender_test_import_missing)

gender_test_import_missing <- read_csv("missing_honours.csv") %>% 
  clean_names() %>% 
  filter(award_name  %in% order) %>% 
  mutate(given_name=gazette_given_name) %>% 
  separate(given_name, c("first", "second")) 



# gedner_test_code <-gender_test_import %>% 
#   select(award_name) %>% 
#   group_by(award_name) %>% 
#   tally()


# View(gender_test_import)
# 
# View(gender_test)

gender_test <- gender_test_import %>% 
  select(first) %>% 
  distinct()

gender_test_22 <- gender_test_import_22 %>% 
  select(first) %>% 
  distinct()


gender_test_missing <- gender_test_import_missing %>% 
  select(first) %>% 
  distinct()

View(gender_pred)
gender_pred <- gender(gender_test$first, method = "ssa") %>% 
  rename(first=name)
  
gender_pred_22 <- gender(gender_test_22$first, method = "ssa") %>% 
  rename(first=name)


gender_pred_missing <- gender(gender_test_missing$first, method = "ssa") %>% 
  rename(first=name)


# View(gender_pred_merge)

gender_pred_merge <- right_join(gender_pred, gender_test_import, by="first") %>% 
  select(-year_min, -year_max) %>% 
  mutate(gender = case_when(gazette_name=="Professor Kerry Brian WALSH"~ "male",
                            gazette_name=="Mr Robin Charles GEHLING"~ "male",
                            gazette_name=="Mr Lindsay Clifford CLARKE"~ "male",
                            gazette_name=="Mr Ashley DONALDSON"~ "male",
                            gazette_name=="Ms Robbie SEFTON"~ "female",
                            gazette_name=="Mr Nicola CERRONE"~ "male",
                            gazette_name=="The late Mr Rosario ROCCA"~ "male",
                            gazette_name=="Dr Leslie Karl NATHANSON"~ "male",
                            gazette_name=="Dr Leslie Karl NATHANSON"~ "male",
                            .default = as.character(gender))) %>% 
  arrange(gender, desc(proportion_male)) %>% 
  mutate(gender = case_when(
    gender=="female" ~"F",
    gender=="male" ~ "M"
  )) %>% 
  select(-c(first, proportion_male, proportion_female, second)) %>% 
  relocate(gender, .after = last_col())
  


# View(gender_pred_merge_22)

gender_pred_merge_22 <- right_join(gender_pred_22, gender_test_import_22, by="first") %>% 
  select(-year_min, -year_max) %>% 
  mutate(gender = case_when(gazette_name=="Mr Leslie James POWER"~ "male",
                            gazette_name=="Mr Leslie Allan DENNIS"~ "male",
                            gazette_name=="The late Mr Leslie David RUSSELL"~ "male",
                            gazette_name=="Dr Robin Clifford McLACHLAN"~ "male",
                            gazette_name=="Mr Jean VAN DER WESTHUYZEN"~ "male",
                            gazette_name=="Mr Jamie HYAMS"~ "male",
                            gazette_name=="Warrant Officer Tagan James WRIGHT"~ "male",
                            gazette_name=="Mr Campbell Robert BOLWELL"~ "male",
                            gazette_name=="Emeritus Professor Leslie Michael IRWIG"~ "male",
                            gazette_name=="Professor Leslie BURNETT"~ "male",
                            gazette_name=="Mr Keran Thomas MAGUIRE"~ "male",
                            gazette_name=="Mr Kim Beresford RICKARDS"~ "male",
                            gazette_name=="Rear Admiral Jaimie Charles HATCHER DSC AM RAN"~ "male",
                            gazette_name=="Ms Lee LIBERMAN"~ "female",
                            gazette_name=="Ms Shayne Joan WILDE"~ "female",
                            gazette_name=="Ms Criss CANNING"~ "female",
                            gazette_name=="Mrs Andy GILD"~ "female",
                            .default = as.character(gender))) %>%
  arrange(desc(gender), desc(proportion_female)) %>% 
  mutate(gender = case_when(
    gender=="female" ~"F",
    gender=="male" ~ "M"
  )) %>% 
  select(-c(first, proportion_male, proportion_female, second)) %>% 
  relocate(gender, .after = last_col())


View(gender_pred_merge_missing)

gender_pred_merge_missing <- right_join(gender_pred_missing, gender_test_import_missing, by="first") %>% 
  select(-year_min, -year_max) %>% 
  mutate(gender = case_when(gazette_name=="Dr Kiran MAZUMDAR-SHAW"~ "female",
                            .default = as.character(gender))) %>%
  arrange(gender, desc(proportion_male)) %>% 
  mutate(gender = case_when(
    gender=="female" ~"F",
    gender=="male" ~ "M"
  )) %>% 
  select(-c(first, proportion_male, proportion_female, second)) %>% 
  relocate(gender, .after = last_col())






honours_additions_gender <- rbind(gender_pred_merge_22,gender_pred_merge, gender_pred_merge_missing) %>% 
  mutate(gender = case_when(str_detect(gazette_name, "Mrs") ~ "F",
                            str_detect(gazette_name, "Ms") ~ "F",
                            str_detect(gazette_name, "Miss") ~ "F",
                            str_detect(gazette_name, "Mr") ~ "M",
                            gazette_name == "Commodore Braddon John WHEELER RAN"~ "M",
                            gazette_name == "Professor Roslynne Elizabeth HANSEN"~ "F",
                            gazette_name == "The Honourable Eadley Graeme STONEY"~ "M",
                            gazette_name == "Captain  P"~ "U",
                            gazette_name == "The Honourable Emilios John KYROU"~ "M",
                            gazette_name == "Dr Furio John VIRANT"~ "M",
                            gazette_name == "Dr Shailja CHATURVEDI"~ "F",
                            gazette_name == "Professor Tissa WIJERATNE"~ "M",
                            gazette_name == "Emerita Professor Ngaire May NAFFINE"~ "F",
                            gazette_name == "Dr Criena FITZGERALD"~ "F",
                            gazette_name == "Professor Helge Hans RASMUSSEN"~ "M",
                            gazette_name == "Professor Elsdon STOREY"~ "M",
                            gazette_name == "Professor S Alexander HASLAM"~ "M",
                            gazette_name == "Professor Xinhua WU"~ "F",
                            gazette_name == "Professor Hua Kun LIU"~ "F",
                            gazette_name == "Professor Farees (Fary) KHAN"~ "F",
                            gazette_name == "Professor Prithvipall Singh BHATHAL"~ "M",
                            .default = as.character(gender))) 


View(honours_additions_gender_na)

write_csv(honours_additions_gender, "honours_additions_gender.csv") 

