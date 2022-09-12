library(tidyverse)
library(janitor)
library(WikidataQueryServiceR)

honour_1 <- read_csv("master_list.csv") %>% 
  clean_names()

# View(honour_1)

##extracting details from wikidata - using WikidataQueryServiceR

wikidata_data <- query_wikidata("SELECT ?person ?personLabel ?personDescription ?refurl ?orderaus ?award_name ?honsid ?date_awarded ?sitelink 
WHERE {
  ?person wdt:P31 wd:Q5 .
  ?person p:P166 ?award .
  ?award ps:P166 ?orderaus .
  ?orderaus wdt:P361 wd:Q1141149 .
  OPTIONAL {?award pq:P585 ?date_awarded}.
  OPTIONAL {?award prov:wasDerivedFrom ?ref .
  ?ref pr:P854 ?refurl } .
  OPTIONAL {?award prov:wasDerivedFrom ?ref .
  ?ref pr:P4766 ?honsid } .
  OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en' }
}")

View(wikidata_data)


## what records have no honsid on file - sweep 1
award_url_hons_id_fix_check <- wikidata_data %>% 
  mutate(de_dupe = paste(person,orderaus)) %>% 
  distinct(de_dupe, .keep_all = TRUE) %>% 
  filter(is.na(honsid))

View(award_url_hons_id_fix_check)

## taking award ID from award url string




##getting honoursid from the refurl of the honors listing then merging back into wikidata extract

award_url_hons_id_fix1 <- award_url_hons_id_fix_check %>% 
  mutate(newId = refurl) %>% 
  filter(str_starts(newId, "https://honours.pmc.gov.au/honours/awards/")) %>% 
  mutate(newId = str_replace_all(newId, "https://honours.pmc.gov.au/honours/awards/", "")) %>% 
  select(-honsid) %>% 
  rename(honsid=newId) %>% 
  select(honsid, person,refurl) %>% 
  right_join(wikidata_data, by=c("person", "refurl")) 
  
View(award_url_hons_id_fix1)

## what records have no honsid on file - sweep 2
award_url_hons_id_fix_check2 <- award_url_hons_id_fix1 %>% 
  filter(is.na(honsid.x) & is.na(honsid.y))

View(award_url_hons_id_fix_check2)

  