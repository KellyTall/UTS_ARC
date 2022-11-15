library(tidyverse)
library(lubridate)
library(WikipediR)
library(httr)
library(RSelenium)
library(rvest)
library(xml2)
library(janitor)

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
wikidata_data_id_na <- wikidata_data %>% 
  filter(is.na(honsid))

View(wikidata_data_id_na)


wikidata_data_id <- wikidata_data %>% 
  filter(!is.na(honsid))


##getting honoursid from the refurl of the honors listing then merging back into wikidata extract

award_url_hons_id_fix1 <- wikidata_data_id_na %>% 
  mutate(newid = refurl) %>% 
  # filter(str_starts(newId, "https://honours.pmc.gov.au/honours/awards/")) %>%
  mutate(newid = str_replace_all(newid, "https://honours.pmc.gov.au/honours/awards/", "")) %>% 
  mutate(newid = str_replace_all(newid, "https://www.itsanhonour.gov.au/honours/honour_roll/search.cfm", "")) %>% 
  mutate(newid = str_replace_all(newid, "&search_type=quick&showInd=true", "")) %>% 
  mutate(newid = str_replace_all(newid, "\\?aus_award_id=", "")) %>% 
  mutate(newid = str_replace_all(newid, "http://old.gg.gov.au/.*", "NA")) %>% 
  mutate(newid = str_replace_all(newid, "https://www.smh.com.au/.*", "NA"))  %>%
  mutate(newid = str_replace_all(newid, "https://www.theguardian.com/.*", "NA"))  %>%
  mutate(newid = str_replace_all(newid, "http://gg.gov.au/.*", "NA")) %>% 
  mutate(newid = str_replace_all(newid, "https://www.gg.gov.au/.*", "NA")) %>% 
  mutate(newid = str_replace_all(newid, "https://www.legislation.gov.au/.*", "NA")) %>% 
  mutate(newid = str_replace_all(newid, "https://www.wa.gov.au/.*", "NA")) %>% 
  mutate(newid = str_replace_all(newid, "https://www.abc.net.au/.*", "NA")) %>% 
  mutate(newid = str_replace_all(newid, "https://web.archive.org/.*", "NA"))  %>% 
  mutate(newid = str_replace_all(newid, "https://www.cbaa.*", "NA"))  %>% 
  mutate(newid = as.numeric(newid)) %>% 
  select(-honsid) %>% 
  rename(honsid = newid) %>% 
  filter(!is.na(honsid)) %>% 
  select(1:6, 9, 7:8)
  
wikidata <- rbind(wikidata_data_id, award_url_hons_id_fix1)  
View(wikidata)
wikidata_wp <- wikidata %>% 
  filter (!is.na(sitelink))

# wikidata_duplicates <- wikidata_wp %>% 
#   group_by(sitelink) %>% 
#   count(sitelink) %>% 
#   filter(n>1) %>% 
#   tally()


##preparing for wikipedia first page edit 

wikipedia_page_list <- wikidata %>% 
  mutate(wikipedia_page = case_when(!is.na(sitelink) ~ "Yes",
                                    TRUE ~ "No")) %>% 
  rename(wikidata_link = person) %>% 
  filter(wikipedia_page =="Yes") %>% 
  rename(wikipedia_url = sitelink) %>% 
  distinct()

# fixing issues with text

## preparing data to extract wikipage ID

View(wikipedia_page_query)

wikipedia_page_query <- wikipedia_page_list %>%
  select(wikipedia_url) %>% 
  mutate(name = str_remove(wikipedia_url, "https://en.wikipedia.org/wiki/")) %>% 
  mutate(name = str_replace_all(name, "_", " ")) %>% 
  mutate(name = str_replace_all(name, "%27", "'")) %>% 
  mutate(name = str_replace_all(name, "%C3%B8", "ø")) %>% 
  mutate(name = str_replace_all(name, "%C3%96", "Ö")) %>% 
  mutate(name = str_replace_all(name, "%C3%A3", "ã")) %>% 
  mutate(name = str_replace_all(name, "%C5%8D", "ō")) %>% 
  mutate(name = str_replace_all(name, "%C5%8C", "Ō")) %>% 
  mutate(name = str_replace_all(name, "%C3%A0", "à")) %>% 
  mutate(name = str_replace_all(name, "%C3%A9", "é")) %>% 
  mutate(name = str_replace_all(name, "%22", '"')) %>% 
  mutate(name = str_replace_all(name, "%C3%B3", 'ó')) %>% 
  mutate(name = str_replace_all(name, "%E2%80%93", '–')) %>% 
  mutate(name = str_replace_all(name, "%C3%A1", 'á')) %>% 
  mutate(name = str_replace_all(name, "%C5%99%C3%AD", 'ří')) %>% 
  mutate(name = str_replace_all(name, "%C3%B6", 'ö')) %>% 
  # mutate(name = str_remove_all(name , "[[\\p{P}][\\p{S}]]")) %>% 
  distinct()

# extracts wikipedia page information for each record - takes some time!


wp_1 <- wikipedia_page_query %>% 
  slice(1:1000)

wp_2 <- wikipedia_page_query %>% 
  slice(1001:2000)

wp_3 <- wikipedia_page_query %>% 
  slice(2001:3000)

wp_4 <- wikipedia_page_query %>% 
  slice(3001:4000)

wp_5 <- wikipedia_page_query %>% 
  slice(4001:6000)


# getinfo <- page_info("en", "wikipedia", page="Ken Wallace (canoeist)", clean_response = TRUE)


wikipedia_page_extraction_1 <- lapply(wp_1$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE) 
})


wikipedia_page_extraction_2 <- lapply(wp_2$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
  # getinfo <- page_info("en", "wikipedia", page="Jacques Cousteau", clean_response = TRUE)
})

wikipedia_page_extraction_3 <- lapply(wp_3$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
  # getinfo <- page_info("en", "wikipedia", page="Jacques Cousteau", clean_response = TRUE)
})

wikipedia_page_extraction_4 <- lapply(wp_4$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
  # getinfo <- page_info("en", "wikipedia", page="Jacques Cousteau", clean_response = TRUE)
})

wikipedia_page_extraction_5 <- lapply(wp_5$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
  # getinfo <- page_info("en", "wikipedia", page="Jacques Cousteau", clean_response = TRUE)
})


wikipedia_page_extraction_unlist <- unlist(wikipedia_page_extraction_1, recursive=FALSE)

wikipedia_page_extraction_format <- tibble (
  id = map(wikipedia_page_extraction_unlist, "pageid"),
  wpURL = map(wikipedia_page_extraction_unlist, "fullurl"),
  name = map(wikipedia_page_extraction_unlist, "displaytitle")) %>%
  mutate(
    pageid = map_chr(id, 1, .default = NA),
    wpURL    = map_chr(wpURL, 1, .default = NA),
    displayName = map_chr(name, 1, .default = NA)
  ) %>%
  select (-id, -name) %>%
  clean_names() %>% 
  mutate( wp_url   = str_replace_all( wp_url  , "%27", "'")) %>%
  mutate( wp_url = str_replace_all(wp_url, "%C3%B8", "ø")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%96", "Ö")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%A3", "ã")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C5%8D", "ō")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C5%8C", "Ō")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%A0", "à")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%A9", "é")) %>%
  mutate(wp_url = str_replace_all(wp_url, "%22", '"')) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%B3", 'ó')) %>%
  mutate(wp_url = str_replace_all(wp_url, "%E2%80%93", '–')) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%A1", 'á')) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C5%99%C3%AD", 'ří')) %>%
  mutate(wp_url = str_replace_all(wp_url, "%C3%B6", 'ö')) %>%
  rename(wp_pageid = pageid)




  