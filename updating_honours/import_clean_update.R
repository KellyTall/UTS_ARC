library(tidyverse)
library(lubridate)
library(WikipediR)
library(WikidataQueryServiceR)
library(httr)
library(RSelenium)
library(rvest)
library(xml2)
library(janitor)
library(stringi)


honour_prev <- read_csv("master_list.csv") %>% 
  clean_names() %>% 
  select(-rec_name)

# colnames(honour_1)

honours_additions <- read_csv("honours_additions_gender.csv") %>% 
  clean_names() %>% 
  select(-media_note)

honour_1 <- rbind(honour_prev, honours_additions)

write_csv(honour_1, "complete_honours_20230318.csv")

# announcement <- honour_1 %>% 
#   select(announcement_event) %>% 
#   group_by(announcement_event) %>% 
#   tally()
# View(announcement)

# colnames(honours_additions)

# View(honour_1)
# View(honour_aus_day_23)

##extracting details from wikidata - using WikidataQueryServiceR



wikidata_data_query <- query_wikidata("SELECT ?person ?personLabel ?personDescription ?refurl ?orderaus ?award_name ?honsid ?date_awarded ?sitelink 
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
  

# View(wikidata_data_query)

write_csv(wikidata_data_query, "wikidata_data_query.csv")

##removing all who are either listed as anon / returned award / 
## had award cancelled due to non-complete records - record does not match the PMO list  

wikidata_data_valid <- wikidata_data_query %>%
  clean_names() %>% 
  mutate(return_remove_anon = case_when(person_label == "Richard Pratt" ~ "missing",
                                   person_label == "Geoffrey Smith" ~ "missing",
                                   person_label == "Peter Harrison" ~ "missing",
                                   person_label == "Dyson Heydon" ~ "missing",
                                   person_label == "Peter Kingston" ~ "missing",
                                   person_label == "Tom Middendorp" ~ "missing",
                                   person_label == "Rolf Harris" ~ "missing",
                                   person_label == "Eddie Obeid" ~ "missing",
                                   person_label == "Peter Tinley" ~ "missing",
                                   person_label == "Sarah Bowen" ~ "missing",
                                   person_label == "Paul Wilson" ~ "missing",
                                   person_label == "Teodoro Esteban López Calderón" ~ "missing",
                                   person_label == "Ashi Tashi Dorji" ~ "missing",
                                   person_label == "Dean Butler" ~ "missing",
                                   person_label == "Joseph F. Dunford, Jr." ~ "missing",
                                   person_label == "H. C. Coombs" ~ "missing",
                                   person_label == "Ian MacDougall" ~ "missing",
                                   person_label == "Édouard Philippe" ~ "missing",
                                   person_label == "Simon Poidevin" ~ "missing",
                                   person_label == "Kelvin Khong" ~ "missing",
                                   person_label == "Clyde Wood" ~ "missing",
                                   TRUE ~ "valid"))
                                   
  
wikidata_data <- wikidata_data_valid %>% 
  filter(return_remove_anon=="valid") %>% 
  select(-return_remove_anon)

# no_id <- wikidata_data %>% 
#   filter(str_detect(refurl, "https://honours.pmc.gov.au/honours/awards/", negate=TRUE) & return_remove_anon=="Valid")

# View(dupe_wiki)

## what records have no honsid on file - sweep 1
wikidata_data_id_na <- wikidata_data %>% 
  filter(is.na(honsid))

# View(wikidata_data_id)

##all records with hondsid
wikidata_data_id <- wikidata_data %>% 
  filter(!is.na(honsid)) 



##getting honoursid from the refurl of the honors listing then merging back into wikidata extract
# View(award_url_hons_id_fix1)

award_url_hons_id_fix1 <- wikidata_data_id_na %>% 
  mutate(newid = refurl) %>% 
  mutate(newid = str_replace_all(newid, "https://honours.pmc.gov.au/honours/awards/", "")) %>% 
  mutate(newid = as.numeric(newid)) %>%
  select(-honsid) %>%
  rename(honsid = newid) 


hons_id_fixed1 <- award_url_hons_id_fix1 %>% 
  filter(!is.na(honsid)) %>% 
  select(1:6, 9, 7:8)
  
  # 

##second sweep of no hons ID - this should be null if all fixed properly
wikidata_data_id_na2 <- award_url_hons_id_fix1 %>% 
    filter(is.na(honsid))

View(wikidata_data_id_na2)

wikidata <- rbind(wikidata_data_id, hons_id_fixed1)  

# View(wikidata)
write_csv(wikidata, "wikidata.csv")

wikidata_wp <- wikidata %>% 
  filter (!is.na(sitelink))

write_csv(wikidata_wp, "wikidata_wp.csv")

##check that all have an honours ID
wikidata_data_id <- wikidata_wp %>% 
  filter(is.na(honsid))

# View(wikidata_duplicates)
# 
# wikidata_duplicates <- wikidata_wp %>%
#   group_by(sitelink) %>%
#   count(sitelink) %>%
#   filter(n>1) %>%
#   tally()


##preparing for wikipedia first page edit 

# View(wikipedia_page_list)
# View(wikidata)
# 
# View(wikidata_wp)

wikipedia_page_list <- wikidata_wp %>% 
  mutate(wikipedia_page = case_when(!is.na(sitelink) ~ "Yes",
                                    TRUE ~ "No")) %>% 
  rename(wikidata_link = person) %>% 
  filter(wikipedia_page =="Yes") %>% 
  rename(wikipedia_url = sitelink) 


wp_check_unique <- wikipedia_page_list %>% 
  select(wikipedia_url) %>% 
  distinct()





# View(wikipedia_page_query)
# View(wikipedia_page_list)

## preparing data to extract wikipage ID # fixing issues with text

wikipedia_page_query <- wikipedia_page_list %>%
  select(wikipedia_url, person_label) %>% 
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
  distinct()
  

# extracts wikipedia page information for each record - takes some time!
## splits file into smaller number of rows to prevent timings out
# View(wp_1)
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


# getinfotest <- page_info("en", "wikipedia", page='Wilfred James "Bill" Gray', clean_response = TRUE)

# getinfo_missing_Barbara_York <- page_info("en", "wikipedia", page="Barbara York Main", clean_response = TRUE)
# getinfo_missing_Sue_packer <- page_info("en", "wikipedia", page="Totti Cohen", clean_response = TRUE)


wikipedia_page_extraction_1 <- lapply(wp_1$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE) 
})
 

wikipedia_page_extraction_2 <- lapply(wp_2$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
  # getinfo <- page_info("en", "wikipedia", page="Jacques Cousteau", clean_response = TRUE)
})

wikipedia_page_extraction_3 <- lapply(wp_3$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
})


wikipedia_page_extraction_4 <- lapply(wp_4$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
})


wikipedia_page_extraction_5 <- lapply(wp_5$name, function (i) {
  getinfo <- page_info("en", "wikipedia", page=i , clean_response = TRUE)
})


##unlisting the above lists!

wikipedia_page_extraction_unlist_1 <- unlist(wikipedia_page_extraction_1, recursive=FALSE)
wikipedia_page_extraction_unlist_2 <- unlist(wikipedia_page_extraction_2, recursive=FALSE)
wikipedia_page_extraction_unlist_3 <- unlist(wikipedia_page_extraction_3, recursive=FALSE)
wikipedia_page_extraction_unlist_4 <- unlist(wikipedia_page_extraction_4, recursive=FALSE)
wikipedia_page_extraction_unlist_5 <- unlist(wikipedia_page_extraction_5, recursive=FALSE)


##combining the lists
wikipedia_page_extraction_unlist <- c(wikipedia_page_extraction_unlist_1,
                                      wikipedia_page_extraction_unlist_2,
                                      wikipedia_page_extraction_unlist_3,
                                      wikipedia_page_extraction_unlist_4,
                                      wikipedia_page_extraction_unlist_5)


## formatting for extraction
# View(wikipedia_page_extraction_format)

wikipedia_page_extraction_format <- tibble (
  id = map(wikipedia_page_extraction_unlist, "pageid"),
  wpURL = map(wikipedia_page_extraction_unlist, "fullurl"),
  name = map(wikipedia_page_extraction_unlist, "displaytitle")) %>%
  mutate(
    wp_pageid = map_dbl(id, 1, .default = NA),
    wpURL= map_chr(wpURL, 1, .default = NA),
    displayName = map_chr(name, 1, .default = NA))%>%
  select (-id, -name) %>%
  clean_names() %>%
  mutate( wp_url  = str_replace_all( wp_url, "%27", "'")) %>%
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
  mutate(row_num = row_number())
  


##check that all IDs have been linked to record with query
check_no_id <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>%
  filter(is.na(wp_pageid))


# write_csv(wikipedia_page_extraction_format, "wikipedia_page_extraction_format.csv")

# wikipedia_page_extraction_format <- read_csv("wikipedia_page_extraction_format.csv")


View(wp_page_create_search19)

wp_page_create_search1 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>%
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(1:250)


wp_page_create_search2 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(251:500)


wp_page_create_search3 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(501:750)

wp_page_create_search4 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(751:1000)


wp_page_create_search5 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(1001:1250)

wp_page_create_search6 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(1251:1500)

wp_page_create_search7 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(1501:1750)

wp_page_create_search8 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(1751:2000)



wp_page_create_search9 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(2001:2250)

wp_page_create_search10 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(2251:2500)

wp_page_create_search11 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(2501:2750)

wp_page_create_search12 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(2751:3000)
View(wp_page_create_search13)

wp_page_create_search13 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>%
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>%
  slice(3001:3250)


wp_page_create_search14 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(3251:3500)


wp_page_create_search15 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(3501:3750)

wp_page_create_search16 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(3751:4000)

wp_page_create_search17 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(4001:4250)

wp_page_create_search18 <- wikipedia_page_extraction_format %>% 
  select(wp_pageid) %>% 
  distinct() %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) %>% 
  slice(4251:4500)


View(wikipedia_page_extraction_format)
wp_page_create_search19 <- wikipedia_page_extraction_format %>% 
  slice(4501:5000) %>% 
  select(wp_pageid) %>% 
  mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
  mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
  select(search_url) 
  
# View(wp_page_create_search13)

##specia query for one record where page creation date was not being picked up
# wp_page_create_missing <- wikipedia_page_extraction_format %>% 
#   filter(wp_pageid==2994195) %>% 
#   select(wp_pageid) %>% 
#   mutate(base = "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&rvlimit=1&rvprop=timestamp&rvdir=newer&pageids=") %>% 
#   mutate(search_url=paste(base, wp_pageid, sep = "")) %>% 
#   select(search_url) 

# query of page creation date for each data set


wp_page_create_query1 <- lapply(wp_page_create_search1$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})



wp_page_create_query2 <- lapply(wp_page_create_search2$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query3 <- lapply(wp_page_create_search3$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query4 <- lapply(wp_page_create_search4$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query5 <- lapply(wp_page_create_search5$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})


wp_page_create_query6 <- lapply(wp_page_create_search6$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query7 <- lapply(wp_page_create_search7$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query8 <- lapply(wp_page_create_search8$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

# View(wp_page_create_search3)
wp_page_create_query9 <- lapply(wp_page_create_search9$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query10 <- lapply(wp_page_create_search10$search_url, function(i){
  
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query11 <- lapply(wp_page_create_search11$search_url, function(i){
  
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query12 <- lapply(wp_page_create_search12$search_url, function(i){
  
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>%
    
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

# View(wp_page_create_search13)

wp_page_create_query13 <- lapply(wp_page_create_search13$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})


wp_page_create_query14 <- lapply(wp_page_create_search14$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query15 <- lapply(wp_page_create_search15$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query16 <- lapply(wp_page_create_search16$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})


wp_page_create_query17<- lapply(wp_page_create_search17$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query18<- lapply(wp_page_create_search18$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

wp_page_create_query19<- lapply(wp_page_create_search19$search_url, function(i){
  date <- read_html(i)
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})


wp_page_create_jack_locket <- lapply(wp_page_create_missing$search_url, function(i){
  
  date <- read_html(i)
  
  EntryInfo <- html_nodes(date, ".s2") %>% 
    html_nodes(xpath="./text()[normalize-space()]") %>% 
    html_text(trim=TRUE) %>% 
    as_tibble() %>% 
    slice_tail(n=9) %>% 
    rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(names_from = rowname, values_from=value) %>% 
    select(-name, -`1`, -`4`, -`3`, -`5`, -`7`, -`8`) %>% 
    rename(pageID=`2`,
           name = `6`,
           pageCreation = `9`)
  
})

 
##bind rows together

# View(wp_page_create_query13)
wp_page_create_bind <- bind_rows(wp_page_create_query1, wp_page_create_query2, wp_page_create_query3,  wp_page_create_query4,
                                 wp_page_create_query5, wp_page_create_query6, wp_page_create_query7, wp_page_create_query8,
                                 wp_page_create_query9, wp_page_create_query10, wp_page_create_query11, wp_page_create_query12,
                                wp_page_create_query13, wp_page_create_query14, wp_page_create_query15, wp_page_create_query16,wp_page_create_query17, 
                                 wp_page_create_query18, wp_page_create_query19,
                                wp_page_create_jack_locket)

## format file - fixing time zone from UTC to Sydney to ensure correct time calculation

View(wp_page_create_bind)
View(wp_page_create_format)

wp_page_create_format <- wp_page_create_bind %>%  
  clean_names() %>% 
  mutate(page_id =str_remove_all(page_id , "[[\\p{P}][\\p{S}]]"),
         name=str_remove_all(name , "\""),
         # name=str_replace_all(name, '\'', '"' ),
         page_creation = str_remove_all(page_creation , "\"" ),
         page_creation = str_remove_all(page_creation , "Z" ),
         page_creation = str_replace_all(page_creation, "T", " ")) %>% 
  filter(page_id!="code") %>%
  mutate(page_creation = ymd_hms(page_creation, tz="UTC")) %>% 
  mutate(aus_page_creation = with_tz(page_creation, tzone = "Australia/Sydney")) %>% 
  ##relaces the source code to unicode character for merging
  mutate(name = stri_unescape_unicode(name)) %>%
  rename(wikipedia_page_id = page_id) %>% 
  mutate(wikipedia_page_id = as.numeric(wikipedia_page_id)) %>% 
  select(wikipedia_page_id,  aus_page_creation, name) %>% 
  mutate(name = case_when(name == 'Bela Bert Grof'~ 'Bela "Bert" Grof',
                          name == 'Peter Bullfrog Moore'~ 'Peter "Bullfrog" Moore',
                          name == 'Wilfred James Bill Gray'~ 'Wilfred James "Bill" Gray',
                          .default = as.character(name))) %>% 
  distinct()


write_csv(wp_page_create_format, "wp_page_create_format.csv")
# wp_page_create_format <- read_csv("wp_page_create_format.csv")
View(wp_page_create_format)

no_date <- wp_page_create_format %>% 
  filter(is.na(aus_page_creation))

# View(wikipedia_page_date)
# View(wikipedia_page_query)
# View(wp_page_create_format)


# wikipedia_page_query  <- wikipedia_page_query %>% 
#   select(-name) %>% 
#   rename(name =name_match_later)

## joining up 

View(wikipedia_page_date)
View(wp_page_create_format)
View(wikipedia_page_query)

wikipedia_page_date <- right_join(wp_page_create_format, wikipedia_page_query, by="name", multiple="all") 
  



# id_check <- wikipedia_page_date %>%
#   filter(is.na(wikipedia_page_id) )

View(wikipedia_page_list)
View(wikipedia_complete)

wikipedia_complete <- right_join(wikipedia_page_list, wikipedia_page_date, by="wikipedia_url", multiple="all") %>% 
  rename(award_id = honsid) %>% 
  filter(award_id!=977965) ##removes one case that was recorded incorrectly on wikidata at time of extraction (Yvette_Higgins as an order holder when she has a sports medal)

# View(honour_1)
# View(all_data_merge_honsid)

all_data_merge_honsid <- left_join(honour_1, wikipedia_complete, by="award_id", multiple="all") %>% 
  filter(award_id!=977965) %>%
  ##removes one case that was recorded incorrectly on wikidata at time of extraction (Yvette_Higgins as an order holder when she has a sports medal)

##data checks

# colnames(all_data_merge_honsid)
# 
# all_data_merge_honsid_check <- left_join(honour_1, wikipedia_complete, by="award_id")  %>%
#   filter(!is.na(wikipedia_url)) %>%
#   select(wikipedia_url, award_id) %>% 
#   filter(award_id!=977965) ##removes one case that was recorded incorrectly on wikidata at time of extraction (Yvette_Higgins as an order holder when she has a sports medal)

# View(honour_1)
# View(wikipedia_complete)
# View(merge_test)

# wp_page_check <- wikipedia_page_list %>%
#   select(wikipedia_url, honsid) %>%
#   rename(award_id=honsid) %>%
#   filter(award_id!=977965) ##removes one case that was recorded incorrectly on wikidata at time of extraction (Yvette_Higgins as an order holder when she has a sports medal)

  


# merge_test <- rbind(all_data_merge_honsid_check, wp_page_check) %>% 
#   group_by(wikipedia_url, award_id) %>% 
#   tally() %>% 
#   filter(n==1)

  



wpCheck <- wikipedia_complete %>%
  tally()
  filter(wikipedia_page=="Yes") %>% 
  filter(is.na(award_id))

  
  
  

View(data_prep_1)

data_prep_1 <- all_data_merge_honsid %>% 
  select(-c(award_system, clasp_level, clasp_text, gazette_postcode, additional_info, gazette_given_name, gazette_surname)) 

id_check <- data_prep_1 %>%
  filter(is.na(wikipedia_page_id) & wikipedia_page =="Yes")

View(id_check)
# 
##recoding state etc
# View(data_prep_1)

data_prep_2 <- data_prep_1 %>% 
  rename(award_name = award_name.x) %>% 
  rename(state = gazette_state) %>% 
  mutate(
    state = case_when(
      state == "NSW" |state == "Nsw" |state == "BULLI" |state == "ARMIDALE" |
        state == "CASTEL HILL" | state == "CASTLE HILL" | state == "COFFS HARBOUR" 
      | state == "GOULBURN" | state == "WAHROONGA" | state =="NORFOLK ISLAND" ~ "NSW",
      state =="VIC" | state =="Vic" | state =="Victoria"| state =="PRAHRAN" ~ "VIC",
      state =="TAS" |state =="Tas" | state =="KING ISLAND TAS" ~ "TAS",
      state =="WA" ~ "WA",
      state =="SA" ~ "SA",
      state =="QLD" | state =="Qld" | state =="CANUNGRA" |state =="HERSTON, QLD" | state =="CAIRNS"   ~ "QLD",
      state =="NT" ~ "NT",
      state =="ACT" |state =="BRUCE" |state =="KINGSTON" ~ "ACT",
      TRUE ~ "Other"
    )) %>% 
  mutate(
    award_abbr2 = case_when(
      award_name == "Companion of the Order of Australia" ~ "AC",
      award_name == "Member of the Order of Australia" ~ "AM",
      award_name == "Officer of the Order of Australia" | award_name == "Honorary Officer of the Order of Australia" ~ "AO",
      award_name == "Medal of the Order of Australia" ~ "OAM"
    )) %>% 
  mutate(award_abbr = ifelse(is.na(award_abbr), award_abbr2, award_abbr)) %>%  
  
  mutate(
    awardComb = case_when(
      award_abbr == "AD" | award_abbr == "AK" ~ "ADK",
      award_abbr == "AC" ~ "AC",
      award_abbr == "AM" ~ "AM",
      award_abbr == "AO" ~ "AO",
      award_abbr == "OAM" ~ "OAM"
    )) %>% 
  clean_names() %>% 
  mutate(state = factor(state, levels = c("NSW", "VIC", "QLD", "SA", "TAS", "WA", "ACT", "NT", "Other"))) %>% 
  mutate(award_comb = factor(award_comb, levels = c("ADK", "AC", "AO", "AM", "OAM")))   


##filling in missing names from name variable
# View(data_prep_3)
data_prep_3 <- data_prep_2 %>% 
  mutate(name = ifelse(!is.na(name), name, gazette_name)) %>% 
  mutate(wikipedia_page = case_when(wikipedia_page=="Yes" ~ "Yes",
                                    TRUE ~ "No")) 

##sorting dates etc
# View(data_prep_4)

data_prep_4 <- data_prep_3 %>% 
  # rename(wp_creation_date =aus_page_creation) %>% 
  filter(awarded_on !="06/13/2022 00:00:00 +00:00") %>% 
  mutate(awarded_on_full = dmy_hms(awarded_on),
         honours_date = as_date(awarded_on_full)) %>% 
  select(-date_awarded, -awarded_on) %>% 
  mutate(honours_year = year(honours_date),
         wikipedia_creation_year = year(wp_creation_date))

##fixing queens bday 2022 date order

data_prep_4_qb <- data_prep_3 %>% 
  rename(wp_creation_date =aus_page_creation) %>% 
  filter(awarded_on =="06/13/2022 00:00:00 +00:00") %>% 
  mutate(awarded_on_full = mdy_hms(awarded_on),
         honours_date = as_date(awarded_on_full)) %>% 
  select(-date_awarded, -awarded_on) %>% 
  mutate(honours_year = year(honours_date),
         wikipedia_creation_year = year(wp_creation_date))


# View(data_prep_5)

data_prep_5 <- rbind(data_prep_4, data_prep_4_qb)


# View(all_data)
all_data  <- data_prep_5 %>% 
  select(name, gender, wikipedia_page, award_comb, state, wikipedia_url, wikipedia_page_id, award_id, wikipedia_page_id, wp_creation_date, 
         honours_date, wikipedia_creation_year, honours_year, award_name, announcement_event, division, citation, person_description)

write_csv(all_data, "all_data.csv")

# wp_page_check <- all_data %>% 
#   filter(is.na(wikipedia_page_id) & wikipedia_page == "Yes")
# # View(wp_page_check)

##recipient list refers to all recipients - data prep_5 is all honours and therevwill be dulicates
##this holds the min award date as first date / honours is the most recent honourrs 
# View(recipient)

recipient <- all_data %>% 
  arrange(name, honours_date) %>% 
  group_by(name) %>% 
  add_tally() %>% 
  # filter(n>1) %>%
  mutate(first_honours_date = min(honours_date)) %>% 
  mutate(highestAward1 = case_when(award_comb == "OAM" ~1,
                                   award_comb == "AM" ~2,
                                   award_comb == "AO" ~3,
                                   award_comb == "AC" ~4,
                                   award_comb == "ADK" ~5)) %>% 
  mutate(highestAward2 = max(highestAward1)) %>% 
  mutate(highestAward = case_when(highestAward2 == 1 ~"OAM",
                                  highestAward2 == 2 ~"AM" ,
                                  highestAward2 == 3~"AO",
                                  highestAward2 == 4~"AC",
                                  highestAward2 == 5 ~"ADK")) %>% 
  filter(highestAward1 == max(highestAward1)) %>% 
  rename(numberOfHonours = n) %>% 
  mutate(first_honours_year = year(first_honours_date) )%>% 
  select(-c(highestAward1, highestAward2 , highestAward)) %>% 
  mutate(prePostWikipedia = case_when(first_honours_date<"2001-01-15" ~ "Pre",
                                      first_honours_date>="2001-01-15" ~ "Post")) %>% 
  mutate(new_honours_date = replace(first_honours_date, first_honours_date<"2001-01-15", "2001-01-15")) %>% 
  mutate(new_honours_year = year(new_honours_date)) %>% 
  ungroup() %>% 
  clean_names() %>% 
  select(1:5, first_honours_date, first_honours_year, pre_post_wikipedia, 6:23)

write_csv(recipient, "recipient.csv")

## calculates date diff of award and page creation

## sets old dates of honors to the first date wikipedia was operating 
# View(wikipedia)
wikipedia <- recipient %>% 
  filter(wikipedia_page=="Yes") %>% 
  mutate(new_honours_date = replace(first_honours_date, first_honours_date<"2001-01-15", "2001-01-15")) %>% 
  mutate(new_honours_year = year(new_honours_date),
         wp_creation_date = as_date(wp_creation_date),
         wp_creation_year = year(wp_creation_date),
         time_diff = wp_creation_date - new_honours_date,
         year_diff = wp_creation_year - new_honours_year) %>% 
  mutate(wikipedia_week = strftime(wp_creation_date, format = "%Y-W%V"),
         new_honours_week = strftime(new_honours_date, format = "%Y-W%V"),
         week_diff = interval(new_honours_date, wp_creation_date) / dweeks(1),
         week_diff = floor(week_diff)) %>% 
  distinct()

wp_check <- 

write_csv(wikipedia, "wikipedia.csv")

# View(week_zero)

week_zero <- wikipedia %>% 
  filter(week_diff == 0 | week_diff ==1 | week_diff ==-1 ) %>% 
  select(name, wikipedia_page_id, wikipedia_url, award_name)





##citation analysis - 



##women only - page created before and after honors
##women only - with and without Wikipedia page

