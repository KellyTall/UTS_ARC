##creating file of all Australian bios on wikipedia

##notes: need to define what we mean by Australian? Citizen / born in Aus ? lived in Aus etc? 
##Rupert Murdoch does not appear in first query (Aus / American), but Lachlan does even w multiple citizenships

library(tidyverse)
library(janitor)
library(WikidataQueryServiceR)

# aus_citizens_wd2 <- query_wikidata(
#   "SELECT ?person ?personLabel ?personDescription ?sitelink
# WHERE
# {
#   ?person wdt:P31 wd:Q5 .
#   ?person wdt:P27 wd:Q408.
#   OPTIONAL { ?sitelink schema:about ?person ;
#               schema:inLanguage 'en' ;
#               schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
#   SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'}.
# }")
# 
# View(aus_citizens_wd)


aus_citizens_wd <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?sitelink  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P27 ?statement0.
        ?statement0 (ps:P27) wd:Q408.
      }
    }
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }")


View(aus_citizens_wd)


wp_bio_aus_citizens <- aus_citizens_wd %>% 
  clean_names() %>% 
  filter(!is.na(sitelink)) %>% 
  distinct()
  

View(wp_bio_aus_citizens)
