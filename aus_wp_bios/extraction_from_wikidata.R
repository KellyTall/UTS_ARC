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


##this query picks up anyone who has ever had Aus citizenship inc R Murdoch ;-)

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

##governors of NSW

wp_bio_gov_NSW <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1528895.
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)

View(wp_bio_NSW_gg)

##governors of VIC

wp_bio_gov_VIC <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1571023
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)

##governors of QLD

wp_bio_gov_QLD <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1467097
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)

##governors of SA

wp_bio_gov_SA <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P39 ?statement0.
        ?statement0 (ps:P39) wd:Q1840570
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)

##people w People of Australia ID / National Centre of Biography - assuming there will be overalp with Women of Aus list below
people_of_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P9159 ?statement0.
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)

View(people_of_aus)

##people w Australian Dictionary of Biography ID
aus_dictionary <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P1907 ?statement0.
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)

View(aus_dictionary)

##women Australia ID - (national centre of biography / ANU school of history)
women_aus <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P9244 ?statement0.
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)


##australian war memorial
aus_wm_people <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P6713 ?statement0.
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)


#Australian National Maritime Museum person
maritime_people <- query_wikidata(
  "SELECT DISTINCT  ?person ?personLabel ?personDescription ?genderLabel ?sitelink 
  
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
    
    {
      SELECT DISTINCT ?person WHERE {
        ?person p:P7769 ?statement0.
      }
    }
      
      OPTIONAL{?person wdt:P21 ?gender.}       
   
    OPTIONAL { ?sitelink schema:about ?person ;
              schema:inLanguage 'en' ;
              schema:isPartOf [ wikibase:wikiGroup 'wikipedia' ] }.
  }"
)


