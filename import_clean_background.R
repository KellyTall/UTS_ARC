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




wp_page_create_query1d <- lapply(wp_page_create_search1d$search_url, function(i){
  
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

wp_page_create_query2a <- lapply(wp_page_create_search2a$search_url, function(i){
  
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


wp_page_create_query2b <- lapply(wp_page_create_search2b$search_url, function(i){
  
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