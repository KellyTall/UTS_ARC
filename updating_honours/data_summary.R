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


##multiple awards - people versus orders



#gender split - plus TF anonymous

all_data <- read_csv( "all_data.csv") %>% 
  clean_names()
