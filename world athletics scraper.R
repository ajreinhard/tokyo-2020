setwd('C:/Users/rei1740/Desktop/Anthony/tokyo')
source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(RJSONIO)
library(tidyverse)
library(rvest)
library(magrittr)

ath_json <- fromJSON('athletes.json')
basic_athlete_fields <- names(ath_json$data$searchAthletes[[1]])[c(1,5:6,8,10:12,16:17)]

athlete_df <- lapply(ath_json$data$searchAthletes, function(x) x[basic_athlete_fields]) %>%
  bind_rows %>% 
  rename(competitorId = id, fullName = name, WA_Id = competitorId_WA) %>% 
  mutate(
    competitorId = as.numeric(competitorId),
    birthDate = as.Date(birthDate)
  )

entry_df <- lapply(ath_json$data$searchAthletes, function(x) {
  lapply(x$competitionEntries, function(y) {
      bind_cols(y['competitorId'], eventName = y$discipline$name)
    })
  }) %>%
  bind_rows

event_df <- lapply(ath_json$data$searchAthletes, function(x) {
  lapply(x$competitionEntries, function(y) {
      y$discipline[-c(1:3,20:22)]
    })
  }) %>%
  bind_rows %>%
  distinct %>% 
  rename(eventName = name, eventOrder = order)

get_results_by_year <- function(ath_id, year = 2021) {
  
  result_page <- 'https://www.worldathletics.org/data/GetCompetitorResultsByYearHtml?' %>% 
    paste0(., 'resultsByYear=', year, '&resultsByYearOrderBy=date&aaId=', ath_id) %>% 
    read_html
  
  results_header <- result_page %>% 
    html_nodes(xpath = '//th/@data-th-value') %>% 
    html_text
      
  if (result_page %>% html_table %>% is_empty) {return(NULL)}
  
  results_table <- result_page %>% 
    html_table %>% 
    extract2(1)
  
  names(results_table) <- results_header
  if (sum(grepl('Wind', results_header)) == 0) results_table <- results_table %>% mutate(Wind = NA)
  
  results_table %>% 
    mutate(
      WA_Id = ath_id,
      year = year,
      Race = as.character(Race),
      Race = ifelse(Race == 'FALSE', 'F', Race),
      Cat = as.character(Cat),
      Cat = ifelse(Cat == 'FALSE', 'F', Cat),
      Pl. = as.numeric(Pl.),
      Result = as.character(Result),
      Wind = as.character(Wind)
    ) %>%
    return
  
}

get_results_by_year(14916212, 2017)

athlete_df %>% 
  filter(lastName == 'FELIX') %>% 
  view

comp_df <- athlete_df %>% 
  left_join(entry_df) %>% 
  filter(eventName == '100 Metres') %>% 
  pull(WA_Id) %>% 
  lapply(., get_results_by_year) %>%
  bind_rows

comp_df %>%
  separate(Result, into = c('hours','mins','secs'), fill = 'left', sep = ':') %>% 
  mutate(
    hours = ifelse(is.na(hours), 0, hours),
    mins = ifelse(is.na(mins), 0, mins),
    Result = as.numeric(hours) * 3600 + as.numeric(mins) * 60 + as.numeric(secs)
  ) %>% 
  filter(Event == '100 Metres') %>% 
  group_by(WA_Id) %>% 
  mutate(SB_Result = min(Result, na.rm = T)) %>% 
  filter(SB_Result <= 11.1) %>% 
  ungroup %>% 
  arrange(-SB_Result) %>% 
  mutate(WA_Id = factor(WA_Id, unique(WA_Id))) %>% 
  #filter(Race == 'F') %>% 
  ggplot(aes(y = WA_Id, x = Result)) +
  geom_jitter(width = 0, height = 0.15) +
  scale_x_reverse(limit = c(11.5, NA))
  

event_df %>% 
  select(eventName) %>% 
  print(n = 100)
