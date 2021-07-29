library(tidyverse)
library(rvest)
library(magrittr)

### function to get all annual results for a single athlete in all events
### return NULL if there are no results for that year
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
      Pl. = as.character(Pl.),
      Result = as.character(Result),
      Wind = as.character(Wind)
    ) %>%
    return
  
}

### use function above to get multiple years
get_full_results_multiple_years <- function(ath_id, years = 2016:2021) {
  lapply(years, function(y) get_results_by_year(ath_id, y)) %>%
    bind_rows %>% 
    return
}

### use World Athletics dbs to get all the endpoints
athlete_df <- readRDS('data/athletes.rds')
entry_df <- readRDS('data/entry.rds')

### scrape all results during this olympic cycle
### this should take just under two hours
comp_df <- athlete_df %>% 
  left_join(entry_df) %>% 
  pull(WA_Id) %>% 
  unique %>%
  lapply(., get_full_results_multiple_years) %>%
  bind_rows %>% 
  tibble


saveRDS(comp_df, 'raw-data/raw-competitions.rds')

comp_df <- readRDS('raw-data/raw-competitions.rds')

comp_df %>% 
  group_by(Remark) %>%
  summarise(n = n()) %>% 
  arrange(-n)

# remark OT = oversized indoor track
# MIX = mixed race
# DQ = disqualified
# SC = scratched performance?
# ST = split time
# UNC = uncertified

comp_df %>% 
  mutate(
    isIndoor = ifelse(grepl(' Indoor', Event), 1, 0),
    Event = gsub(' Indoor','', Event),
    Event = gsub(',000 Metres Race Walk', 'xKilometres Race Walk', Event),
    Event = gsub('000 Metres Race Walk', 'xKilometres Race Walk', Event),
    Event = gsub(' Kilometres Race Walk', 'xKilometres Race Walk', Event),
    Event = gsub(' Kilometres', '000 Metres', Event),
    Event = gsub('xKilometres Race Walk', ' Kilometres Race Walk', Event),
    Event = gsub('0000', '0,000', Event),
    Event = ifelse(Event == '12000 Metres', '12,000 Metres', Event),
    Event = ifelse(Event == '15000 Metres', '15,000 Metres', Event),
    Event = ifelse(Event == '25000 Metres', '25,000 Metres', Event),
    measure = case_when(
      Event == 'Long Jump' | Event == 'Triple Jump' ~ 'Distance (Metres)',
      grepl('Jump', Event) | grepl('Vault', Event) ~ 'Height (Metres)',
      grepl('Throw', Event) | grepl('Shot', Event) | grepl('Weight', Event) | grepl('Hour', Event) ~ 'Distance (Metres)',
      grepl('athlon', Event) ~ 'Points',
      T ~ 'Time (Seconds)'
    )
  ) %>% 
  group_by(eventName = Event, measure) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  view

comp_df %>% 
  separate(Result, into = c('hours','mins','secs'), fill = 'left', sep = ':') %>% 
  mutate(
    hours = ifelse(is.na(hours), 0, hours),
    mins = ifelse(is.na(mins), 0, mins),
    secs = gsub('h','', secs),
    notes = case_when(
      secs %in% c('NM','DNF','NH') ~ 'DNF',
      !is.na(as.numeric(secs)) ~ 'None',
      secs %in% c('-','VST') ~ Remark,
      secs == '' ~ 'None',
      T ~ secs
    ),
    notes = ifelse(notes == 'None', NA, notes),
    mark = as.numeric(hours) * 3600 + as.numeric(mins) * 60 + as.numeric(secs),
    Wind = as.numeric(trimws(gsub('\\+','',Wind))),
    Date = as.Date(Date, format = '%d %b %Y')
  ) %>% 
  select(-c(hours, mins, secs)) %>% 
  rename(compDate = Date, compName = Competition, eventName = Event, compCountry = Cnt., compCat = Cat, compHeat = Race, compPlace = Pl., wind = Wind, remark = Remark)

 library(lubridate)
  
comp_df %>% 
  separate(Result, into = c('hours','mins','secs'), fill = 'left', sep = ':', remove = F) %>% 
  mutate(
    hours = ifelse(is.na(hours), 0, hours),
    mins = ifelse(is.na(mins), 0, mins),
    isHandtimed = ifelse(grepl('h', secs), 1, 0),
    secs = gsub('h','', secs),
    mark = as.numeric(hours) * 3600 + as.numeric(mins) * 60 + as.numeric(secs) + 0.24 * isHandtimed,
    Remark = ifelse(Remark == '', NA, Remark),
    Wind = as.numeric(trimws(gsub('\\+','',Wind))),
    Pl. = as.numeric(Pl.),
    Date = as.Date(Date, format = '%d %b %Y'),
    isIndoor = ifelse(grepl(' Indoor', Event), 1, 0),
    legalWind = ifelse(Event %in% c('Long Jump', 'Triple Jump', '100 Meters', '200 Meters', '100 Metres Hurdles, 110 Metres Hurdles') & Wind > 2 & isIndoor == F, 0, 1),
    meetsWAStandards = ifelse(is.na(Remark) & legalWind == 1 & !is.na(mark), 1, 0),
    Event = gsub(' Indoor','', Event),
    Event = gsub(',000 Metres Race Walk', 'xKilometres Race Walk', Event),
    Event = gsub('000 Metres Race Walk', 'xKilometres Race Walk', Event),
    Event = gsub(' Kilometres Race Walk', 'xKilometres Race Walk', Event),
    Event = gsub(' Kilometres', '000 Metres', Event),
    Event = gsub('xKilometres Race Walk', ' Kilometres Race Walk', Event),
    Event = gsub('0000', '0,000', Event),
    Event = ifelse(Event == '12000 Metres', '12,000 Metres', Event),
    Event = ifelse(Event == '15000 Metres', '15,000 Metres', Event),
    Event = ifelse(Event == '25000 Metres', '25,000 Metres', Event),
    measure = case_when(
      Event == 'Long Jump' | Event == 'Triple Jump' ~ 'Distance (Metres)',
      grepl('Jump', Event) | grepl('Vault', Event) ~ 'Height (Metres)',
      grepl('Throw', Event) | grepl('Shot', Event) | grepl('Weight', Event) | grepl('Hour', Event) ~ 'Distance (Metres)',
      grepl('athlon', Event) ~ 'Points',
      T ~ 'Time (Seconds)'
    ),
    Cat = case_when(
      Cat == 'OW' ~ 'Olympics/World Championships',
      Cat == 'DF' ~ 'Diamond League Final',
      Cat == 'GW' ~ 'Non-Outdoor T&F World Championships',
      Cat == 'GL' ~ 'Area Outdoor Championships',
      Cat == '' | is.na(Cat) ~ 'Unknown',
      T ~ paste0('Category ', Cat)
    ),
    raceStage = gsub('[0-9]+', '', Race),
    raceOrder = as.numeric(gsub('[A-Z]+', '', Race)),
    raceStage = case_when(
      raceStage == 'F' ~ 'Final',
      raceStage == 'SF' | raceStage == 'SR' ~ 'Semi',
      raceStage == 'QF' ~ 'Quarter',
      raceStage == 'Q' ~ 'Qualifier',
      raceStage == 'H' ~ 'Heat',
      raceStage == 'PR' ~ 'Prelim',
      raceStage == 'CE' ~ 'Combined Event'
    )
  ) %>% 
  rename(compDate = Date, compName = Competition, eventName = Event, compCountry = Cnt., compCat = Cat, compHeat = Race, compPlace = Pl., wind = Wind, remark = Remark, markString = Result) %>% 
  select(-c(hours, mins, secs)) %>% 
  filter(compDate > '2016-08-21') %>% 
  left_join(athlete_df) %>% 
  select(
    compDate, compName, WA_Id, fullName, firstName, lastName, sexCode, countryCode, eventName, markString, mark, measure, wind,
    legalWind, remark, meetsWAStandards, isHandtimed, isIndoor, compCat, compPlace, compCountry, raceStage, raceOrder 
  ) %>% 
  saveRDS('olympian-performances.rds')