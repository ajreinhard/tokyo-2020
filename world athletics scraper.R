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


# look for places that might have been NA'd
comp_df %>% filter(is.na(Pl.) & !is.na(as.numeric(Result)))
comp_df$Race %>% table
comp_df %>% filter(is.na(as.numeric(Result)))
comp_df %>% pull(Remark) %>% table

### work on cleaning up the big competition df (better col names/cleaner fields)
clean_comp_df <- comp_df %>%
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
  
saveRDS(clean_comp_df, 'data/competitions.rds')


clean_comp_df %>% 
  group_by(eventName) %>% 
  summarise(n = n()) %>% view




### jitter plot for event
clean_comp_df %>% 
  filter(Event == 'Pole Vault') %>% 
  group_by(WA_Id) %>% 
  mutate(SB_Result = max(Result, na.rm = T)) %>% 
  #filter(SB_Result > 6.5) %>% 
  ungroup %>% 
  arrange(SB_Result) %>% 
  mutate(WA_Id = factor(WA_Id, unique(WA_Id))) %>% 
  #filter(Race == 'F') %>% 
  ggplot(aes(y = WA_Id, x = Result)) +
  geom_jitter(width = 0, height = 0.15) +
  geom_text(aes(x = SB_Result * 1.01, label = paste0(lastName, ' (', countryCode, ')'), hjust = 0))
  #scale_x_reverse(limit = c(11.5, NA))
  






### starting work on h2h matrix
athlete_comp <- comp_df %>% 
  filter(year >= 2020) %>% 
  left_join(athlete_df) %>% 
  select(Date, Competition, Cat, Race, Event, WA_Id, Pl.)

athlete_comp %>% 
  left_join(athlete_comp, by = c('Date', 'Competition', 'Cat', 'Race', 'Event'), suffix = c('', '_opp')) %>% 
  filter(WA_Id != WA_Id_opp) %>% 
  group_by(WA_Id, WA_Id_opp) %>% 
  summarise(
    matchups = n(),
    wins = sum(ifelse(Pl. < Pl._opp, 1, 0), na.rm = T),
    win_pct = wins / matchups
  ) %>% 
  ungroup %>% 
  filter(WA_Id == 14679502) %>% 
  view
  
### career graph
athlete_df %>% filter(firstName == 'Shelly-Ann') %>% left_join(entry_df) %>% select(fullName, WA_Id, eventName)
career_df <- get_full_results_multiple_years(14285680, 1998:2021)

career_df %>% 
  left_join(athlete_df) %>% 
  #filter(is.na(as.numeric(Result)))
  filter(Event == '100 Metres') %>% 
  mutate(
    Date = as.Date(Date, format = '%d %b %Y'),
    age = (Date - birthDate) / 365.25,
    Result = as.numeric(Result),
    weight = ifelse(Race == 'F', 1, 0.5)
  ) %>% 
  ggplot(aes(x = age, y = Result, weight = weight)) +
  geom_point() +
  geom_smooth()


### csv with athlete name & multiple results only for just olympic event
### indoor indicator
### figure out cat & race meaning
### investigate place
### order events by what is being measured and more being better
### find a way to import schedules
### use flags and get country colors
### create full career scraper
### what does wind mean/what is legal wind
### finish h2h and career scraper
### find out who is on mixed relay team
### shelly-ann had h next to one time. Also some blank results and places