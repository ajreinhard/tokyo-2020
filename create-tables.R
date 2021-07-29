library(jsonlite)
library(tidyverse)
library(magrittr)

### take JSON file from World Athletics and create tables for Olympic Athletes
### save all of the tables
ath_json <- jsonlite::fromJSON('raw-data/athletes.json')

ath_json$data$searchAthletes[c(1,5:6,8,10:12,16:17)] %>% 
  rename(competitorId = id, fullName = name, WA_Id = competitorId_WA) %>% 
  mutate(
    competitorId = as.numeric(competitorId),
    birthDate = as.Date(birthDate)
  ) %>% 
  as_tibble %>% 
  saveRDS('data/athletes.rds')
  
competitors <- ath_json$data$searchAthletes$competitionEntries %>% bind_rows %>% select(competitorId, sexCode)
eventName <- ath_json$data$searchAthletes$competitionEntries %>% bind_rows %>% pull(discipline) %>% pull(name)
entry_df <- tibble(competitors, eventName)
saveRDS(entry_df, 'data/entry.rds')

ath_json$data$searchAthletes$competitionEntries %>%
  bind_rows %>%
  pull(discipline) %>%
  .[-c(1:3,20:22)] %>% 
  distinct %>% 
  rename(eventName = name, eventOrder = order) %>% 
  inner_join(entry_df %>% select(sexCode, eventName) %>% distinct) %>% 
  saveRDS('data/events.rds')