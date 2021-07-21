library(RJSONIO)
library(tidyverse)
library(magrittr)

### take JSON file from World Athletics and create tables for Olympic Athletes

### save all of the tables for the men
m_ath_json <- fromJSON('raw-data/athletes-men.json', nullValue = NA)
basic_athlete_fields <- names(m_ath_json$data$searchAthletes[[1]])[c(1,5:6,8,10:12,16:17)]

m_athlete_df <- lapply(m_ath_json$data$searchAthletes, function(x) x[basic_athlete_fields]) %>%
  bind_rows %>% 
  rename(competitorId = id, fullName = name, WA_Id = competitorId_WA) %>% 
  mutate(
    competitorId = as.numeric(competitorId),
    birthDate = as.Date(birthDate)
  )

m_entry_df <- lapply(m_ath_json$data$searchAthletes, function(x) {
  lapply(x$competitionEntries, function(y) {
    bind_cols(y['competitorId'], eventName = y$discipline$name)
  })
}) %>%
  bind_rows %>% 
  mutate(sexCode = 'M')

m_event_df <- lapply(m_ath_json$data$searchAthletes, function(x) {
  lapply(x$competitionEntries, function(y) {
    y$discipline[-c(1:3,20:22)]
  })
}) %>%
  bind_rows %>%
  distinct %>% 
  rename(eventName = name, eventOrder = order) %>% 
  mutate(sexCode = 'M')

### save all of the tables for the women
w_ath_json <- fromJSON('raw-data/athletes-women.json', nullValue = NA)
basic_athlete_fields <- names(w_ath_json$data$searchAthletes[[1]])[c(1,5:6,8,10:12,16:17)]

w_athlete_df <- lapply(w_ath_json$data$searchAthletes, function(x) x[basic_athlete_fields]) %>%
  bind_rows %>% 
  rename(competitorId = id, fullName = name, WA_Id = competitorId_WA) %>% 
  mutate(
    competitorId = as.numeric(competitorId),
    birthDate = as.Date(birthDate)
  )

w_entry_df <- lapply(w_ath_json$data$searchAthletes, function(x) {
  lapply(x$competitionEntries, function(y) {
    bind_cols(y['competitorId'], eventName = y$discipline$name)
  })
}) %>%
  bind_rows %>% 
  mutate(sexCode = 'W')

w_event_df <- lapply(w_ath_json$data$searchAthletes, function(x) {
  lapply(x$competitionEntries, function(y) {
    y$discipline[-c(1:3,20:22)]
  })
}) %>%
  bind_rows %>%
  distinct %>% 
  rename(eventName = name, eventOrder = order) %>% 
  mutate(sexCode = 'W')


### combine men & women and save data
bind_rows(w_athlete_df, m_athlete_df) %>% saveRDS('data/athletes.rds')
bind_rows(w_entry_df, m_entry_df) %>% saveRDS('data/entry.rds')
bind_rows(w_event_df, m_event_df) %>% saveRDS('data/events.rds')
