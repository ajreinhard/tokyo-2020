source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
setwd('C:/Users/rei1740/Desktop/Anthony/tokyo')
library(rvest)
library(jsonlite)

event_abbr_df <- read_csv('events_abbr.csv')
athlete_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/athletes.rds'))
entry_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/entry.rds'))
event_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/events.rds'))

event_alt_rm_df <- read_csv('alt_finder.csv') %>% 
  pivot_longer(c(`1`,`2`,`3`,`4`), values_to = 'fullName', names_to = 'nameOrder') %>% 
  filter(is.na(fullName)) %>% 
  mutate(
    nameOrder = as.numeric(nameOrder),
    fullName = NULL
  )
  
full_event_df <- athlete_df %>% 
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  filter(!grepl('Relay',eventName)) %>%
  group_by(eventName, countryCode, sexCode) %>% 
  filter(n() > 3) %>% 
  mutate(nameOrder = row_number()) %>% 
  ungroup

event_alt_df <- full_event_df %>% 
  inner_join(event_alt_rm_df) %>% 
  select(WA_Id, eventName) %>% 
  mutate(isAlternate = 1)

get_world_list <- function(event, sexCode, pg) {
  
  if (sexCode == 'M')  {sex = 'men'} else {sex = 'women'}
  
  raw_html <- paste0('https://www.worldathletics.org/world-rankings/',event,'/',sex,'?regionType=world&page=',pg,'&rankDate=2021-07-20&limitByCountry=0') %>% 
    read_html
  
  world_list_df <- raw_html %>% 
    html_table %>% 
    extract2(1) %>% 
    data.frame(stringsAsFactors = F) %>% 
    mutate(across(.cols = everything(), as.character))
  
  WA_Id <- raw_html %>%
    html_nodes(xpath = '//tr/@data-athlete-url') %>% 
    html_text() %>% 
    substr(., nchar(.)-8, nchar(.)) %>% 
    as.numeric
  
  world_list_df %>% 
    rename(eventRank = Place, fullName = Competitor, countryCode = Nat, eventScore = Score) %>% 
    mutate(
      eventAbbr = event,
      WA_Id = WA_Id,
      eventScore = as.numeric(eventScore),
      eventRank = as.numeric(eventRank),
      sexCode = sexCode,
      DOB = NULL,
      Event.List = NULL
    ) %>% 
    return
  
}

#curr_ranks <- lapply(1:5, function(x) get_world_list('20km-race-walking','W',x)) %>% bind_rows %>% as_tibble

ind_event_df <- event_df %>% 
  inner_join(event_abbr_df) %>% 
  select(sexCode, eventAbbr, eventName) %>% 
  mutate(eventAbbr = ifelse(sexCode == 'W' & eventAbbr == '20km-race-walking', 'race-walking', eventAbbr))


world_top500_df <- lapply(1:nrow(ind_event_df), function(evt) {
  lapply(1:5, function(x) get_world_list(ind_event_df$eventAbbr[evt], ind_event_df$sexCode[evt], x)) %>% bind_rows
}) %>% bind_rows %>% as_tibble

col_vec = c('Team USA' = '#E33144', 'Team USA Alt' = '#E33144', 'Competitors' = 'grey30')
fill_vec = c('Team USA' = '#212176', 'Team USA Alt' = 'white', 'Competitors' = 'lightblue')
size_vec = c('Team USA' = 1.5, 'Team USA Alt' = 1.5, 'Competitors' = 0.8)

viz_df <- athlete_df %>%
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  left_join(ind_event_df) %>% 
  left_join(event_alt_df) %>% 
  inner_join(world_top500_df, by = c('WA_Id', 'eventAbbr'), suffix = c('', '_ranking')) %>% 
  filter(sexCode == 'M' & eventScore >= 1100) %>% 
  group_by(eventName) %>% 
  mutate(
    min_evt_score = min(eventScore),
    max_evt_score = max(eventScore)
  ) %>% 
  ungroup %>% 
  arrange(-eventOrder) %>% 
  mutate(
    eventName = factor(eventName, unique(eventName)),
    ath_type = case_when(
      countryCode == 'USA' & is.na(isAlternate) ~ 'Team USA',
      countryCode == 'USA' & isAlternate == 1 ~ 'Team USA Alt',
      T ~ 'Competitors'
    ),
    ath_type = factor(ath_type, c('Team USA','Team USA Alt','Competitors'))
  ) %>% 
  arrange(desc(ath_type))
  
line_sep_df <- viz_df %>% 
  group_by(eventName) %>% 
  summarise(
    min_evt_score = min(eventScore),
    max_evt_score = max(eventScore)
  ) %>% 
  mutate(
    min_evt_score = round((min_evt_score + lag(min_evt_score)) / 2,0),
    max_evt_score = round((max_evt_score + lag(max_evt_score)) / 2,0)
  ) %>% 
  filter(!is.na(max_evt_score)) %>% 
  group_by(eventName) %>% 
  summarise(
    eventScore = min(min_evt_score):max(max_evt_score),
    to_max = max(eventScore) - eventScore,
    to_min = eventScore - min(eventScore),
    fade = ifelse(to_max < to_min, to_max, to_min),
    alpha_num = ifelse(fade >= 100, 100, fade) / 100,
    .groups = 'drop'
  ) %>% 
  filter(eventScore %% 5 == 0)

p <- viz_df %>% 
  ggplot(aes(x = eventScore, y = eventName)) + 
  geom_jitter(aes(color = ath_type, fill = ath_type, size = ath_type, stroke = ifelse(ath_type == 'Competitors', 0.2, 0.3)), height = 0.1, shape = 21, alpha = 0.7) +
  geom_line(data = line_sep_df, aes(group = eventName, alpha = alpha_num, y = as.numeric(eventName) - 0.5), color = 'grey85', size = 0.3) +
  geom_shadowtext(aes(x = ifelse(min_evt_score == eventScore, 1110, NA), y = as.numeric(eventName) + 0.35, label = eventName), family = font_SB, size = 1.6, color = 'darkblue', bg.color = 'white', bg.r = 0.2, hjust = 0) +
  theme_SB +
  labs(title = 'Tokyo 2020 - Team USA Men',
       subtitle = '2021 World Athletics Event Scores',
       x = 'Event Score',
       y = NULL
  ) +
  scale_color_manual(values = col_vec) +
  scale_fill_manual(values = fill_vec) +
  scale_size_manual(values = size_vec) +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = c(0.93,1.005),
    legend.justification = c(1,0),
    legend.direction = 'vertical',
    legend.key.height = unit(0.6, 'lines'),
    legend.key.width = unit(0.3, 'lines'),
    legend.margin = margin(t = -0.2, b = 0.2, r = 0.3, l = 0.6, unit='lines'),
    legend.text = element_text(size = 7, margin = margin(b = -2.5, t = -3, unit = 'pt')),
    legend.title = element_blank()
  )

brand_plot(p, save_name = 'team USA M.png', asp = 3/4, data_home = 'Data: World Athletics', fade_borders = 'trl')


athlete_df %>%
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  filter(sexCode == 'W' & eventName == '100 Metres' & countryCode == 'JAM') %>% 
  pull(WA_Id)
  anti_join(curr_ranks, by = c('fullName' = 'Competitor', 'countryCode' = 'Nat')) %>% 
  view

  


###### get exact records for rankings calc
  
jsonlite::fromJSON('https://www.worldathletics.org/WorldRanking/RankingScoreCalculation?competitorId=510000') %>% 
  gsub('\\"','\"',.) %>% 
  jsonlite::fromJSON(flatten = T)



####### all time records for an event


get_records <- function(pg) {
  'https://www.worldathletics.org/records/all-time-toplists/sprints/100-metres/outdoor/men/senior?regionType=world&timing=all&windReading=regular&bestResultsOnly=false&firstDay=1899-12-30&lastDay=2020-07-20&page=' %>% 
    paste0(., pg) %>% 
    read_html %>% 
    html_table %>% 
    extract2(1) %>% 
    data.frame(stringsAsFactors = F) %>% 
    mutate(across(.cols = everything(), as.character)) %>% 
    return
}

st_time <- Sys.time()
all_time_df <- lapply(1:215, get_records) %>% bind_rows %>% as_tibble
Sys.time() - st_time

all_time_df %>%
  rename_all(tolower) %>% 
  rename(countryCode = nat, fullName = competitor, compDate = date, compVenue = venue, score = results.score) %>% 
  mutate(
    var.8 = NULL,
    handheld = ifelse(grepl('h', mark), 1, 0),
    mark = as.numeric(gsub('h','', mark)) + handheld * 0.24,
    score = as.numeric(score),
    wind = as.numeric(wind),
    dob = as.Date(dob, format = '%d %b %Y'),
    compDate = as.Date(compDate, format = '%d %b %Y'),
    rank = as.numeric(rank)
  ) %>% 
  saveRDS('top-m-100m.rds')


### check for other notes
athlete_df %>%
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  left_join(ind_event_df) %>% 
  left_join(event_alt_df) %>% 
  right_join(world_top500_df, by = c('WA_Id', 'eventAbbr'), suffix = c('', '_ranking')) %>% 
  filter(is.na(competitorId)) %>% 
  arrange(eventRank) %>% 
  slice(1:100) %>% 
  view


athlete_df %>%
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  left_join(ind_event_df) %>% 
  left_join(event_alt_df) %>% 
  anti_join(world_top500_df, by = c('WA_Id', 'eventAbbr'), suffix = c('', '_ranking')) %>% 
  filter(countryCode == 'USA' & typeName != 'Relays')


olympic_df <- athlete_df %>%
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  left_join(ind_event_df)

world_top500_df %>% 
  left_join(olympic_df, by = c('WA_Id', 'eventAbbr'), suffix = c('_ranking', '')) %>% 
  left_join(ind_event_df, by = c('sexCode_ranking' = 'sexCode', 'eventAbbr'), suffix = c('_delete', '')) %>% 
  mutate(tokyoQualifier = !is.na(competitorId)) %>% 
  select(eventRank, sexCode = sexCode_ranking, eventName, WA_Id, fullName = fullName_ranking, countryCode = countryCode_ranking, eventScore, tokyoQualifier) %>% 
  arrange(eventName, sexCode, eventRank) %>% 
  saveRDS('world-rankings-2021.rds')


world_df <- readRDS('world-rankings-2021.rds')

p <- world_df %>% 
  left_join(event_df) %>% 
  arrange(-eventOrder) %>% 
  filter(sexCode == 'M' & eventScore >= 1100) %>% 
  mutate(
    eventName = factor(eventName, unique(eventName)),
    tokyoQualifier = ifelse(tokyoQualifier, 'Olympian', 'Failed to Qualify'),
    tokyoQualifier = factor(tokyoQualifier, rev(unique(tokyoQualifier)))
  ) %>% 
  arrange(tokyoQualifier) %>% 
  ggplot(aes(x = eventScore, y = eventName)) + 
  geom_jitter(aes(color = tokyoQualifier, fill = tokyoQualifier), height = 0.1, shape = 21, alpha = 0.7) +
  #geom_line(data = line_sep_df, aes(group = eventName, alpha = alpha_num, y = as.numeric(eventName) - 0.5), color = 'grey85', size = 0.3) +
  geom_shadowtext(aes(x = 1110, y = as.numeric(eventName) + 0.35, label = eventName), family = font_SB, size = 1.6, color = 'darkblue', bg.color = 'white', bg.r = 0.2, hjust = 0) +
  theme_SB +
  labs(title = 'Tokyo 2020 - Top Athletes Missing',
       subtitle = '2021 World Athletics Event Scores',
       x = 'Event Score',
       y = NULL
  ) +
  #scale_color_manual(values = col_vec) +
  #scale_fill_manual(values = fill_vec) +
  #scale_size_manual(values = size_vec) +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = c(0.93,1.005),
    legend.justification = c(1,0),
    legend.direction = 'vertical',
    legend.key.height = unit(0.6, 'lines'),
    legend.key.width = unit(0.3, 'lines'),
    legend.margin = margin(t = -0.2, b = 0.2, r = 0.3, l = 0.6, unit='lines'),
    legend.text = element_text(size = 7, margin = margin(b = -2.5, t = -3, unit = 'pt')),
    legend.title = element_blank()
  )

brand_plot(p, save_name = 'missed team M.png', asp = 3/4, data_home = 'Data: World Athletics', fade_borders = 'trl')



#### look at relay pool

athlete_df %>% 
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  left_join(ind_event_df) %>% 
  left_join(event_alt_df) %>% 
  filter(eventName == '4x100 Metres Relay' & sexCode == 'M') %>% 
  left_join(world_top500_df %>% filter(eventAbbr == '100m') %>%  select(WA_Id, eventScore)) %>% 
  group_by(countryName) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = eventScore, y = countryName)) + 
  geom_jitter(height = 0.1, shape = 21, alpha = 0.7)




athlete_df %>% 
  left_join(entry_df) %>% 
  left_join(event_df) %>% 
  left_join(ind_event_df) %>% 
  left_join(event_alt_df) %>% 
  pull(eventName) %>% table
  filter(eventName == '4x400 Metres Relay') %>% 
  pull(countryCode) %>% table
  left_join(world_top500_df %>% filter(eventAbbr == '400m') %>%  select(WA_Id, eventScore))


  



