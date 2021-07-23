setwd('C:/Users/rei1740/Desktop/Anthony/tokyo')
library(colorfindr)
library(tidyverse)
library(magick)

athlete_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/athletes.rds'))

country_list <- athlete_df %>%
  pull(countryCode) %>%
  unique

for (i in country_list) {
  i %>% 
    paste0('https://media.aws.iaaf.org/Flags/',.,'.gif') %>% 
    image_read %>% 
    image_convert(format = 'png') %>% 
    image_write(paste0('flags/',i,'.png'))
}

flag_color_df <- dir('flags') %>% 
  sapply(., function(x) c(gsub('.png','',x), get_colors(paste0('flags/',x), exclude_col = '#FFFFFF') %>% make_palette(n = 2, show = F))) %>% 
  t %>%
  as_tibble %>% 
  rename(countryCode = 1, color1 = 2, color2 = 3)


athlete_df %>%
  group_by(countryCode) %>%
  summarise(n = n()) %>% 
  left_join(flag_color_df) %>% 
  arrange(-n) %>% 
  write.csv('countrycolors2.csv', row.names = F)


cnt_pri <- flag_color_df$color1
cnt_sec <- flag_color_df$color2
names(cnt_pri) <- flag_color_df$countryCode
names(cnt_sec) <- flag_color_df$countryCode


get_colors('flags/BEL.png', exclude_col = '#000000') %>% make_palette(n = 3, show = T)




library(lubridate)

top_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/top-m-100m.rds')) %>% 
  mutate(compYrMo = year(compDate) * 100 + month(compDate))

all_compYrMo <- expand.grid(year = 1960:2021, month = 1:12) %>% 
  mutate(compYrMo = year * 100 + month) %>% 
  arrange(compYrMo) %>% 
  pull(compYrMo)
  
monthly_top20 <- lapply(all_compYrMo, function(x) {
  top_df %>% 
    filter(compYrMo <= x) %>% 
    arrange(mark, wind, compDate) %>% 
    mutate(
      rank_at_time = row_number(),
      evalYrMo = x
    ) %>% 
    slice(1:20) %>% 
    return
}) %>% bind_rows
  
inc_cnt <- monthly_top20 %>% pull(countryCode) %>% unique


inc_cnt[!(inc_cnt %in% flag_color_df$countryCode)]


top_df %>% 
  filter(compYrMo < 201612) %>% 
  view

monthly_top20 %>% 
  filter(!countryCode %in% c('USA','JAM')) %>% 
  arrange(desc(evalYrMo))

monthly_top20 %>% 
  group_by(evalYrMo) %>% 
  summarise(
    uni_ath = n_distinct(fullName),
    uni_cnt = n_distinct(countryCode)
  ) %>% 
  filter(evalYrMo >= 196806) %>% 
  view

monthly_top20 %>% 
  group_by(evalYrMo, fullName) %>% 
  summarise(
    uni_ath = n()
  ) %>% 
  filter(evalYrMo >= 196806) %>% 
  view




comp_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/competitions.rds'))

comp_df %>% pull(remark) %>% table %>% sort

comp_df %>% 
  left_join(athlete_df) %>% 
  filter(remark == 'ST') %>% 
  arrange(desc(compDate)) %>% 
  select(compDate, WA_Id, fullName, compName, eventName, mark, remark) %>% 
  view
