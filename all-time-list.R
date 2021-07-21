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
  sapply(., function(x) c(gsub('.png','',x), get_colors(paste0('flags/',x)) %>% make_palette(n = 2, show = F))) %>% 
  t %>%
  as_tibble %>% 
  rename(countryCode = 1, color1 = 2, color2 = 3)

cnt_pri <- flag_color_df$color1
cnt_sec <- flag_color_df$color2
names(cnt_pri) <- flag_color_df$countryCode
names(cnt_sec) <- flag_color_df$countryCode

comp_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/competitions.rds'))
entry_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/entry.rds'))
events_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/events.rds'))

entry_df %>% 
  left_join(athlete_df) %>% 
  left_join(events_df) %>% 
  group_by(eventName, sexCode, countryCode) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!grepl('Relay', eventName)) %>% 
  view
events_df %>% view
p <- entry_df %>% 
  left_join(athlete_df) %>% 
  left_join(events_df) %>% 
  filter(eventName == '110 Metres Hurdles' & sexCode == 'M') %>% 
  left_join(comp_df) %>% 
  filter(wind <= 2) %>% 
  group_by(WA_Id) %>% 
  arrange(mark) %>% 
  mutate(
    PB_mark = min(mark, na.rm = T),
    mark_rank = row_number()
  ) %>% 
  ungroup %>% 
  arrange(PB_mark) %>% 
  mutate(
    WA_Id = factor(WA_Id, unique(WA_Id)),
    PB_rank = as.numeric(WA_Id),
    WA_Id = factor(WA_Id, rev(unique(WA_Id))),
  ) %>% 
  filter(mark_rank <= 10 & PB_rank <= 15)  %>%
  filter((((median(mark) - min(mark)) * 2) + median(mark)) > mark) %>% 
  ggplot(aes(y = PB_rank, x = mark)) +
  geom_jitter(aes(fill = countryCode, color = countryCode),width = 0, height = 0.15, shape = 21, show.legend = F) +
  geom_text(aes(x = PB_mark, label = lastName), hjust = 0, size = 2) +
  geom_text(aes(y = PB_rank + 0.2, x = PB_mark, label = countryName), hjust = 0, size = 1.3) +
  scale_x_reverse(expand = expansion(mult = c(0.03,0.2)), breaks = pretty_breaks(n = 10)) +
  scale_y_reverse() +
  labs(title = 'Tokyo 2020 - Men\'s 200 Metres',
       subtitle = 'Top 10 Performances from Olympic Cycle',
       x = 'Time (Seconds)',
       y = NULL
  ) +
  scale_color_manual(values = cnt_pri) +
  scale_fill_manual(values = cnt_sec) +
  theme_SB +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )

brand_plot(p, 'm110 test.png', data_home = 'Data: World Athletics', fade_borders = 'ltr', asp = 3/4)


library(rvest)

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
         



