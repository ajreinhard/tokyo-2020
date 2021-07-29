setwd('C:/Users/rei1740/Desktop/Anthony/tokyo')
library(colorfindr)
library(tidyverse)
library(magick)

athlete_df <- readRDS(url('https://raw.githubusercontent.com/ajreinhard/tokyo-2020/main/data/athletes.rds'))

country_list <- c('BAR','VEN','BEL','BRA','CUB','NAM','NGR','POR')

for (i in country_list) {
  i %>% 
    paste0('https://media.aws.iaaf.org/Flags/',.,'.gif') %>% 
    image_read %>% 
    image_convert(format = 'png') %>% 
    image_write(paste0(i,'.png'))
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


get_colors('POR.png', exclude_col = '#000000') %>% make_palette(n = 4, show = T)



source('https://github.com/ajreinhard/data-viz/raw/master/ggplot/plot_SB.R')
library(ggstance)
library(ggtext)
library(lubridate)

country_name_df <- readRDS('data/athletes.rds') %>% 
  select(countryCode, countryName) %>% 
  distinct

color_df <- read_csv('raw-data/country_colors.csv')
champs_df <- read.csv('raw-data/championships.csv', stringsAsFactors = F) %>% 
  mutate(
    compStart = as.Date(compStart),
    compEnd = as.Date(compEnd)
  ) %>% 
  group_by(compVenue, compType, compEdition) %>% 
  summarise(compDate = as.Date(compStart:compEnd, origin = '1970-01-01')) %>% 
  ungroup

west_ger_col <- color_df %>% filter(countryCode == 'GER') %>% mutate(countryCode = 'FRG')
east_ger_col <- color_df %>% filter(countryCode == 'GER') %>% mutate(countryCode = 'GDR')
ussr_col <- color_df %>% filter(countryCode == 'CHN') %>% mutate(countryCode = 'URS')

color_df <- bind_rows(color_df, west_ger_col, east_ger_col, ussr_col)

col_pri <- color_df$color1
col_sec <- color_df$color2
names(col_pri) <- color_df$countryCode
names(col_sec) <- color_df$countryCode

top_df <- readRDS('data/top-m-100m.rds') %>% 
  mutate(compYrMo = year(compDate) * 100 + month(compDate))

all_compYrMo <- expand.grid(year = 1960:2021, month = 1:12) %>% 
  mutate(compYrMo = year * 100 + month) %>% 
  arrange(compYrMo) %>% 
  filter(compYrMo >= 196806 & compYrMo <= 202107) %>% 
  mutate(
    frame_order = row_number(),
    frame_desc = paste0('As of ', month.name[month], ' ', year)
  )

monthly_top20 <- lapply(all_compYrMo$compYrMo, function(x) {
  top_df %>% 
    filter(compYrMo <= x) %>% 
    arrange(mark, wind, compDate) %>% 
    mutate(
      rank_at_time = row_number(),
      evalYrMo = x,
      leader = min(mark),
      rank_ties = ifelse(mark == lag(mark, default = 0), NA, rank_at_time)
    ) %>% 
    fill(rank_ties, .direction = 'down') %>% 
    slice(1:20) %>% 
    return
}) %>% bind_rows


viz_df <- monthly_top20 %>% 
  left_join(all_compYrMo, by = c('evalYrMo' = 'compYrMo'))

addtl_end_time <- tibble(frame_order = max(all_compYrMo$frame_order) + 1:50)
addtl_start_time <- tibble(frame_order = -(1:30 - 1))

end_freeze <- viz_df %>% 
  filter(frame_order == max(frame_order)) %>% 
  mutate(frame_order = NULL) %>% 
  full_join(addtl_end_time, by = character())

start_freeze <- viz_df %>% 
  filter(frame_order == 1) %>% 
  mutate(frame_order = NULL) %>% 
  full_join(addtl_start_time, by = character())

viz_df <- viz_df %>% 
  bind_rows(start_freeze) %>% 
  bind_rows(end_freeze)


my_ani <- viz_df %>% 
  separate(compVenue, into = c('venueName', 'venueCountry'), sep = '\\(', remove = F) %>% 
  mutate(
    venueCountry = gsub('\\)', '', venueCountry),
    comp_short_date = format(compDate, '%b %Y')
  ) %>%
  left_join(champs_df) %>%
  left_join(country_name_df %>% rename(venueCountry = countryCode)) %>% 
  mutate(
    countryName = ifelse(venueCountry == 'URS', 'Soviet Union', countryName),
    countryName = ifelse(venueCountry == 'RUS', 'Russia', countryName)
  ) %>% 
  #filter(frame_order <= 510 & frame_order >= 500) %>% 
  #filter(frame_order <= 10) %>% 
  mutate(
    mark_pts = 11 - mark,
    compImg = case_when(
      compType == 'Olympics' ~ 'raw-data/rings.png',
      compType == 'World Championship' ~ 'raw-data/world.png',
      T ~ 'none'
    )
  ) %>%
  ggplot(aes(x = mark_pts, y = rank_at_time, group = paste0(fullName, compDate, compVenue, pos))) +
  geom_barh(aes(color = countryCode, fill = countryCode), stat = 'identity', show.legend = F, size = 2, width = 0.8, alpha = 0.7) +
  geom_richtext(aes(label = paste0('#', rank_ties, ' ', fullName, ', ', countryCode, ' (',number(mark, accuracy = 0.01),')'), x = mark_pts - ifelse(is.na(compType), 0.05, 0.05) - 0.01), hjust = 1, size = 11, family = 'MS Reference Sans Serif', show.legend = F, label.colour = 'transparent', fill = 'transparent', label.size = 0, nudge_y = -0.05, color = 'white') +
  geom_text(aes(x = 0.02, y = rank_at_time, label = paste0(comp_short_date, ' / ', countryName)), color = 'white', family = font_SB, size = 6, hjust = 0) +
  geom_image(aes(image = paste0('https://media.aws.iaaf.org/Flags/',countryCode,'.gif'), x = mark_pts - 0.03), asp = 2.03, size = 0.02) +
  geom_image(aes(image = ifelse(compImg=='none', NA, compImg), x = mark_pts + 0.04), asp = 2.03, size = 0.04) +
  geom_richtext(aes(x = 1.55, y = ifelse(rank_at_time == 1, 15.5, NA)), label = '= Olympic Performance<br>= World Championship<br>Performance', text.colour = '#00008B', family = font_SB, size = 9, fill = 'grey70', label.margin = unit(4, "lines"), label.padding = unit(c(1.1,1.1,1.1,4.0), "lines"), label.colour = '#00008B', label.size = 1.4, label.r = unit(0.5, "lines")) +
  geom_image(aes(image = ifelse(rank_at_time == 1, 'raw-data/rings.png', NA), x = 1.425, y = 14.9), asp = 2.03, size = 0.03) +
  geom_image(aes(image = ifelse(rank_at_time == 1, 'raw-data/world.png', NA), x = 1.43, y = 15.45), asp = 2.03, size = 0.03) +
  scale_y_reverse(expand = expansion(mult = c(0.01, 0.01))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,1.7,0.1), limit = c(0,1.75), labels = c('',number(seq(10.9,9.3,-0.1), accuracy = 0.1))) +
  scale_fill_manual(values = col_pri) +
  scale_color_manual(values = col_sec) +
  labs(title = 'Top 20 Men\'s 100m Performances of All-Time',
       subtitle = 'Handheld times have been adjusted  |  Legal wind only  |  {all_compYrMo$frame_desc[as.numeric(ifelse(as.numeric(previous_state) < 1, 1, ifelse(as.numeric(previous_state) > max(all_compYrMo$frame_order), max(all_compYrMo$frame_order), previous_state)))]}',
       x = 'Time (Seconds)',
       y = NULL) +
  theme(
    line = element_line(lineend = 'round', color='darkblue'),
    text = element_text(family = font_SB, color='darkblue'),
    plot.background = element_rect(fill = 'grey95', color = 'transparent'),
    panel.border = element_rect(color = 'darkblue', fill = NA),
    panel.background = element_rect(fill = 'white', color = 'transparent'),
    axis.ticks = element_line(color = 'darkblue', size = 1.8),
    axis.ticks.length = unit(10, 'pt'),
    axis.title = element_text(size = 29),
    axis.text = element_text(size = 25, color = 'darkblue'),
    plot.title = element_text(size = 51),
    plot.subtitle = element_text(size = 29),
    plot.caption = element_text(size = 18),
    legend.background = element_rect(fill = 'grey90', color = 'darkblue'),
    legend.key = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color='grey70', size = 1.1),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 23, color = 'darkblue', family = font_SB)
  ) + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color='grey70', size = 1.4),
    axis.line = element_line(color = 'darkblue', size = 1.8)
  ) +
  transition_states(frame_order, transition_length = 1, state_length = 1, wrap = F) +
  view_follow(fixed_y = T, fixed_x = T) +
  enter_fly(x_loc = 0) +
  exit_fly(y_loc = -21) +
  ease_aes('linear')

#718 rows (minus 1 to get total frames?)
## 1920x1080 and 5600 frames at 40fps is probably best quality
anim_save(filename = '100m race.mp4', animation = animate_SB(my_ani, nframes = 2871, fps = 40, height = 1080, width = 1920, data_home = 'Data: World Athletics', renderer = ffmpeg_renderer(ffmpeg='C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe')))
system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "100m race.mp4" -map 0 -c:v libx264 -c:a copy -y "100m race working.mp4"')

system('"C:/Program Files/ImageMagick-7.0.7-Q16/ffmpeg.exe" -i "100m race music.mp4" -map 0 -c:v libx264 -c:a copy -y "100m race music working.mp4"')

# used to create branded videos
Scene2 <- ggproto(
  "Scene2",
  gganimate:::Scene,
  plot_frame = function(self, plot, i, newpage = is.null(vp), vp = NULL, widths = NULL, heights = NULL, ...) {
    plot <- self$get_frame(plot, i)
    plot <- ggplot_gtable(plot)
    
    # insert changes here
    logo_file <- magick::image_read('C:/Users/Owner/Documents/GitHub/data-viz/ggplot/statbutler.png')
    
    author_txt <- textGrob('By Anthony Reinhard', x=unit(0.065, 'npc'), gp=gpar(col='darkblue', fontfamily=font_SB, fontsize=24), hjust=0)
    data_txt <- textGrob(self$data_home, x=unit(1 - (.01), 'npc'), gp=gpar(col='grey95', fontfamily=font_SB, fontsize=24), hjust=1)
    footer_bg <- grid.rect(x = unit(seq(0.5,1.5,length=1000), 'npc'), gp=gpar(col = 'transparent', fill = colorRampPalette(c('grey95', 'darkblue'), space = 'rgb')(1000)), draw = F)
    footer <- grobTree(footer_bg, author_txt, data_txt)
    
    plt.final <- grid.arrange(plot, footer, heights=unit(c(1, 44), c('null','pt')))
    plot <- ggdraw(plt.final) + draw_image(logo_file, x = 0.002, y = 0, hjust = 0, vjust = 0, height = 0.08, width = 0.1067 * (9/16))
    
    if (!is.null(widths)) plot$widths <- widths
    if (!is.null(heights)) plot$heights <- heights
    if (newpage) grid.newpage()
    grDevices::recordGraphics(
      requireNamespace("gganimate", quietly = TRUE),
      list(),
      getNamespace("gganimate")
    )
    if (is.null(vp)) {
      grid.draw(plot)
    } else {
      if (is.character(vp)) seekViewport(vp)
      else pushViewport(vp)
      grid.draw(plot)
      upViewport()
    }
    invisible(NULL)
  }
)

Scene2$data_home <- NULL

### the next four functions will simply duplicate existing nested gganimate functions and replace them with my personalized Scene2 function
# used to create branded videos
create_scene2 <- function(transition, view, shadow, ease, transmuters, nframes, data_home) {
  if (is.null(nframes)) nframes <- 100
  ggproto(NULL, Scene2, transition = transition, 
          view = view, shadow = shadow, ease = ease, 
          transmuters = transmuters, nframes = nframes,
          data_home = data_home)
}

# used to create branded videos
ggplot_build2 <- gganimate:::ggplot_build.gganim
formals(ggplot_build2) <- c(formals(ggplot_build2), alist(data_home = ))
body(ggplot_build2) <- body(ggplot_build2) %>%
  as.list() %>%
  inset2(4,
         quote(scene <- create_scene2(plot$transition, plot$view, plot$shadow, 
                                      plot$ease, plot$transmuters, plot$nframes, data_home))) %>%
  as.call()


# used to create branded videos
prerender2 <- gganimate:::prerender
formals(prerender2) <- c(formals(prerender2), alist(data_home = ))
body(prerender2) <- body(prerender2) %>%
  as.list() %>%
  inset2(3,
         quote(ggplot_build2(plot, data_home))) %>%
  as.call()


# used to create branded videos
animate_SB <- gganimate:::animate.gganim
formals(animate_SB) <- c(formals(animate_SB)[-length(formals(animate_SB))], alist(data_home = ''), formals(animate_SB)[length(formals(animate_SB))])
body(animate_SB) <- body(animate_SB) %>%
  as.list() %>%
  inset2(8,
         quote(plot <- prerender2(plot, nframes_total, data_home))) %>%
  as.call()


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

