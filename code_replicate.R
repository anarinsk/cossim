#librarian::shelf(hadley/tidyr)
#devtools::install_github("tidyverse/tidyr", INSTALL_opts = c('--no-lock'))
librarian::shelf(tidyverse, tidyr)

# read user play data and song data from the internet
play_data <- "https://static.turi.com/datasets/millionsong/10000.txt" %>%
  read_tsv(col_names = c('user', 'song_id', 'plays'))

song_data <- 'https://static.turi.com/datasets/millionsong/song_data.csv' %>%
  read_csv() %>%
  distinct(song_id, title, artist_name)

# join user and song data together
all_data <- play_data %>%
  group_by(user, song_id) %>%
  summarise(plays = sum(plays, na.rm = TRUE)) %>%
  inner_join(song_data)

top_1k_songs <- all_data %>%
  group_by(song_id, title, artist_name) %>%
  summarise(sum_plays = sum(plays)) %>%
  ungroup() %>%
  top_n(1000, sum_plays) %>% 
  distinct(song_id)

all_data_top_1k <- all_data %>%
  inner_join(top_1k_songs)

top_1k_wide <- all_data_top_1k %>%
  ungroup() %>%
  distinct(user, song_id, plays) %>%
  spread(song_id, plays, fill = 0)

view_songinfo <- function(tbl, left_join_col_name){
  tbl %>% rename(
    song_id = {{left_join_col_name}}
  ) -> tbl 
  tbl %>% left_join(song_data, by = 'song_id')
}
calc_cos_sim <- function(song_code, input_tbl){
#  
  input_tbl %>% select(-user) -> fbl0 
  fbl0 %>% 
    mutate_at(vars(-{{song_code}}), ~ . * {{song_code}}) %>% 
    summarise_all( ~sum(.) ) -> dot_product
  
  fbl0 %>% 
    mutate_all(~ .^2) %>% 
    summarise_all( ~sqrt(sum(.)) ) -> norm 
  
  norm %>% 
    mutate_at(vars(-{{song_code}}), ~ . * {{song_code}}) -> denom
  
  (dot_product / denom) %>% 
    tidyr::gather() %>% 
    filter(between(value, -1, 1)) %>% 
    arrange(-value) 
# 
}

### Test 
calc_cos_sim(SOIHUUT12AF72A2188, top_1k_wide) %>% view_songinfo(key) -> vdf 
calc_cos_sim(SOIHUUT12AF72A2188, top_1k_wide) -> vdf 


view_songinfo(top_1k_songs, song_id) -> vdf 

vdf %>% 
  rename(key = song_id)
