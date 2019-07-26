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

top_1k_long <- all_data_top_1k %>%
  ungroup() %>%
  distinct(user, song_id, plays) %>% 
  arrange(song_id, user) %>% 
  group_by(song_id) 

view_songinfo <- function(tbl, left_join_col_name=song_id){
  tbl %>% rename(
    song_id = {{left_join_col_name}}
  ) -> tbl 
  tbl %>% left_join(song_data, by = 'song_id')
}
calc_dot_product <- function(vec_x, vec_y){
  
  vec_x %>% ungroup() %>% select(user, plays) -> vec_x
  vec_y %>% ungroup() %>% select(user, plays) -> vec_y
  
  vec_x %>% 
    full_join(vec_y, by = 'user') %>% 
    replace_na(list(plays.x = 0, plays.y = 0)) %>% 
    mutate(
      prod = plays.x * plays.y
    ) %>% 
    summarise(
      norm.x = sqrt(sum(plays.x^2)), 
      norm.y = sqrt(sum(plays.y^2)), 
      dot_prod = sum(prod)
    ) %>%  
    mutate(
      cos_sim = dot_prod / (norm.x * norm.y)
    )
}

# Wrapping function 
generate_song_list_by_cos_sim <- function(song_id_x, input_tbl){
  
  input_tbl -> tblf0 
  
  tblf1 <- tblf0 %>%
    ungroup() %>%
    distinct(user, song_id, plays) %>% 
    arrange(song_id, user) 
  
  tblf1 %>% filter(song_id == song_id_x) -> vector_x 
  
  tblf1 %>% 
    group_by(song_id) %>% 
    group_modify( ~ calc_dot_product(vector_x, .)) %>% 
    arrange(-cos_sim) %>% view_songinfo()
} 

generate_song_list_by_cos_sim("SODEOCO12A6701E922", all_data_top_1k) -> vdf 
generate_song_list_by_cos_sim('SOPJLFV12A6701C797', all_data_top_1k) -> vdf 
generate_song_list_by_cos_sim('SOJYBJZ12AB01801D0', all_data_top_1k) -> vdf 
