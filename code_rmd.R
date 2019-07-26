#devtools::install_github("yihui/xfun") 
#install.packages("Rcpp", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#install.packages("testthat", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#devtools::install_github("tidyverse/tidyverse", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
#devtools::install_github("tidyverse/tidyr", INSTALL_opts = c('--no-lock')

#devtools::install_github('rstudio/fontawesome')
#devtools::install_github('juba/rmdformats')
#install.packages('DT')
#install.packages('showtext')

xfun::pkg_attach(c("tidyverse", 'DT', 'showtext'))

all_data_top_1k <- readRDS("all_data_top_1k.rds")

view_songinfo <- function(tbl, left_join_col_name=song_id){
  tbl %>% rename(
    song_id = {{left_join_col_name}}
  ) -> tbl 
  
  song_data_1k <- all_data_top_1k %>% 
    ungroup() %>% 
    select(-c(user, plays)) %>% 
    distinct(song_id, .keep_all=TRUE)
  
  tbl %>% left_join(song_data_1k, by = 'song_id')
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
