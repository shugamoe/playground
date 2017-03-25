# R file to create model for expected number of points for next score
library(tidyverse)
library(purrr)

setwd('~/playground/')

if (!exists('plays_df')){
  plays_df <- as_data_frame(read.csv('nfl_00_16/PLAY.csv'))
}

calc_next_score <- function(play_gid, play_pid, inspect = FALSE){
  print(paste(play_gid, play_pid))
  if (inspect){
    browser()
  }
  
  play <- plays_df %>% 
    filter(gid == play_gid, pid == play_pid)
  
  # Check if the next score actually happened on the play in question
  if (play$pts != 0){
    return(play$pts) 
  }
    
  search_df <- plays_df %>%
    filter(gid == play$gid, 
           qtr == play$qtr,
           pid > play$pid,
           pts != 0)
  
  if (dim(search_df)[1] == 0){
    return(0) # There was no next score (end of quarter, half, or game).
  }

  next_score_play <- search_df[1,]
  next_score_play$pts
}

plays_df %>% 
  # filter(gid == 1) %>%
  mutate(next_score = map2_dbl(.$gid, .$pid, ~ calc_next_score(.x, .y))) %>%
  write.csv('PLAY_NS.csv')
