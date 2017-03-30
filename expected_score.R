# R file to create model for expected number of points for next score
library(tidyverse)
library(purrr)


if (!exists('plays_df')){
  plays_df <- read_csv('nfl_00_16/PLAY.csv')
}


# TODO(jcm): Change this function, there are some mistakes in it. The 
# pts is positive for off, negative for def, but who is offense and defense may
# change.
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
           qtr <= play$qtr,
           pid > play$pid,
           pts != 0)
  
  if (dim(search_df)[1] == 0){
    return(0) # There was no next score (end of quarter, half, or game).
  }

  next_score_play <- search_df[1,]
  next_score_play$pts
}

# plays_df %>% 
#   # filter(gid == 1) %>%
#   mutate(next_score = map2_dbl(.$gid, .$pid, ~ calc_next_score(.x, .y))) %>%
#   write.csv('PLAY_NS.csv')

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Homework 2 Expected scores from 1st and 10s with at least 25 minutes left in
# the half until the end of the half.

calc_min_in_half <- function(play_row){
  qtr <- play_row$qtr
  min <- play_row$min
  sec <- play_row$sec
  if (qtr %in% c(2, 4)){
    return(min + sec / 60)
  } else if (qtr %in% c(1, 3)){
    return(15 + min + sec / 60)
  }
}

plays_df_filtered <- plays_df %>%
  select(gid, pid, off, def, pts, qtr)

calc_scoring_until_half <- function(gid, pid, qtr, off, plays_df){
  search_df <- plays_df %>%
    filter(gid == gid,
           pid >= pid,
           qtr == qtr | qtr == qtr + 1) %>%
    select(off, def, pts) %>%                                 
    invoke_rows(calc_teams_scoring_until_half, ., off_of_int = off,
                .collate = 'cols', .to = 'toi_score') # TOI = Team of interest
  
  sum(search_df$toi_score)
}

calc_teams_scoring_until_half <- function(off_of_int, off, def, pts){
  if (off_of_int == off){
    return(pts)
  } else if (off_of_int == def){
    return(- pts)
  } else {
    print(sprintf("off of int: %s \n off: %s def: %s fuck", off_of_int, off, def))
  }
}



first_and_tens <- plays_df %>% 
  filter(dwn == 1,
         ytg == 10,
         qtr %in% c(1, 2, 3, 4)) %>%
  by_row(calc_min_in_half, .collate = "cols", .to="min_in_half") %>%
  filter(min_in_half > 25) %>%
  select(gid, pid, qtr, off) %>%
  invoke_rows(calc_scoring_until_half, ., 
              plays_df=plays_df_filtered,
              .collate="cols",
              .to="scoring_until_half")
