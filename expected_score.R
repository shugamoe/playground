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
# HW2 Expected scores from 1st and 10s with at least 25 minutes left in
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

calc_scoring_until_half <- function(play_row, plays_df){
  # print(play_row$gid)
  search_df <- plays_df %>%
    filter(gid == play_row$gid,
           pid >= play_row$pid,
           qtr == play_row$qtr | qtr == play_row$qtr + 1) %>%
    by_row(calc_team_scoring_until_half, off_of_int = play_row$off,
                .collate = 'cols', .to = 'toi_score') %>%
  # TOI = Team of interest
    filter(toi_score != 'bad')

  return(c(sum(search_df$toi_score)))
}

calc_team_scoring_until_half <- function(play_row, off_of_int){
  if (off_of_int == play_row$off){
    return(play_row$pts)
  } else if (off_of_int == play_row$def){
    return(- play_row$pts)
  } else {
    if (play_row$pts != 0){
      print('welp...')
    }
    # browser()
    # print(sprintf("off of int: %s \n off: %s def: %s fuck", off_of_int, off, def))
    return(play_row$pts)
  }
}

if (!exists('first_and_tens')){
  first_and_tens <- plays_df %>% 
    filter(dwn == 1,
           ytg == 10,
           qtr %in% c(1, 2, 3, 4),
           def != off) %>%
    by_row(calc_min_in_half, .collate = "cols", .to = "min_in_half") %>%
    filter(min_in_half > 25) %>%
    select(gid, pid, qtr, off, yfog)
} else if(!"scoring_until_half" %in% names(first_and_tens)) {
  first_and_tens <- first_and_tens %>%
    by_row(calc_scoring_until_half,
                plays_df = plays_df_filtered,
                .collate = "cols",
                .to = "scoring_until_half")
} else {
  table_of_int <- first_and_tens %>% 
    group_by(yfog) %>% 
    summarise(mean_score = mean(scoring_until_half),
              n_obs = n()) %>% 
    ungroup()
  
  exp_score_plot <- table_of_int %>%
    ggplot(aes(yfog, mean_score)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    labs(title = 'Mean net Score until half by yard line',
         x = 'Yard Line',
         y = 'Mean Net Score Until Half')

  n_obs_plot <- table_of_int %>%
     ggplot(aes(yfog, n_obs)) +
     geom_line() +
     labs(title = 'Number of observations by yard line',
            x = 'Yard Line',
            y = '# Observations')
  
  print(exp_score_plot)
  print(n_obs_plot)
}






