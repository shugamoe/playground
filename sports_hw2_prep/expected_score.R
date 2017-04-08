# R file to create model for expected number of points for next score
library(tidyverse)
library(purrr)
library(modelr)


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

if (!exists('plays_df')){
 games_df <- read_csv('nfl_00_16/GAME.csv') %>%
    select(gid, h, seas)
 plays_df <- read_csv('nfl_00_16/PLAY.csv') %>%
   filter(qtr %in% c(1, 2, 3, 4)) %>%
   mutate(min_in_half = ifelse(qtr %in% c(2, 4), min + sec / 60,
                               ifelse(qtr %in% c(1,3), 
                                      15 + min + sec / 60, 'not_possible')),
          min_in_game = ifelse(qtr %in% c(2, 4), min_in_half,
                               min_in_half + 15)) %>%
   merge(games_df, ., by = 'gid')
   plays_df$min_in_half <- as.numeric(plays_df$min_in_half)
}

calc_scoring_until_reset <- function(play_row, plays_df){
  print(sprintf('g: %i | p: %i', play_row$gid, play_row$pid))
  search_df <- plays_df %>%
    filter(gid == play_row$gid,
           pid >= play_row$pid,
           (qtr == play_row$qtr) | (qtr == play_row$qtr + 1)) %>%
    by_row(calc_net_scores, off_of_int = play_row$off,
                .collate = "cols", .to = "net_score") 
  
  first_play <- search_df[1,]
  second_play <- search_df[2,]
  last_play <- tail(search_df, 1)
  fg_td_only <- search_df %>%
    filter((net_score != 2 & net_score != -2) & (net_score != 0)) 
  
  # browser()
  if (nrow(fg_td_only) == 0){
    fg_td_only <- fg_td_only %>%
      bind_rows(last_play)
    # browser()
  }
  net_till_reset_score <- fg_td_only$net_score[1]
  net_till_half_score <- sum(search_df$net_score)
  if ((first_play$net_score != 0 & first_play$net_score != -2 &
       first_play$net_score != 2)){
    time_to_reset <- first_play$min_in_half - second_play$min_in_half 
    reset_min_in_half <- second_play$min_in_half
  } else if (net_till_reset_score == 0){
    time_to_reset <- play_row$min_in_half
    reset_min_in_half <- 0 # 
  } else {
    time_to_reset <- play_row$min_in_half - fg_td_only$min_in_half[1]
    reset_min_in_half <- fg_td_only$min_in_half[1]
  }
  
  net_score_info <- c(net_till_half_score, net_till_reset_score, reset_min_in_half, time_to_reset)
  if (length(net_score_info) != 4){
    browser()
  }
  net_score_info
}

calc_net_scores <- function(play_row, off_of_int){
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

make_raw_exp_scores_table <- function(test = FALSE, plays_df){
  force(plays_df)
  if (test){
    gid_stop <- 1
  } else {
    gid_stop <- Inf
  }
  first_and_tens <- plays_df %>%
    filter(dwn == 1,
         ytg == 10 | yfog > 90,
         qtr %in% c(1, 2, 3, 4),
         def != off,
         gid <= gid_stop) %>%
    select(seas, gid, pid, qtr, min_in_half, min, sec, h, ptso, ptsd, off, def, yfog, dseq)
  # browser()
  first_and_tens <- first_and_tens %>%
    mutate(drive_start = ifelse(dseq == 1, 1, 0)) %>%
    by_row(calc_scoring_until_reset, plays_df = plays_df, .collate = "cols",
           .to = "ex_score_info")  %>%
    rename(net_score_to_half = ex_score_info1,
           net_score_to_reset = ex_score_info2,
           time_to_reset = ex_score_info4,
           reset_min_in_half = ex_score_info3) %>%
    mutate(reset_min_in_game = ifelse(qtr %in% c(1, 3), 15 + reset_min_in_half,
                                     reset_min_in_half))
  first_and_tens
}

first_and_tens <- make_raw_exp_scores_table(FALSE, plays_df)
write_csv(first_and_tens, 'raw_fdowns_nscore_half_and_reset.csv')