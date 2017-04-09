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

games_df <- read_csv('nfl_00_16/GAME.csv') %>%
  select(gid, h, seas)
plays_df <- read_csv('nfl_00_16/PLAY.csv') %>%
 filter(qtr %in% c(1, 2, 3, 4)) %>%
 mutate(min_in_half = ifelse(qtr %in% c(2, 4), min + sec / 60,
                             ifelse(qtr %in% c(1,3),     # -100 shouldn't happen
                                    15 + min + sec / 60, -100)),
        min_in_game = ifelse(qtr %in% c(3,4), min_in_half, 
                             ifelse(qtr %in% c(1, 2), 30 + min_in_half, NA))) %>%
 merge(games_df, ., by = 'gid')
 plays_df$min_in_half <- as.numeric(plays_df$min_in_half)

calc_scoring_until_reset <- function(play_row, plays_df){
  print(sprintf('g: %i | p: %i', play_row$gid, play_row$pid))
  
  # Get the current play, and all future plays within the same game that are in
  # the same half of the game.
  search_df <- plays_df %>%
    filter(gid == play_row$gid,
           pid >= play_row$pid,
           (qtr == play_row$qtr) | (qtr == play_row$qtr + 1)) %>%
    by_row(calc_net_scores, off_of_int = play_row$off,
                .collate = "cols", .to = "net_score") 
  
  
  # We don't count a safety as a reset, only FGs and TD's + extra point(s)
  fg_td_only <- search_df %>%
    filter(net_score %in% c(-8, -7 , -6, -3, 3, 6, 7, 8)) 
  
  # If there wasn't a TD + extra point(s) or FG, we want to add the last play 
  last_play <- tail(search_df, 1)
  if (nrow(fg_td_only) == 0){
    fg_td_only <- fg_td_only %>%
      bind_rows(last_play)
    net_till_reset_score <- last_play$net_score
    time_to_reset <- play_row$min_in_half
    reset_min_in_half <- 0
    # browser()
    
  # Otherwise, there was a TD + extra point(s) or FG before the half ended.
  } else {
    reset_play <- fg_td_only[1,]
    net_till_reset_score <- reset_play$net_score
    
    # If there was only one TD or FG before the half ended.
    play_after_reset <- search_df %>% 
      filter(pid == reset_play$pid + 1)
    # If the TD or FG was the actually the last play of the half
    if ((reset_play$qtr == 2) & (play_after_reset$qtr == 3)){
      time_to_reset <- play_row$min_in_half
      reset_min_in_half <- play_after_reset$min_in_half
    # If the TD or FG was not the last play of the half
    } else {
      time_to_reset <- play_row$min_in_half - play_after_reset$min_in_half 
      reset_min_in_half <- play_after_reset$min_in_half
    }
  }
  
  # It's possible that the last play of the half could be a safety. Let's check
  # if that's the case. We didn't want to count safety's as
  if (net_till_reset_score %in% c(2, -2)){
    print('last play in half is safety?')
    browser()
  }
  net_till_half_score <- sum(search_df$net_score)
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
    select(seas, gid, pid, qtr, min_in_game, min_in_half, min, sec, h, ptso, ptsd, off, def, yfog, dseq)
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