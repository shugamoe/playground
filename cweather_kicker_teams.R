# Create spreadsheet for cold weather kickers

library(tidyverse)

players_df <- read_csv('nfl_00_16/PLAYER.csv')
kickers_df <- read_csv('nfl_00_16/KICKER.csv')

players_of_int <- c(
  'DA-0300',
  'SJ-0300',
  'AV-0400',
  'RL-1300',
  'JF-0900',
  'NR-0100',
  'MS-5200',
  'OM-0100',
  'JH-0900',
  'RL-0900',
  'PD-0200',
  'JK-0200',
  'JE-0200',
  'KB-2300',
  'JB-7100',
  'JC-0900',
  'JR-1100',
  'SG-1100',
  'JW-3300',
  'JN-0600',
  'MB-4600',
  'RG-1500',
  'RB-2200',
  'JS-1100',
  'NK-0200',
  'LT-1100',
  'MV-0100'
)

kickers_df %>%
  filter(player %in% players_of_int, year <= 2012) %>%
  merge(players_df, by = 'player') %>%
  mutate(full_name = paste(fname, lname)) %>%
  dplyr::select(full_name, player, year, team) %>%
  arrange(full_name, year) %>%
  group_by(full_name, player, year) %>%
  summarise(com_team = max(team)) %>%
  ungroup() %>%
  spread(year, com_team) %>%
  write_csv('kicker_team_table2000_2012.csv')
