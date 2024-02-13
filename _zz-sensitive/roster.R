library(tidyverse)

roster_05 <- read_csv("_zz-sensitive/Class Roster - Winter Quarter 2024 - STAT 218 (8273).csv")
roster_06 <- read_csv("_zz-sensitive/Class Roster - Winter Quarter 2024 - STAT 218 (8274).csv")

bind_rows(roster_05, roster_06) %>%
  separate(`Plan(s)`, into = c('major', 'drop'), sep = ' ') %>%
  rename(standing = Level) %>%
  select(major, standing) %>%
  write_csv(file = 'content/data/roster-anonymous.csv')

bind_rows(roster_05, roster_06) %>%
  select(Name, `Email Address`) %>%
  rename(Email = `Email Address`) %>%
  separate(Name, into = c('Last Name', 'First Name'), sep = ', ') %>%
  separate(`Last Name`, into = c('first', 'second')) %>%
  mutate(`Last Name` = if_else(is.na(second), first, second)) %>%
  select(`First Name`, `Last Name`, Email) %>%
  write_csv(file = '_zz-sensitive/gradescope-roster.csv')
