library(tidyverse)
library(readxl)library(gmailr)
library(xtable)
setwd('~/OneDrive - Cal Poly/stat218-s24-submissions')
gm_auth_configure(path = '~/documents/courses/stat218/_zz-gmail-auth.json')

## HELPER FUNCTIONS

fact_fn <- function(x){factor(x, levels = unique(x)) |> as.numeric()}
max_na <- function(x){
  if(sum(is.na(x)) == length(x)){
    NA
  }else{
    max(x, na.rm = T)
  }
}

## READ IN RAW ASSIGNMENT DATA

ps1_revised <- readxl::read_xlsx('PS1 Corrections.xlsx') |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  # group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'ps1')

ps1_original <- readxl::read_xlsx('PS1.xlsx') |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question), 
         assignment = 'ps1')

ps1 <- full_join(ps1_original, ps1_revised, 
          by = c('name', 'email', 'outcome', 'question', 'question.number', 'assignment'),
          suffix = c('.original', '.revised')) |>
  mutate(score = max_na(c(score.original, score.revised))) |>
  select(-score.original, -score.revised) |>
  select(name, email, outcome, assignment, question, question.number, score)

ps2 <- readxl::read_xlsx('PS2.xlsx') |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'ps2') |>
  select(name, email, outcome, assignment, question, question.number, score) |>
  filter(question.number != 7)

ps3 <- readxl::read_xlsx('PS3.xlsx') |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'ps3') |>
  select(name, email, outcome, assignment, question, question.number, score) |>
  filter(question.number != 11)

ps4 <- readxl::read_xlsx('PS4.xlsx') |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'ps4') |>
  select(name, email, outcome, assignment, question, question.number, score)

ps5 <- readxl::read_xlsx('PS5.xlsx') |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'ps5') |>
  select(name, email, outcome, assignment, question, question.number, score)


test1_original <- readxl::read_xlsx('Test1.xlsx') |>
  bind_rows(readxl::read_xlsx('Test1 Extension.xlsx')) |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'test1') |>
  select(name, email, outcome, assignment, question, question.number, score) |>
  filter(str_starts(outcome, 'l')) 

test1_revised <- readxl::read_xlsx('Test1 Corrections.xlsx') |>
  bind_rows(readxl::read_xlsx('Test1 Extension Corrections.xlsx')) |>
  rename_with(tolower)  |>
  select(name, email, starts_with('points')) |>
  pivot_longer(starts_with('points'), 
               names_to = 'question', 
               values_to = 'score') |>
  separate(question, into = c('outcome', 'question'), sep = '] ') |>
  group_by(name, email) |>
  mutate(outcome = str_remove(outcome, '.*\\['),
         question.number = fact_fn(question),
         assignment = 'test1') |>
  select(name, email, outcome, assignment, question, question.number, score) |>
  filter(str_starts(outcome, 'l')) 

test1_manual_questions <- c(2, 3, 7, 9, 10, 12, 17, 18, 19, 20, 23, 24, 25, 26, 28, 40, 41)

test1_joined <- full_join(test1_original, test1_revised, 
                 by = c('name', 'email', 'outcome', 'question', 'question.number', 'assignment'),
                 suffix = c('.original', '.revised')) |>
  rowwise() |>
  mutate(score = if_else(question.number %in% test1_manual_questions,
                         max_na(c(score.original, score.revised)),
                         max_na(c(score.original, mean(c(score.original, score.revised), na.rm = T))))) |>
  mutate(score = if_else(score == 0.5, 0.2, score))

test1 <- test1_joined |>
  select(-score.original, -score.revised) |>
  select(name, email, outcome, assignment, question, question.number, score)

## JOIN AND COMPUTE SCORES

scores_long <- bind_rows(ps1, ps2, ps3, ps4, ps5, test1) |>
  unite('problem', c(assignment, question.number, outcome, question), sep = '_._') |>
  pivot_wider(names_from = problem, values_from = score) |>
  pivot_longer(-c(1:2), names_to = 'problem', values_to = 'score') |>
  separate_wider_delim(problem, 
                       names = c('assignment', 'question.number', 'outcome', 'question'), 
                       delim = '_._') |>
  mutate(score = replace_na(score, 0)) |>
  mutate(score = if_else((assignment == 'test1') & (question.number%in%c(38, 39)),
                         na_if(score, 0),
                         score)) |>
  mutate(score = if_else((assignment == 'ps3') & (question.number == 23),
                          na_if(score, 0),
                          score)) 

outcomes <- scores_long |>
  group_by(name, email, outcome) |>
  summarize(score = mean(score, na.rm = T)) |>
  mutate(current.status = cut(score, 
                              breaks = c(0, 0.5, 0.8, 1.1), 
                              right = F, 
                              labels = c('not met', 'partly met', 'fully met'))) |>
  mutate(outcome = toupper(outcome))

scores_long |>
  group_by(name, email) |>
  summarize(n.na = mean(is.na(score))) |>
  arrange(desc(n.na))

## FOR CHECKING INDIVIDUAL RECORDS

outcomes |> filter(email == 'sashouri@calpoly.edu')
scores_long |> filter(name == 'Abigail Blair')

## EMAIL GRADE SUMMARIES

students <- outcomes |> distinct(name, email)
names <- pull(students, name)
emails <- pull(students, email)

for(i in 1:length(names)){
  
  tbl1 <- filter(outcomes, name == names[i]) |> 
    ungroup() |>
    select(name, outcome, score, current.status) |> 
    xtable() |> 
    print(type = 'html', include.rownames = F)
  
  tbl2 <- filter(test1_joined, name == names[i]) |>
    ungroup() |>
    select(name, question.number, outcome, score.original, score.revised, score) |>
    mutate(score.revised = na_if(score.revised, 0),
           score = if_else((question.number%in%c(38, 39)),
                           na_if(score, 0),
                           score)) |>
    rename(score.final = score) |>
    xtable() |> 
    print(type = 'html', include.rownames = F)
  
  body <- paste("<html>Good afternoon, <br><br> The table below provides a current estimate of your scores by learning outcome; you may refer to the syllabus to understand how these scores will be utilized in determining letter grades. 
                <br><br> Please note that these are *estimates only* and will change as further assignments are taken into account. Please also note that the scores are currently unweighted; assignment weights may be used in final grade calculations. In short, these estimates are not final and are intended to give you an *approximate* sense of where you currently stand on the outcomes we have covered based on the assignments you have submitted.<br><br>
                This summary is based on PS1 through PS5, and Test 1. A summary of your test 1 initial scores, revision scores, and final scores is provided below the signature line. If you received an extension on Test 1, your revision scores are not yet available but will be included in the next grade summary.<br>",
                tbl1, "<br> Please let me know if you have any questions or notice any potential errors. If so, please reply to my Cal Poly email (on cc).<br><br> Trevor <br>",
                tbl2, "<br>*Please note that question numbers 38-39 were optional, and only count towards your score if answered correctly.", sep = '<br>')
  
  email <- gm_mime() |>
    gm_to(emails[i]) |>
    gm_from("tdruiz001@gmail.com") |>
    gm_cc("truiz01@calpoly.edu") |>
    gm_subject(paste("STAT218 current score estimates (" , names[i], ')', sep = '')) |>
    gm_html_body(body)
  
  gm_send_message(email)
}


names
