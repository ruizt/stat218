library(tidyverse)
library(readxl)
library(gmailr)
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

read_assignment <- function(assignment_name, optional_qnum, manual_qnum, w){
  correction_path <- paste(assignment_name, 'Corrections.xlsx', sep = ' ')
  
  if(!file.exists(correction_path)){
    out <- paste(assignment_name, '.xlsx', sep = '') |>
      readxl::read_xlsx() |>
      rename_with(tolower)  |>
      select(name, email, starts_with('points')) |>
      pivot_longer(starts_with('points'), 
                   names_to = 'question', 
                   values_to = 'score') |>
      separate(question, into = c('outcome', 'question'), sep = '] ') |>
      group_by(name, email) |>
      mutate(outcome = str_remove(outcome, '.*\\['),
             question.number = fact_fn(question),
             assignment = assignment_name |> str_split_i(' ', 1) |> tolower(),
             optional = question.number %in% optional_qnum) |>
      select(name, email, outcome, assignment, 
             question, question.number, optional, score)
  }else{
    original <- paste(assignment_name, '.xlsx', sep = '') |>
      readxl::read_xlsx() |>
      rename_with(tolower)  |>
      select(name, email, starts_with('points')) |>
      pivot_longer(starts_with('points'), 
                   names_to = 'question', 
                   values_to = 'score') |>
      separate(question, into = c('outcome', 'question'), sep = '] ') |>
      group_by(name, email) |>
      mutate(outcome = str_remove(outcome, '.*\\['),
             question.number = fact_fn(question),
             assignment = assignment_name |> str_split_i(' ', 1) |> tolower(),
             optional = question.number %in% optional_qnum) |>
      select(name, email, outcome, assignment, 
             question, question.number, optional, score)
    
    revised <- paste(assignment_name,  'Corrections.xlsx', sep = ' ') |>
      readxl::read_xlsx() |>
      rename_with(tolower)  |>
      select(name, email, starts_with('points')) |>
      pivot_longer(starts_with('points'), 
                   names_to = 'question', 
                   values_to = 'score') |>
      separate(question, into = c('outcome', 'question'), sep = '] ') |>
      group_by(name, email) |>
      mutate(outcome = str_remove(outcome, '.*\\['),
             question.number = fact_fn(question),
             assignment = assignment_name |> str_split_i(' ', 1) |> tolower(),
             optional = question.number %in% optional_qnum) |>
      select(name, email, outcome, assignment, 
             question, question.number, optional, score)
    
    out <- full_join(original, revised, 
                     by = c('name', 'email', 'outcome', 'question', 'question.number', 'optional', 'assignment'),
                     suffix = c('.original', '.revised')) |>
      rowwise() |>
      mutate(score = if_else(question.number %in% manual_qnum,
                             max_na(c(score.original, score.revised)),
                             max_na(c(score.original, mean(c(score.original, score.revised), na.rm = T))))) |>
      mutate(score = if_else(score == 0.5, w[1], w[2]*score)) |>
      ungroup() |>
      select(-score.original, -score.revised) |>
      select(name, email, outcome, assignment, question, question.number, optional, score)
  }
  
  return(out)
}

## READ IN ASSIGNMENT DATA

# assignment names
names <- c(paste('PS', 1:10, sep = ''), paste('Test', 1:2, sep = ''), 'Test1 Extension')

# optional questions
optional_qnums <- list(
  ps3 = c(23),
  test1 = c(38, 39),
  test3 = c(32, 33, 34, 35)
)

# manual questions
manual_qnums <- list(
  ps1 = 1:8,
  test1 = c(2, 3, 7, 9, 10, 12, 17, 18, 19, 20, 23, 24, 25, 26, 28, 40, 41),
  test2 = c(1, 4, 8, 15, 18, 22, 23, 36, 38, 47, 48, 51, 53, 57),
  test3 = c(9, 12, 19, 23, 24, 30, 31, 35)
)

# correction credit (autograded, manual)
correction_weights <- c(0.3, 0.9)

# read in assignments
scores_long <- lapply(names, function(.x){
  read_assignment(assignment_name = .x, 
                  optional_qnum = eval(parse(text = paste('optional_qnums$', tolower(str_split_i(.x, ' ', 1)), sep = ''))),
                  manual_qnum = eval(parse(text = paste('manual_qnums$', tolower(str_split_i(.x, ' ', 1)), sep = ''))),
                  w = correction_weights)
}) |>
  bind_rows() |>
  unite('problem', c(assignment, question.number, outcome, question, optional), sep = '_._') |>
  pivot_wider(names_from = problem, values_from = score) |>
  pivot_longer(-c(1:2), names_to = 'problem', values_to = 'score') |>
  separate_wider_delim(problem, 
                       names = c('assignment', 'question.number', 'outcome', 'question', 'optional'), 
                       delim = '_._') |>
  mutate(optional = as.logical(optional))

dropped <- scores_long |>
  group_by(name, email) |>
  summarize(prop.na = mean(is.na(score))) |>
  arrange(desc(prop.na)) |>
  filter(prop.na > 0.9) |>
  pull(name)
 
# # weighted learning outcome scores
# outcomes <- scores_long |>
#   filter(!(name %in% dropped)) |>
#   mutate(score = replace_na(score, 0)) |>
#   mutate(score = if_else(optional == T,
#                          na_if(score, 0),
#                          score),
#          category = str_trunc(assignment, width = 1, side = 'right', ellipsis = '')) |>
#   group_by(name, email, outcome, category) |>
#   summarize(score = mean(score, na.rm = T),
#             n.questions = n()) |>
#   left_join(tibble(category = c('t', 'p'), weight = c(0.5, 0.5)), by = 'category') |>
#   mutate(weighted.score = score*weight) |>
#   group_by(name, email, outcome) |>
#   summarize(score = sum(weighted.score)) |>
#   mutate(current.status = cut(score, 
#                               breaks = c(0, 0.5, 0.8, 1.1), 
#                               right = F, 
#                               labels = c('not met', 'partly met', 'fully met'))) |>
#   mutate(outcome = toupper(outcome)) |>
#   filter(str_starts(outcome, 'L'))

# unweighted learning outcome scores
outcomes <- scores_long |>
  filter(!(name %in% dropped)) |>
  mutate(score = replace_na(score, 0)) |>
  mutate(score = if_else(optional == T,
                         na_if(score, 0),
                         score)) |>
  group_by(name, email, outcome) |>
  summarize(score = mean(score, na.rm = T)) |>
  mutate(current.status = cut(score, 
                              breaks = c(0, 0.5, 0.8, 1.1), 
                              right = F, 
                              labels = c('not met', 'partly met', 'fully met'))) |>
  mutate(outcome = toupper(outcome)) |>
  filter(str_starts(outcome, 'L'))



## FOR CHECKING INDIVIDUAL RECORDS

outcomes |> filter(name == 'XX')
scores_long |> filter(name == 'XX')


## FOR INSTRUCTOR SUMMARIES

# distribution of questions across assignments
scores_long |>
  distinct(assignment, question.number, outcome) |>
  group_by(assignment, outcome) |>
  count() |>
  spread(outcome, n)

# score summary stats by outcome
outcomes |>
  group_by(outcome) |>
  summarize(mean = mean(score),
            median = median(score),
            sd = sd(score))

# percentage of students not/partly/fully meeting each outcome
outcomes |>
  group_by(outcome) |>
  count(current.status) |>
  mutate(prop = n/sum(n)) |>
  pivot_wider(id_cols = -n, names_from = current.status, values_from = prop)

# pass rate
outcomes |>
  group_by(name) |>
  count(current.status) |>
  mutate(current.status = fct_collapse(current.status, 
                                       pass = c('partly met', 'fully met'),
                                       no.pass = 'not met')) |> 
  group_by(name, current.status) |>
  summarize(n = sum(n)) |>
  filter(current.status == 'pass') |>
  group_by(n) |>
  count(name = 'count') |>
  ungroup() |>
  mutate(prop = count/sum(count))

# id students not passing
notify_list <- outcomes |>
  group_by(name, email) |>
  count(current.status) |>
  spread(current.status, n) |>
  rename_with(~str_replace(.x, ' ', '.')) |>
  mutate(across(ends_with('met'), ~replace_na(.x, 0))) |>
  mutate(n.passing = partly.met + fully.met) |>
  filter(n.passing < 6, fully.met < 3) |>
  mutate(n.short.passing = 6 - n.passing,
         n.short.c = 3 - fully.met) |>
  select(n.short.passing, n.short.c) |>
  arrange(desc(n.short.passing)) |>
  print(n = 30)


## EMAIL GRADE SUMMARIES

students <- outcomes |> distinct(name, email)
student_names <- pull(students, name)
student_emails <- pull(students, email)

for(i in 1:length(student_names)){
  
  tbl1 <- filter(outcomes, name == student_names[i]) |> 
    ungroup() |>
    select(name, outcome, score, current.status) |> 
    xtable() |> 
    print(type = 'html', include.rownames = F)
  
  tbl2 <- filter(scores_long, name == student_names[i], assignment == 'test2') |>
    select(name, question.number, outcome, optional, score) |>
    mutate(optional = na_if(optional, F)) |>
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

## EMAIL NOTIFICATIONS

notify_names <- notify_list$name
notify_emails <- notify_list$email

for(i in 1:length(notify_names)){

notification <- paste('Tentative estimates indicate that you are ', 
      notify_list$n.short.passing[i], 
      ' partly or fully met outcomes short of the six required to pass and, provided you meet the passing requirement, ',
      notify_list$n.short.c[i],
      ' fully met outcomes short of the three required to earn a C- or better.',
      sep = '')

body <- paste('<html>Good afternoon, <br>',
              notification,
              '<br> Being one or two outcomes away from targets is not necessarily cause for concern, as there are several remaining outcomes. 
              However, if you are three or more away from targets, you will need to improve the quality of your work in the remaining weeks of the quarter, and I strongly recommend making a plan now for how to do so. 
              I am happy to help in this regard, and encourage you to come talk with me during office hours if you would like my input. <br><br>
              Trevor',
              sep = '<br>')


email <- gm_mime() |>
  gm_to('truiz01@calpoly.edu') |>
  gm_from("tdruiz001@gmail.com") |>
  # gm_cc("truiz01@calpoly.edu") |>
  gm_subject(paste("STAT218 grade notification (" , notify_names[i], ')', sep = '')) |>
  gm_html_body(body)

gm_send_message(email)

}














