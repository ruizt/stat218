library(tidyverse)
library(readxl)
library(gmailr)
library(xtable)
filepath <- '~/OneDrive - Cal Poly/stat218-s24-submissions/'
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
  correction_path <- paste(filepath, assignment_name, ' Corrections.xlsx', sep = '')
  
  if(!file.exists(correction_path)){
    out <- paste(filepath, assignment_name, '.xlsx', sep = '') |>
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
    original <- paste(filepath, assignment_name, '.xlsx', sep = '') |>
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
    
    revised <- paste(filepath, assignment_name,  ' Corrections.xlsx', sep = '') |>
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
names <- c(paste('PS', 1:13, sep = ''), paste('Test', 1:4, sep = ''), 'Test1 Extension')

# optional questions
optional_qnums <- list(
  ps3 = c(23),
  ps11 = c(14, 23:27),
  ps13 = c(8),
  test1 = c(38, 39),
  test3 = c(32, 33, 34, 35),
  test4 = c(22, 23, 38:42)
)

# manual questions
manual_qnums <- list(
  ps1 = 1:8,
  test1 = c(2, 3, 7, 9, 10, 12, 17, 18, 19, 20, 23, 24, 25, 26, 28, 40, 41),
  test2 = c(1, 4, 8, 15, 18, 22, 23, 36, 38, 47, 48, 51, 53, 57),
  test3 = c(9, 12, 19, 23, 24, 30, 31, 35)
)

# correction credit (autograded, manual)
correction_weights <- c(0.5, 1)

# read in assignments
scores_long_raw <- lapply(names, function(.x){
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
  mutate(optional = as.logical(optional),
         outcome = str_trunc(outcome, 3, ellipsis = '') |> str_remove_all(']')) |>
  filter(str_starts(outcome, 'l')) 

dropped <- scores_long_raw |>
  group_by(name, email) |>
  summarize(prop.na = mean(is.na(score))) |>
  arrange(desc(prop.na)) |>
  filter(prop.na > 0.9) |>
  pull(name)

scores_long <- scores_long_raw |>
  filter(!(name %in% dropped)) |>
  mutate(score = replace_na(score, 0)) |>
  mutate(score = if_else(optional == T,
                         na_if(score, 0),
                         score))  

# weighted learning outcome scores
outcomes.weighted <- scores_long |>
  filter(!(name %in% dropped)) |>
  mutate(score = replace_na(score, 0)) |>
  mutate(score = if_else(optional == T,
                         na_if(score, 0),
                         score),
         category = str_trunc(assignment, width = 1, side = 'right', ellipsis = '')) |>
  group_by(name, email, outcome, category) |>
  summarize(score = mean(score, na.rm = T),
            n.questions = n()) |>
  left_join(tibble(category = c('t', 'p'), weight = c(0.7, 0.3)), by = 'category') |>
  mutate(weighted.score = score*weight) |>
  group_by(name, email, outcome) |>
  summarize(score = sum(weighted.score)) |>
  mutate(current.status = cut(score,
                              breaks = c(0, 0.5, 0.7799, 1.1),
                              right = F,
                              labels = c('not met', 'partly met', 'fully met'))) |>
  mutate(outcome = toupper(outcome)) |>
  filter(str_starts(outcome, 'L'))

# unweighted learning outcome scores
outcomes.unweighted <- scores_long |>
  group_by(name, email, outcome) |>
  summarize(score = mean(score, na.rm = T)) |>
  mutate(current.status = cut(score,
                              breaks = c(0, 0.5, 0.8, 1.1),
                              right = F,
                              labels = c('not met', 'partly met', 'fully met'))) |>
  mutate(outcome = toupper(outcome)) |>
  ungroup()

grade.levels <- expand.grid(c('+', '', '-'), toupper(letters[1:4])) |> 
  select(Var2, Var1) |>
  slice(-1) |>
  unite(grade, sep = '') |>
  pull(grade) |>
  rev()

# import project scores and assign grades
grades.weighted <- paste(filepath, 'Project.xlsx', sep = '') |>
  read_xlsx() |>
  select(starts_with('Email'), Assessment) |>
  pivot_longer(-Assessment, names_to = 'drop', values_to = 'email') |>
  rename(current.status = Assessment) |>
  select(email, current.status) |>
  drop_na() |>
  mutate(outcome = 'L11',
         current.status = tolower(current.status) |> 
           factor(levels = levels(outcomes.weighted$current.status)),
         score = NA) |>
  left_join(distinct(outcomes.weighted, name, email), by = c('email')) |>
  bind_rows(select(outcomes.weighted, email, current.status, outcome, score, name)) |>
  select(name, email, outcome, score, current.status) |>
  group_by(name, email, current.status) |>
  count() |>
  spread(current.status, n) |>
  rename_with(~gsub(' ', '.', .x)) |>
  mutate(across(ends_with('met'), ~replace_na(.x, 0))) |>
  mutate(tally = if_else(partly.met + fully.met < 6, -1, floor(fully.met + partly.met/3)),
         grade = cut(tally, breaks = c(-1:10, 13), right = F, labels = c('F', grade.levels))) |>
  ungroup() 

grades.unweighted <- paste(filepath, 'Project.xlsx', sep = '') |>
  read_xlsx() |>
  select(starts_with('Email'), Assessment) |>
  pivot_longer(-Assessment, names_to = 'drop', values_to = 'email') |>
  rename(current.status = Assessment) |>
  select(email, current.status) |>
  drop_na() |>
  mutate(outcome = 'L11',
         current.status = tolower(current.status) |> 
           factor(levels = levels(outcomes.unweighted$current.status)),
         score = NA) |>
  left_join(distinct(outcomes.unweighted, name, email), by = c('email')) |>
  bind_rows(select(outcomes.unweighted, email, current.status, outcome, score, name)) |>
  select(name, email, outcome, score, current.status) |>
  group_by(name, email, current.status) |>
  count() |>
  spread(current.status, n) |>
  rename_with(~gsub(' ', '.', .x)) |>
  mutate(across(ends_with('met'), ~replace_na(.x, 0))) |>
  mutate(tally = if_else(partly.met + fully.met < 6, -1, floor(fully.met + partly.met/3)),
         grade = cut(tally, breaks = c(-1:10, 13), right = F, labels = c('F', grade.levels))) |>
  ungroup() 


full_join(grades.weighted, grades.unweighted, by = c('name', 'email'), suffix = c('.w', '.uw')) |>
  select(name, email, grade.w, grade.uw) |>
  mutate(delta = as.numeric(grade.w) - as.numeric(grade.uw)) |>
  filter(grade.w != grade.uw) |>
  arrange(delta) |>
  print(n = 100)

# inspect
grades.weighted |>
  arrange(grade) |>
  view()

# distribution
grades.weighted |>
  mutate(grade = str_trunc(grade, 1, ellipsis = '')) |>
  count(grade)


# export

grades.export <- grades.weighted |>
  select(email, grade) |>
  left_join(read_csv('_zz-s24/grades-points.csv')) |>
  rename(`SIS Login ID` = email,
         `Final Grade` = grade) 

read_csv('_zz-s24/canvas-download-sect05.csv') |>
  left_join(grades.export, by = 'SIS Login ID') |>
  # write_csv('_zz-s24/canvas-upload-sect05.csv')
  select(Student, `Final Grade`) |>
  view()

read_csv('_zz-s24/canvas-download-sect06.csv') |>
  left_join(grades.export, by = 'SIS Login ID') |>
  # write_csv('_zz-s24/canvas-upload-sect06.csv')
  select(Student, `Final Grade`) |>
  view()


# traditional grading (for comparison)
scores_long |>
  group_by(name, email, assignment) |>
  summarize(score = mean(score, na.rm = T)) |>
  mutate(category = str_trunc(assignment, 2, ellipsis = '')) |>
  group_by(name, email, category) |>
  summarize(score = mean(score)) |>
  left_join(tibble(category = c('ps', 'te'), weight = c(0.3, 0.7))) |>
  mutate(weighted.score = score*weight) |>
  summarize(total = sum(weighted.score)) |>
  arrange(desc(total)) |>
  mutate(grade.trad = cut(total, breaks = c(0, 0.6, 0.7, 0.8, 0.9, 1), labels = c('F', 'D', 'C', 'B', 'A'))) |>
  ungroup() |>
  left_join(grades.weighted, by = c('name', 'email')) |>
  select(name, email, starts_with('grade')) |>
  mutate(grade.outcome = str_trunc(grade, 1, ellipsis = '') |> fct_drop()) |>
  select(-grade) |>
  filter(as.numeric(grade.trad) > as.numeric(grade.outcome)) |>
  print(n = 100)
  

## FOR CHECKING INDIVIDUAL RECORDS

outcomes.weighted |> filter(name == 'Avery Hughes')
scores_long_raw |> filter(name == 'Abigail Blair') |>
  group_by(assignment) |>
  summarize(score = mean(score, na.rm = T))
scores_long |> filter(name == 'XX') |>
  group_by(outcome, assignment) |>
  summarize(score = mean(score, na.rm = T)) |>
  spread(outcome, score)

## FOR INSTRUCTOR SUMMARIES

# distribution of questions across assignments
scores_long |>
  distinct(assignment, question.number, outcome) |>
  group_by(assignment, outcome) |>
  count() |>
  spread(outcome, n) |>
  ungroup()

scores_long |>
  group_by(outcome, assignment) |>
  summarize(score = mean(score, na.rm = T)) |>
  spread(outcome, score) 

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

outcome.summary <- paste(filepath, 'Project.xlsx', sep = '') |>
  read_xlsx() |>
  select(starts_with('Email'), Assessment) |>
  pivot_longer(-Assessment, names_to = 'drop', values_to = 'email') |>
  rename(current.status = Assessment) |>
  select(email, current.status) |>
  drop_na() |>
  mutate(outcome = 'L11',
         current.status = tolower(current.status) |> 
           factor(levels = levels(outcomes.weighted$current.status)),
         score = NA) |>
  left_join(distinct(outcomes.weighted, name, email), by = c('email')) |>
  bind_rows(select(outcomes.weighted, email, current.status, outcome, score, name)) |>
  select(name, email, outcome, score, current.status) |>
  arrange(name, outcome) |>
  mutate(outcome = factor(outcome, levels = paste('L', c(1:11, 'X'), sep = '')))

outcome.summary

students <- outcome.summary |> distinct(name, email)
student_names <- pull(students, name)
student_emails <- pull(students, email)

notify_set <- setdiff(1:length(student_names), c(2, 5, 8, 52))

for(i in notify_set){
  
  tbl1 <- filter(outcome.summary, name == student_names[i]) |> 
    ungroup() |>
    select(name, outcome, score, current.status) |> 
    arrange(as.numeric(outcome)) |>
    xtable() |> 
    print(type = 'html', include.rownames = F)
  
  # tbl2 <- filter(scores_long, name == student_names[i], assignment == 'test2') |>
  #   select(name, question.number, outcome, optional, score) |>
  #   # mutate(optional = na_if(optional, F)) |>
  #   select(-optional) |>
  #   xtable() |> 
  #   print(type = 'html', include.rownames = F)
  
  body <- paste("<html>Good afternoon, <br><br> The table below provides a final estimate of your scores by learning outcome; your final grade for the course will be determined based on these scores. <br><br> This summary accounts for all assignments, and scores are weighted to allocate more importance to tests. In all but a few instances, this benefits the class. Additionally, the 'fully met' threshold was lowered slightly to 0.78. <br><br>",
                tbl1, 
                "Regarding assignment of letter grades, I've decided to count every three partly met outcomes as the equivalent of one fully met outcome; this is a (beneficial) change from the syllabus, but otherwise, the assignment of letter grades is exactly as specified. <br><br> 
                Please also be advised that I may adjust letter grades by one level (e.g., B to either B+ or B-) at my discretion consistent with course policies (e.g., attendance). I'm letting you know this in the interest of transparency, but ask that you not attempt to negotiate grade increases. You can expect to see grades posted by Tuesday of next week.",
                "<br> It's been a pleasure working with you this quarter, and I hope you'll feel free to be in touch if I can be helpful to you in any way in the future. Best wishes for the summer ahead. <br><br> Trevor <br>",
                sep = '<br>')
  
  email <- gm_mime() |>
    gm_to(student_emails[i]) |>
    gm_from("tdruiz001@gmail.com") |>
    gm_cc("truiz01@calpoly.edu") |>
    gm_subject(paste("STAT218 current score estimates (" , student_names[i], ')', sep = '')) |>
    gm_html_body(body)
  
  gm_send_message(email)
}

## EMAIL NOTIFICATIONS

notify_names <- notify_list$name
notify_emails <- notify_list$email

for(i in 3:length(notify_names)){

notification <- paste('Tentative estimates indicate that you are ', 
      notify_list$n.short.passing[i], 
      ' partly or fully met outcomes short of the six required to pass and, provided you meet the passing requirement, ',
      notify_list$n.short.c[i],
      ' fully met outcomes short of the three required to earn a C- or better.',
      sep = '')

body <- paste('<html>Good afternoon, <br>',
              notification,
              '<br> Being one or two outcomes away from targets is not necessarily cause for concern, as there are several remaining outcomes, and current estimates are based on only seven outcomes.<br><br>
              However, if you are three or more away from targets, you will need to improve the quality of your work in the remaining weeks of the quarter, and I strongly recommend making a plan now for how to do so.<br><br>
              I am happy to help in this regard, and encourage you to come talk with me during office hours if you would like my input or wish to discuss your work in the class further; you can schedule a time at https://calendly.com/tdruiz/office-hour. <br><br>
              Trevor',
              sep = '<br>')


email <- gm_mime() |>
  gm_to(notify_emails[i]) |>
  # gm_to('truiz01@calpoly.edu') |>
  gm_from("tdruiz001@gmail.com") |>
  gm_cc("truiz01@calpoly.edu") |>
  gm_subject(paste("STAT218 grade notification (" , notify_names[i], ')', sep = '')) |>
  gm_html_body(body)

gm_send_message(email)

}














