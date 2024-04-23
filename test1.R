## ----packages and data--------------------------------------------
library(tidyverse)


## ----yrbss data---------------------------------------------------
data(yrbss, package = 'oibiostat')

yrbss <- yrbss |>
  transmute(age = age,
            sex = factor(gender),
            grade = factor(grade, levels = as.character(9:12), ordered = T),
            sleep.hours = factor(school.night.hours.sleep,
                                 levels = c('<5', as.character(6:9), '10+'), ordered = T),
            exercise.days = physically.active.7d) |>
  drop_na()

save(yrbss, file = 'data/yrbss.RData')


## ----yrbss problem, eval = F, echo = F----------------------------
## # part b: load and inspect data
## load('data/yrbss.RData')
## head(yrbss)
## 
## # part d: inspect distributions of age and grade level
## hist(yrbss$age, breaks = 5, right = F)
## yrbss$grade |> table() |> barplot()
## 
## # part e: distribution of hours of sleep on school nights
## yrbss$sleep.hours |> table() |> barplot()
## 
## # part f: hours of sleep by grade level
## table(yrbss$sleep.hours, yrbss$grade) |>
##   proportions(margin = 2) |>
##   barplot(legend = T)
## 
## # part g: distribution of days of exercise per week
## hist(yrbss$exercise.days, breaks = 6)


## ----longevity data-----------------------------------------------
data("case0501", package = 'Sleuth3')

longevity <- case0501 |> 
  filter(str_sub(Diet, 1, 1) == 'N') |>
  mutate(Diet = factor(Diet, levels = c('NP', 'N/N85', 'N/R50', 'N/R40'))) |>
  rename_with(tolower)

save(longevity, file = 'data/longevity.RData')


## ----longevity and diet problem, eval = F, echo = F---------------
## # load data and inspect
## load('data/longevity.RData')
## head(longevity)
## 
## # part b: average lifetime by diet
## longevity |>
##   group_by(diet) |>
##   summarize(avg.lifetime = mean(lifetime))
## 
## # part c: standard deviation of lifetimes by group
## longevity |>
##   group_by(diet) |>
##   summarize(sd.lifetime = sd(lifetime))
## 
## # part d: graphic to compare lifetimes by diet
## boxplot(lifetime ~ diet, data = longevity)


## ----mammal data--------------------------------------------------
mammals <- MASS::mammals |>
  rownames_to_column(var = 'mammal') |>
  mutate(log.body = log(body),
         log.brain = log(brain),
         bb.ratio = brain/body,
         mammal = tolower(mammal))

save(mammals, file = 'data/mammals.RData')


## ----brain and body size probleml, eval = F-----------------------
## # load and inspect data
## load('data/mammals.RData')
## head(mammals)
## 
## # part a: plot of log brain size (y) against log body size (x)
## plot(mammals$log.brain, mammals$log.body)
## 
## # part b: correlation of log brain size and log body size
## cor(mammals$log.brain, mammals$log.body)
## 
## # part c: histogram of brain weights
## hist(mammals$brain, breaks = 25)
## 
## # part d: measure of center and spread for brain weights
## median(mammals$brain)
## IQR(mammals$brain)
## 
## # part e: largest brain size? (inspect directly with view(...))
## view(mammals)
## mammals |> slice_max(brain) |> pull(mammal)
## 
## # part f: largest brain:body ratio?
## mammals |> slice_min(bb.ratio) |> pull(mammal)
## mammals |> slice_max(bb.ratio) |> pull(mammal)


## ----baden simulated data-----------------------------------------
moderna <- expand.grid(group = c('placebo', 'vaccine'),
            illness = c('no', 'yes')) |>
  bind_cols(reps = c(15210 - 185, 15210 - 11, 185, 11)) |>
  mutate(drop = map(reps, ~rep(1, .x))) |>
  unnest(drop) |>
  select(1:2)

save(moderna, file = 'data/moderna.RData')


## ----moderna vaccine, eval = F------------------------------------
## # load data and inspect
## load('data/moderna.RData')
## head(moderna)
## 
## # part e: contingency table (proportion infected by group)
## tbl <- table(moderna) |> proportions(margin = 1)
## tbl
## 
## # part f: relative risk and efficacy
## tbl[2, 2]/tbl[1, 2]
## 1 - tbl[2, 2]/tbl[1, 2]

