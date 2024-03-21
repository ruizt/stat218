## ----eval = T------------------------------------------------
library(tidyverse)
library(Sleuth3)
library(oibiostat)
library(openintro)
library(pander)


## ----eval = T------------------------------------------------
test.out <- diabetes2 |> table() |> chisq.test()
test.out$residuals |> pander(caption = 'Chi-square test residuals')


## ----problem 1-----------------------------------------------
head(ex1220)


## ----problem 2-----------------------------------------------
head(ex0817)


## ----problem 3-----------------------------------------------
prop.alzheimer <- 16/144
n <- 144


## ----problem 4-----------------------------------------------
head(migraine)


## ----problem 5-----------------------------------------------
diabetes <- avandia |> table()
diabetes 


## ----problem 6-----------------------------------------------
head(census)


## ----problem 7-----------------------------------------------
asthma <- matrix(data = c(49, 781, 30, 769), 
               nrow = 2, 
               byrow = T, 
               dimnames = list(sex = c('female', 'male'),
                               asthma = c('yes', 'no'))
                )
asthma 


## ----problem 8-----------------------------------------------
head(babies_crawl)


## ----problem 10----------------------------------------------
salt <- matrix(data = c(15, 170, 6, 190), 
               nrow = 2, byrow = T,
               dimnames = list(salt = c('high', 'low'),
                               cause = c('cvd', 'other')))

