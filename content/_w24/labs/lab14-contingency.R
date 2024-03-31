## ----load packages-------------------------------------------------------------
library(tidyverse)
library(oibiostat)
library(epitools)
library(openintro)
library(Sleuth3)
library(pander)


## ----obesity data--------------------------------------------------------------
obesity <- case1801 |> column_to_rownames('Obesity') |> as.matrix() |> as.table()
obesity |> pander()


## ----estimating risk ratio, echo = T-------------------------------------------
riskratio(obesity, rev = 'both')


## ----smoking data--------------------------------------------------------------
smoking <- case1803 |> column_to_rownames('Smoking') |> as.matrix() |> as.table() |> t()
smoking |> pander()


## ----estimating odds ratio-----------------------------------------------------
oddsratio(smoking, rev = 'both', method = 'wald')


## ----practice problem 1--------------------------------------------------------
cyclo <- matrix(data = c(21, 4, 9, 56), 
               nrow = 2, 
               byrow = T, 
               dimnames = list(raspberries = c('yes', 'no'),
                               cyclosporiasis = c('yes', 'no'))
                )

cyclo |> pander()


## ----practice problem 2--------------------------------------------------------
chd <- matrix(data = c(84, 2916, 87, 4913), 
               nrow = 2, 
               byrow = T, 
               dimnames = list(smoker = c('yes', 'no'),
                               chd = c('yes', 'no'))
                )

chd |> pander()


## ----practice problem 3--------------------------------------------------------
case1901 |> pander()


## ----practice problem 4--------------------------------------------------------
data("LEAP")
leap <- xtabs(~ overall.V60.outcome + treatment.group, data = LEAP)
leap |> pander()


## ----practice problem 5--------------------------------------------------------
mammogram <- matrix(data = c(500, 44425, 505, 44405), 
               nrow = 2, 
               byrow = T, 
               dimnames = list(mammogram = c('yes', 'no'),
                               cancer.death = c('yes', 'no'))
                )
mammogram |> pander()


## ----practice problem 6--------------------------------------------------------
diabetes <- avandia |> table()
diabetes |> pander()


## ----practice problem 7--------------------------------------------------------
vaccine <- biontech_adolescents |> table()
vaccine |> pander()


## ----practice problem 8--------------------------------------------------------
asthma <- matrix(data = c(49, 781, 30, 769), 
               nrow = 2, 
               byrow = T, 
               dimnames = list(sex = c('female', 'male'),
                               asthma = c('yes', 'no'))
                )
asthma |> pander()

