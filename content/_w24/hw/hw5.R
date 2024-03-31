## ----libraries--------------------------------------------------------------------------
library(openintro)
library(epitools)


## ----prenatal vitamins and autism-------------------------------------------------------
vitamin <- matrix(data = c(111, 70, 143, 159),
                  nrow = 2, byrow = T,
                  dimnames = list(vitamin = c('no vitamin', 'vitamin'), 
                                  autism = c('autism', 'no autism')))


## ----tea and esophageal carcinoma-------------------------------------------------------
tea <- matrix(data = c(47 - 17, 17, 824 - 283, 283),
              nrow = 2, byrow = T,
              dimnames = list(tea = c('tea', 'no tea'),
                              carcinoma = c('no carcinoma', 'carcinoma')))


## ----malaria vaccine--------------------------------------------------------------------

head(malaria)
