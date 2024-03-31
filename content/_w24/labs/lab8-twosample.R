## ----load datasets----------------------------------------------------------------------------------------------------------------------------
library(oibiostat)
library(Sleuth3)
data(swim)
data(famuss)
data(sleep)
finch <- case0201
diets <- ex0112
cloud <- case0301


## ----paired data------------------------------------------------------------------------------------------------------------------------------
# step 1: compute difference wetsuit - swimsuit
velocity.diff <- swim$wet.suit.velocity - swim$swim.suit.velocity

# step 2: perform one-sample t test
t.test(velocity.diff, mu = 0, alternative = 'greater')


## ----your turn 1, eval = F--------------------------------------------------------------------------------------------------------------------
## # inspect data
## head(famuss)
## 
## # compute difference ndrm.ch - drm.ch
## strength.diff <- ...
## 
## # perform t test
## t.test(...)


## ----independent data-------------------------------------------------------------------------------------------------------------------------
# examine data; note that grouping is shown as a variable
head(finch)

# perform t test
t.test(Depth ~ Year, data = finch, mu = 0, alternative = 'less')


## ----eval = F---------------------------------------------------------------------------------------------------------------------------------
## # inspect data
## head(diets)
## 
## # perform test
## t.test(..., data = ..., mu = ..., alternative = ...)


## ----cloud seeding data-----------------------------------------------------------------------------------------------------------------------
# inspect data
head(cloud)


## ----sleep data-------------------------------------------------------------------------------------------------------------------------------
# inspect data after ordering by participant ID
dplyr::arrange(sleep, ID) |> head()


## ----your turn 2, eval = F--------------------------------------------------------------------------------------------------------------------
## # test whether cloud seeding increases rainfall
## 
## # test whether sleep drugs produce different effects
## 

