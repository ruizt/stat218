## ----load data, results='markup'-----------------------------------
# load nhanes dataset
load('data/nhanes.RData')

# extract total cholesterol column
cholesterol <- nhanes$TotChol

# view summary
str(cholesterol)


## ----population summaries, results='markup'------------------------
# numeric summary
summary(cholesterol)

# compute population mean and sd
pop_mean <- mean(cholesterol)
pop_sd <- sd(cholesterol)

# construct histogram
hist(cholesterol, breaks = 30)
abline(v = pop_mean, col = 4, lty = 2)


## ----draw a sample, results = 'hide', fig.show='hide'--------------
# draw a sample -- try running this line a few times
sample(cholesterol, size = 50, replace = F)

# now store a sample
samp <- sample(cholesterol, size = 50)

# calculate mean and sd
samp_mean <- mean(samp)
samp_sd <- sd(samp)
samp_mean
samp_sd

# estimation error
samp_mean - pop_mean
samp_sd - pop_sd

# make a histogram
hist(samp, breaks = 10)

# add lines at sample mean and population mean
abline(v = samp_mean, col = 2) # red line
abline(v = pop_mean, col = 4, lty = 2) # blue line


## ----your turn 1, eval = F-----------------------------------------
## # make vectors of 6 or more means and standard deviations
## samp_means <- ...
## samp_sds <- ...
## 
## # calculate errors
## samp_means - pop_mean
## samp_sds - pop_sd
## 
## # how would you measure variability?
## 


## ----comparing frequency distributions-----------------------------
# population distribution
hist(cholesterol, breaks = 30)

# small sample size (try running a few times)
hist(sample(cholesterol, size = 50), breaks = 15)

# large sample size (try running a few times)
hist(sample(cholesterol, size = 1000), breaks = 20)


## ----simulating sampling distributions, results='markup'-----------
# number of samples to simulate
nsim <- 1000

# this generates nsim sample means from samples of size 10
samp_means_10 <- sapply(1:nsim,
                     function(i){
                       mean(sample(cholesterol, size = 10))
                     })

# repeat, but for samples of size 100
samp_means_100 <- sapply(1:nsim,
                     function(i){
                       mean(sample(cholesterol, size = 100))
                     })


# inspect
str(samp_means_10)
str(samp_means_100)


## ----your turn 2, eval = F-----------------------------------------
## # calculate standard deviation of simulated means
## sim_sd_10 <- ...
## 
## # calculate theoretical standard deviation
## theory_sd_10 <- ...
## 
## # calculate average error
## avg_error_10 <- ...
## 
## # calculate standard deviation of simulated means
## sim_sd_100 <- ...
## 
## # calculate theoretical standard deviation
## theory_sd_100 <- ...
## 
## # calculate average error
## avg_error_100 <- ...


## ----output lab script, eval = F, echo = F, results = 'hide', message = F, warning = F----
## knitr::purl(input = 'lab4-sampling.qmd',
##             output = 'lab4-sampling.R')

