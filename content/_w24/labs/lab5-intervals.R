## ----load data--------------------------------------------------------------------------------------------------------------------------------------------
# load dataset
load('data/nhanes.RData')

# extract cholesterol variable
cholesterol <- nhanes$TotChol


## ----draw a sample and calculate an interval--------------------------------------------------------------------------------------------------------------
# fix sample size
n <- 35

# draw a sample of size 35
my_samp <- sample(cholesterol, size = n)

# calculate mean and standard error
xbar <- mean(my_samp)
xbar.se <- sd(my_samp)/sqrt(n)

# calculate the interval
xbar.interval <- xbar + c(-1, 1)*2*xbar.se

# display
xbar.interval


## ----your turn 1------------------------------------------------------------------------------------------------------------------------------------------
# does the interval contain the population mean?



## ----simulating deviations--------------------------------------------------------------------------------------------------------------------------------
# fix sample size 
n <- 5

# function to simulate scaled deviation of one sample mean
sim_dev <- function(n, data){
  samp <- sample(data, size = n)
  xbar <- mean(samp)
  xbar.se <- sd(samp)/sqrt(n)
  xbar.sd <- sd(data)/sqrt(n)
  mu <- mean(data)
  dev <- c(se = (xbar - mu)/xbar.se, 
           sd = (xbar - mu)/xbar.sd)
  return(dev)
}

# repeat many simulations
nsim <- 1000
sim.devs <- sapply(1:nsim, function(i){sim_dev(n, data = cholesterol)}) |> 
  t() |> 
  as.data.frame()

# how many are between -2 and 2 using SD?
coverage.sd <- sum(abs(sim.devs$sd) < 2)/nsim
coverage.sd

# using SE?
coverage.se <- sum(abs(sim.devs$se) < 2)/nsim
coverage.se


## ----calculating intervals--------------------------------------------------------------------------------------------------------------------------------
# fix sample size and desired coverage
n <- 10
coverage <- 0.95

# draw one sample
samp <- sample(cholesterol, size = n)

# interval ingredients
xbar <- mean(samp)
xbar.se <- sd(samp)/sqrt(n)
c.val <- qt((1 - coverage)/2, df = n - 1, lower.tail = F)

# calculate interval
xbar + c(-1, 1)*c.val*xbar.se


## ----your turn 3------------------------------------------------------------------------------------------------------------------------------------------
# adjust coverage and calculate a new critical value

# calculate the interval


