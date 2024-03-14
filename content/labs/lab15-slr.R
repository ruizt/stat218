## ----load packages----------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(oibiostat)
library(openintro)
library(Sleuth3)
load('data/prevend.RData')


## ----prevend data, fig.width=4, fig.height=3--------------------------------------------------------------------------------------
# scatterplot
plot(prevend)


## ----your turn 1, echo = T--------------------------------------------------------------------------------------------------------
hand.fit(b0 = 80, b1 = -0.2)


## ----your turn 2, echo = T--------------------------------------------------------------------------------------------------------
hand.fit(b0 = 80, b1 = -0.2, .resid = T)


## ----fitting an SLR model, echo = T-----------------------------------------------------------------------------------------------
# fit the model
fit <- lm(RFFT ~ Age, data = prevend)

# inspect output
fit


## ----parameter estimates, echo = T------------------------------------------------------------------------------------------------
# retrieve coefficients
coef(fit)

# retrieve estimate of error SD
sigma(fit)


## ----model visualization, echo = T------------------------------------------------------------------------------------------------
# scatterplot
plot(prevend)

# add line
abline(a = coef(fit)[1], b = coef(fit)[2])


## ----fitted values and residuals, echo = T----------------------------------------------------------------------------------------
# fitted values
fit.fitted <- fit$fitted.values

# residuals
fit.resid <- fit$residuals

# residual vs fit
plot(fit.resid, fit.fitted)
abline(h = 0)

# residual histogram
hist(fit.resid)

# quantile-quantile plot
qqnorm(fit.resid)
qqline(fit.resid)


## ----inferences, echo = T---------------------------------------------------------------------------------------------------------
# model summary
summary(fit)

# confidence intervals
confint(fit, level = 0.95)


## ----age of the universe, echo = T------------------------------------------------------------------------------------------------
# load data
load('data/hubble.RData')

# make a plot

# fit model

# residual diagnostics

# CI for inverse of hubble constant

# conversion
c <- 978440076094


