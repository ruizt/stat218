## ----------------------------------------------------------------------------------------------------------------------------------------------
cancer <- bind_cols(year = ex0323$Year,
                    sunspot.activity = ex0323$SunspotActivity,
                    cancer.rate = ex0323$CancerRate,
                    delta = lm(CancerRate ~ Year, data = ex0323)$resid)
# save(cancer, file = 'data/cancer.RData')

# a
plot(cancer$year, cancer$cancer.rate, type = 'b')

# b
t.test(delta ~ sunspot.activity, data = cancer)


## ----------------------------------------------------------------------------------------------------------------------------------------------
plants <- Sleuth3::ex0428
# save(plants, file =  'data/plants.RData')

# a
t.test(Pair(Cross, Self) ~ 1, data = plants)$conf

# b
t.test(Pair(Cross, Self) ~ 1, data = plants, alternative = 'greater')


## ----------------------------------------------------------------------------------------------------------------------------------------------
tubercle <- ex0211
# save(tubercle, file = 'data/tubercle.RData')

# a. 
t.test(Lifetime ~ Group, data = tubercle, mu = 0, alternative = 'less')

# b.
int <- t.test(Lifetime ~ Group, data = tubercle, mu = 0, alternative = 'two.sided', conf.level = 0.99)$conf

# c.
int/t.test(Lifetime ~ Group, data = tubercle, mu = 0, alternative = 'two.sided')$estimate[2]



## ----------------------------------------------------------------------------------------------------------------------------------------------
marijuana <- Sleuth3::ex0432
# save(marijuana, file = 'data/marijuana.RData')

# a
t.test(Pair(Marijuana, Placebo) ~ 1, data = marijuana, alternative = 'less')


## ----------------------------------------------------------------------------------------------------------------------------------------------
# a
power.t.test(delta = 40, sd = 94, power = 0.9)$n

# c
power.t.test(power = 0.9, sd = 94, n = 40)$delta

