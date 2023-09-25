library(tidyverse)

set.seed(93401)
boxing <- tibble(sport = "boxing",
                 won = c(rep("red", 148), 
                         rep("blue", 124)
                         )
)

set.seed(93401)
others <- tibble(sport = sample(c("boxing",
                                  "tae kwon do", 
                                  "Greco-Roman wrestling", 
                                  "freestyle wrestling"
                                  ), 
                                185, 
                                replace = T
                                ),
                 won = c(rep("red", 100),
                         rep("blue", 85)
                         )
                 
)

set.seed(93401)
athletes <- boxing |> 
  bind_rows(others) |> 
  mutate(match = sample(1:457,457), .before = sport) |> 
  arrange(match) |> 
  select(-match)

write.csv(athletes, "04-Rlabs/02-Rlab-one-categorical/data/athletes.csv", row.names = F)


272 boxing matches 148 of them were won by competitors wearing red.  0.551 

data collected on the results of 457 matches and found that the competitor wearing red won 248 times, whereas the competitor wearing blue won 209 times. 0.543.