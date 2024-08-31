#### fun with infer
#### The idea behind infer is great and something I did not know before
#### In this file I play around with it a bit

#### In this script I will only focus on ANOVA and multiple regression, since they are most common in my own work

#### The idea of infer is simple: There is only one test.

# It works like this
# Given your observed data, calculate a test statistic theta, e.g. mean difference, proportion, etc.
# Then define the Null hypothesis in the form of a stochastic distribution
# Then generate samples from aforementioned distribution
# Then put theta in this null world and check how it fits
# Then calculate the p-value that theta could exist in this null world
# Then decide if it is statistically significant

library(tidymodels)
library(tidyverse)

theme_set(
  theme_bw() +
    theme(panel.grid.minor = element_blank())
)

d <- gss

glimpse(d)

# is there a difference between age and political party affiliation ?

ggplot(
  d,
  aes(
    x = partyid,
    y = age
  )
) + 
  geom_boxplot(aes(fill = partyid)) +
  scale_fill_manual(values = paletteer::paletteer_d("Redmonder::dPBIPuOr"))


d |> 
  summarise(
    mean_age = mean(age),
    sd_age = sd(age),
    .by = partyid
  )

# There seems to be some variation in means, but honestly not much

f_test <- 
  d |> 
  specify(
    age ~ partyid
  ) |> 
    hypothesise(
      null = "independence"
    ) |> 
    calculate(stat = "F")

# so now we got our test statistic

# next we simulate a Null world

null_world <- 
  d |> 
  specify(
    age ~ partyid
  ) |> 
  hypothesise(
    null = "independence"
  ) |> 
  generate(reps = 1000, type = "permute") |> 
  calculate(stat = "F")

null_world |> visualise(bins = 15, method = "simulation") +
  shade_p_value(f_test,
                direction = "greater")
# calculate the p value from the observed statistic and null distribution
p_val <- null_world |> 
  get_p_value(obs_stat = f_test,
              direction = "greater")

p_val

# also get effect size 
effectsize::f_to_eta2(f = f_test$stat)

# Something I am missing from this work flow are effect sizes. 