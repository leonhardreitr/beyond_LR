# yey mlm for within stuff

# load some libraries

library(tidyverse)
library(modelbased)
library(lme4)
library(afex)

theme_set(
  theme_bw() +
    theme(
      panel.grid = element_blank()
    )
)

# load data

osf_url <- "https://osf.io/53wc4/?action=download"
d <- 
read_csv(osf_url)

glimpse(d)

d_agg <- 
  d |> 
  summarise(
    RT = mean(RT),
    .by = c(PID, modality)
  )

d$modality <- ifelse(d$modality == "Audio-only", 0, 1)


mod <- lmer(
  
)