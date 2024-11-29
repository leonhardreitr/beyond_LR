# yey mlm for within stuff

# load some libraries

library(tidyverse)
library(parameters)
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

library(ez)

ezANOVA(data = d_agg, dv = RT, wid = PID, within = modality) |> summary()


d$modality <- ifelse(d$modality == "Audio-only", 0, 1)


mod <- lmer(RT ~ 1 + modality + 
         (1 + modality|PID) + (1 + modality|stim), 
       data = d)



parameters::model_parameters(mod) -> pra
library(see)
plot(pra) +  theme(panel.background = element_blank(),         
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(colour = "black", fill = NA),
                   legend.position = "none",
                   axis.text = element_text(size = 14), 
                   axis.title = element_text(size = 14))
modelbased::estimate_grouplevel(mod)
estimate_prediction(mod)
