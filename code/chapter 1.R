# Packages required for Chapter 1
library(knitr) 
library(gridExtra)
library(GGally)
library(kableExtra)
library(jtools)
library(rsample)
library(broom)
library(tidyverse)    

# but also
library(easystats)
library(brms)

theme_set(theme_apa())

d <- read_csv("data/derbyplus.csv")

glimpse(d)

report(d) |> kable(format = "pipe")
model1 <- lm(speed ~ year, data = d)

model_parameters(model1)

ggplot(d, aes(year, speed)) +
  geom_smooth(method = "lm", se = F, col = "black",
              lty = 4) + 
  geom_point()
