
# Learning statistics backwards by Jae-Young Son

# Exercise 2 (Session 3: Multiple Regression)

## 0. SET UP ----------------------

library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)

## 1. CHANGE DEPENDS ON MANY QUANTITIES ----------

## 1.1 Extracting information about models ----------

penguins %>% 
  glimpse()

# predict body mass by bill depth

lm_bd <- penguins %>% 
  lm(body_mass_g ~ bill_depth_mm, data = .)

tidy(lm_bd) # tidy turns the results of a linear model into a nice tidy tibble  
summary(lm_bd)

# now let#s take a glance at model fit metrics, e.g we  might look at R²

glance(lm_bd) # bill depth accounts for 22% of the variance in body mass


## 1.2 Review ------------------------

# Predicting body mass using flipper length

lm_fl <- penguins %>% 
  lm(body_mass_g ~ flipper_length_mm, data = .)

tidy(lm_fl)
glance(lm_fl)

# Do the same with bill length as predictor

lm_bl <- penguins %>% 
  lm(body_mass_g ~ bill_length_mm, data = .)
tidy(lm_bl)

## 1.3 Shared variance -----------------
lm_fl %>% glance()
lm_bl %>% glance()

# 76% + 35% is 111% so the added explained variance can't be the one that we get when we combine the models. Let's try it and see

lm_combi <- penguins %>% 
  lm(body_mass_g ~ flipper_length_mm + bill_length_mm, data = .)
tidy(lm_combi)
glance(lm_combi)

# Mhh. This is the same as the explained variance of the flipper length model Ó.ò Adding bill length doesn't help at all
# That's the problem with shared variance. When we hold flipper length constant, beak length doesn't significantly predict body size




