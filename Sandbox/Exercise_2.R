
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

penguins %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, color = bill_length_mm)) +
  geom_point() +
  scale_colour_viridis_b(option = "plasma")

# When bill length would predict body mass over and above flipper length, for any given flipper length we should see first purple, then pink then yellow
# dots. This is not the case. 

## 1.4 Prediction and hypothesis testing ----------------

# In science reality, we should only test predictions when we have an hypothesis. E.g. in the example, we might have hypothesized that beak
# length influences body weight b/c birds with bigger beaks can catch more food. This turns out to not be true when controlling for the
# confounding impact of body size, represented by flipper length. Do not hypothesize after the results are known (HARKing)!

## 1.5 Interpreting model estimates ---------------------

# Build a model with bill length and depth as predictors

lm_bd_bl <- penguins %>% 
  lm(body_mass_g ~ bill_depth_mm + bill_length_mm, data = .)

# Compare it with the simple models:

glance(lm_bd_bl)
glance(lm_bd)
glance(lm_bl)
# Bill length and depth don't seem to share too much varince, b/c the combined model explains more variance than the single ones

lm_bd_bl %>%  tidy()

## 2. EXERCISES ---------------------------------------------------------------------------------------

covid_intervention <- here("Data", "covid_intervention.csv") %>%
  read_csv() %>%
  mutate(keep = if_else(sub %% 2 == 0, "threat", "prosocial")) %>%
  filter(keep == intervention) %>%
  select(sub, willingness, valence, arousal, extraversion = bfi_extraversion, neuroticism = bfi_neuroticism)

## Exercise 1 -------------

# Hypothesis: the more negative and activated a person feels, the mroeo willing they are to self isolate

lm_affect <- covid_intervention %>% 
  lm(willingness ~ valence + arousal, data = .)

lm_affect %>%  tidy()
# both factors are significant. Higher valence (more positive) and higher arousal are associated with more willingness to isolate. A person
# with 0 arousal and maximally negative mood has on average a 91.8% willingness to isolate.
lm_affect %>% glance()
# That's also why the model explains only 3% of the variance

lm_arousal <- covid_intervention %>% 
  lm(willingness ~ arousal, data = .)
lm_arousal %>% glance()
lm_arousal %>% tidy()

lm_valence <- covid_intervention %>% 
  lm(willingness ~ valence, data = .)
lm_valence %>% glance()
lm_valence %>% tidy()

covid_intervention %>% 
  ggplot(aes(x = arousal, y = willingness, color = valence)) +
  geom_point()

# Valence really doesn't explain much. Also, the effect goes into the opposite direction than the hypothesis. Better mood is associated
# with (very slightly) more willingness to self-isolate. In the plot we can see that we have a strong ceiling effect.

## Exercise 2 ------------

# How do neuroticism and extraversion influence willingness to isolate?
# Hypotheses: The more extraverted a person, the less willing and the more neurotic, the more willing to self-isolate.

lm_extra <- covid_intervention %>% 
  lm(willingness ~ extraversion, data = .)
lm_neuro <- covid_intervention %>% 
  lm(willingness  ~ neuroticism, data = .)
lm_pers <- covid_intervention %>% 
  lm(willingness ~ neuroticism + extraversion, data = .)

glance(lm_extra)
glance(lm_neuro)
glance(lm_pers)

tidy(lm_extra)
tidy(lm_neuro)
tidy(lm_pers)

# both are insignificant, and the effects go in the opposite direction than I expectet, oupsi

covid_intervention %>% 
  ggplot(aes(x = extraversion, y = willingness, color = neuroticism)) +
  geom_point()

covid_intervention %>% 
  ggplot(aes(x = neuroticism, y = willingness, color = extraversion)) +
  geom_point()



