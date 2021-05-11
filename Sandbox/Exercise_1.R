
# Learning statistics backwards by Jae-Young Son

# Exercise 1 (Session 2: Simple Regression)

## 0. SET UP ----------------------

#install.packages("palmerpenguins")
library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)

## 1. CHANGE DEPENDS ON QUANTITIES -------------------

# Let's take a look at the data. Does flipper length predict body mass? We start with a plot:

penguins
penguins %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# Now let's test the hypothesis. We use a linear model. Unfortunately, lm() doesn't go well with the tidyverse, since the first input argument is not
# the data but the formular. We need to tell R that the data from the pipe goes somewhere else by using the "lazy dot operator" . 
# For the formula, we use Wilkinson notation, where we use ~ instead of =, b/c = already has a meaning in code

penguins %>% 
  lm(body_mass_g ~ flipper_length_mm,
     data = .) %>% 
  tidy()
# Result interpretation: estimate for flipper length is 49.7, saying that for every mm increase in flipper length, the weight of a penguin increases by 
# 49.7g. The p-value is nearly 0, indicating that it is highly unlikely that the null hypothesis is true. the intercept tells us that the hypothetical 
# body mass of a penguin with flipper length = 0 would be -5781.


# Are penguins with bigger bills (Schnabel) heavier?

# plot
penguins %>% 
  ggplot(aes(x = bill_length_mm, y = body_mass_g)) +
  geom_point()

# model
penguins %>% 
  lm(body_mass_g ~ bill_length_mm, data = .) %>% 
  tidy()
# Result: Yes, bill length predicts body mass. On average, a penguin with a 1mm longer bill is 87.4g heavier. A penguin with no bill would weigh 362g ;)

# What is the expected weight of a penguin with a 40mm bill?
362 + 40*87.4
# The expected weight of this penguin would be 3858g

## 2. CHANGE DEPENDS ON CATEGORIES ---------------

# Is the average weight of Adelie and Gentoo penguins different?

penguins %>% 
  filter(species %in% c("Adelie", "Gentoo")) %>% 
  group_by(species) %>% 
  ggplot(aes(x = species, y = body_mass_g )) +
  geom_bar(stat = "summary", fun = mean)

penguins %>% 
  filter(species %in% c("Adelie", "Gentoo")) %>% 
  lm(body_mass_g ~ species, data = .) %>% 
  tidy()
# The intercept is now the average weight of an Adelie penguin. Gentoo penguins are 1375g heavier on average


# Let’s try testing two hypotheses using the same regression: that on average, both Chinstrap and Gentoo penguins
# have a different body mass than Adelie penguins. Plot the data. Run this analysis. Interpret the results.

penguins %>% 
  group_by(species) %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_bar(stat = "summary", fun = mean)

penguins %>% 
  lm(body_mass_g ~ species, data = .) %>% 
  tidy()
# Chinstrap penguins are on average only 32.4g heavier than Adelie penguins which is not significant with a 0.63 probability to be a random difference.
# Gentoo penguins are significantly heavier than Adelie penguins by an average of 1375g

# Change refernce category:

# Chinstrap as reference category
penguins %>% 
  mutate(species = fct_relevel(species, levels = c("Chinstrap", "Adelie", "Gentoo"))) %>% 
  lm(body_mass_g ~ species, data = .) %>% 
  tidy()
# results look similar, estimate is now negative (b/c Adelie is lighter)

# Gentoo as reference category
penguins %>% 
  mutate(species = fct_relevel(species, levels = c("Gentoo", "Chinstrap", "Adelie"))) %>% 
  lm(body_mass_g ~ species, data = .) %>% 
  tidy()
# Now both other terms are sigificant. By reordering, we can find out which levels differ significantly from which others (=reparameterizing a model)


## 3. OTHER TESTS -----------------------------------

# Mathematically, other tests like correlations or t-tests work in the same way as the regression, using an underlying linear model

## 3.1 Correlation --------------------------------

# Correlation between flipper length and body mass:
with(penguins, cor.test(flipper_length_mm, body_mass_g)) %>% 
  tidy()

# Correlations work on standardized data --> they have no units. We can destandardize a correlation to turn it into a regression slope:
# Regression Slope = Correlation-Coefficient * (SD_y/SD_x)

with(penguins, cor.test(flipper_length_mm, body_mass_g)) %>% 
  tidy() %>% 
  select(estimate) %>% 
  mutate(estimate = estimate * with(penguins, sd(body_mass_g, na.rm = TRUE) / sd(flipper_length_mm, na.rm = TRUE)))

# compare this with our regression:
penguins %>% 
  lm(body_mass_g ~ flipper_length_mm, data = .) %>% 
  tidy()

# As we see, the estimate is the same as the slope that we got by destandardizing our correlation, and the p-value for the regression is
# identical to the one from the correlation

## 3.2 T-test -------------------------------------

# test wheter body mass differs between Adelie and Gentoo penguins
with(penguins %>% filter(species %in% c("Adelie", "Gentoo")),
     t.test(body_mass_g ~ species, var.equal = TRUE)) %>% 
  tidy()

# compare this to our regression:
penguins %>% 
  filter(species %in% c("Adelie", "Gentoo")) %>% 
  lm(body_mass_g ~ species, data = .) %>% 
  tidy()
# Besides the sign flip, that's the same!


## 4. EXERCISES ----------------------------------------------------------------------------------------

# The dataset contains data on willigness to self-isolate in the early days of the pandemic. Participants either received a threatening
# or prosocial message (the virus is coming for you vs. you can help others by staying home)

# Variables: intervention (threat, prosocial), willingness (0=not willing, 100=extremely willing), change (0=no change, 100=complete change),
# valence(emotions after reading the message, higher numbers = more positive), arousal (higher numbers = more activated)

covid_intervention <- here("Data", "covid_intervention.csv") %>% 
  read_csv() %>% 
  mutate(keep = if_else(sub %% 2 == 0, "threat", "prosocial")) %>% # %% gives the rest of a division --> weise geraden subjects threat zu, ungeraden prosocial
  filter(keep == intervention) %>% # behalte nur die zeilen wo keep mit intervention überein stimmt (threat für gerade, prosocial für ungerade subjects)
  select(sub, intervention, willingness, change, valence, arousal)

## 1. Form a research question: Are threats or prosocial messages more effective in changing willigness to self isolate?
## 2. Plot data to visualize your analysis

covid_intervention %>% 
  ggplot(aes(x = intervention, y = willingness)) +
  geom_bar(stat ="summary", fun = mean)

## 3. Using linear regression, test the hypothesis

covid_intervention %>% 
  lm(willingness ~ intervention, data = .) %>% 
  tidy()
# oh no, this was wrong. willingness was the baseline measure before the intervention
# Now the same with change:

covid_intervention %>% 
  ggplot(aes(x = intervention, y = change)) +
  geom_bar(stat = "summary", fun = mean)

covid_intervention %>% 
  lm(change ~ intervention, data = .) %>% 
  tidy()
# still not significant

## 4. Interpret the results and explain them to your english major friend: 
# On average, people who received the prosocial message changed their willingness to isolate by 1.33 point more than people in the threat
# group, who on average changed their willingness by 24.8. This is no significant effect of the intervention, b/c with a probability of 
# 0.6 this difference was random

## Optionally, use a correlation or a t-test to see if the results match

with(covid_intervention,
     t.test(change ~ intervention, var.equal = TRUE)) %>% 
  tidy()
# No significant difference

# What about the effect on mood?

# Now the same with change:

covid_intervention %>% 
  ggplot(aes(x = intervention, y = valence)) +
  geom_bar(stat = "summary", fun = mean)

covid_intervention %>% 
  lm(valence ~ intervention, data = .) %>% 
  tidy()
# oh boy, there is some difference!

# Now let's look at arousal:

covid_intervention %>% 
  ggplot(aes(x = intervention, y = arousal)) +
  geom_bar(stat = "summary", fun = mean)

covid_intervention %>% 
  lm(arousal ~ intervention, data = .) %>% 
  tidy()
# also significant


# Now let's look at the real data
library(lme4)
library(lmerTest)

covid_intervention %>% 
  group_by(sub, intervention) %>% 
  ggplot(aes(x = intervention, y = change, color = sub, group = sub)) +
  #geom_bar(stat = "summary", fun = mean) +
  geom_line(stat = "identity")

covid_intervention %>% 
  lmer(change ~ intervention + (1|sub), data = .) %>% 
  summary()

# hm yeah whatever

