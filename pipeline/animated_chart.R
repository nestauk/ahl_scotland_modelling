rm(list = ls())

library(datasauRus)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(gifski)

theme_set(theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta"))


ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')

library(here)

source(here("pipeline", "extrapolate.R"))

perc = c("0%", "5%", "10%", "20%", "30%", "40%", "50%")

women <- rbind(make_pred_table(scenario_over_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% mutate(state = "overweight"),
               make_pred_table(scenario_obese_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% mutate(state = "obese"),
               make_pred_table(scenario_morb_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% mutate(state = "morbidly obese")) %>% 
  mutate(rel_freq = factor(label_percent()(rel_freq), levels = perc, labels = perc))

png(here("outputs", "figures", "png", "female_gif_split.png"), units = "px", width = 1000, height = 800, res = 100)
women %>% 
  ggplot(., aes(x = rel_freq, y = diff)) + 
  geom_bar(stat = "identity", fill = "#0000ff") +
  ylim(-600,0) +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent(1)(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = as.character(round(diff,0))), vjust= -1 , color = "white") +
  labs(x = "Obesity Prevalence Reduction Compared to 2019 Levels", y = "kcal/day reduction") +
  facet_wrap(state ~ .)
dev.off()


p <- women %>% 
  ggplot(., aes(x = rel_freq, y = diff)) + 
  geom_bar(stat = "identity", fill = "#0000ff") +
  ylim(-600,0) +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = as.character(round(diff,0))), vjust= -1 , color = "white") +
  labs(x = "Obesity Prevalence Reduction Compared to 2019 Levels", y = "kcal/day reduction") +
  ggtitle("Women: {closest_state}") +
  transition_states(state, 3, 1) 

p

anim_save(here("outputs", "figures", "gif", "women.gif"), p)


men <- rbind(make_pred_table(scenario_over_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% mutate(state = "overweight"),
               make_pred_table(scenario_obese_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% mutate(state = "obese"),
               make_pred_table(scenario_morb_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% mutate(state = "morbidly obese")) %>% 
  mutate(rel_freq = factor(label_percent()(rel_freq), levels = perc, labels = perc))

png(here("outputs", "figures", "png", "male_gif_split.png"), units = "px", width = 1000, height = 800, res = 100)
men %>% 
  ggplot(., aes(x = rel_freq, y = diff)) + 
  geom_bar(stat = "identity", fill = "#0000ff") +
  ylim(-600,0) +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = as.character(round(diff,0))), vjust= -1 , color = "white") +
  labs(x = "Obesity Prevalence Reduction Compared to 2019 Levels", y = "kcal/day reduction") +
  facet_wrap(state ~ .)
dev.off()

p <- men %>% 
  ggplot(., aes(x = rel_freq, y = diff)) + 
  geom_bar(stat = "identity", fill = "#0000ff") +
  ylim(-600,0) +
  geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
  geom_text(aes(x = as.factor(rel_freq), label = as.character(round(diff,0))), vjust= -1 , color = "white") +
  labs(x = "Obesity Prevalence Reduction Compared to 2019 Levels", y = "kcal/day reduction") +
  ggtitle("Men: {closest_state}") +
  transition_states(state, 3, 1) 

p

anim_save(here("outputs", "figures", "gif", "men.gif"), p)
