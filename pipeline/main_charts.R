rm(list = ls())

library(here)

source(here("pipeline", "extrapolate.R"))

# plots

png(here("outputs", "figures", "png", "overweight.png"), units = "px", width = 1000, height = 600, res = 100)
grid.arrange(
  make_pred_table(scenario_over_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Overweight Female", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  make_pred_table(scenario_over_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Overweight Male", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "obese.png"), units = "px", width = 1000, height = 600, res = 100)
grid.arrange(
  make_pred_table(scenario_obese_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Obese Female", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  make_pred_table(scenario_obese_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Obese Male", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "morbidly_obese.png"), units = "px", width = 1000, height = 600, res = 100)
grid.arrange(
  make_pred_table(scenario_morb_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Morbidly Obese Female", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  make_pred_table(scenario_morb_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Morbidly Obese Male", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "excess_weight.png"), units = "px", width = 1000, height = 600, res = 100)
grid.arrange(
  make_pred_table(scenario_excess_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Excess Weight Female", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  make_pred_table(scenario_excess_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Excess Weight Male", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  nrow = 1)
dev.off()

png(here("outputs", "figures", "png", "population.png"), units = "px", width = 1000, height = 600, res = 100)
grid.arrange(
  make_pred_table(scenario_pop_female, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Population Female", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  make_pred_table(scenario_pop_male, thresholds = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)) %>% 
    ggplot(., aes(x = as.factor(rel_freq), y = diff)) + 
    geom_bar(stat = "identity", fill = "#0000ff") +
    ylim(-600,0) +
    geom_text(aes(x = as.factor(rel_freq), label = label_percent()(pp)), vjust= 1.5 ) +
    geom_text(aes(x = as.factor(rel_freq), label = round(diff,0)), vjust= -1 , color = "white") +
    labs(title = "Population Male", x = "Obesity Prevalence Reduction", y = "kcal/day reduction"),
  nrow = 1)
dev.off()



# example plots

plot_df <- merge(scenario_obese_male, year_trend, by.x = "year_model", by.y = "year_c")

plot_df %>% 
  ggplot(., aes(x = rel_freq, y = intake_end)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  ggtitle("Example: obese men")

mod <- lm(intake_end ~ rel_freq, plot_df)

extra <- data.frame(rel_freq = seq(0, 0.5, 0.01))

pred <- data.frame(pred = predict(mod, extra), extra) 

pred %>% 
  ggplot(., aes(x = rel_freq, y = pred)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") +
  ggtitle("Example: obese men")