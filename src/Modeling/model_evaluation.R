load(here::here("data", "working", "model_objects", "basic_model_bayes_no_interact.RData"))
load(here::here("data", "working", "model_objects", "basic_model_bayes_yes_interact.RData"))
load(here::here("data", "working", "model_objects", "basic_model_freq_no_interact.RData"))
load(here::here("data", "working", "model_objects", "basic_model_freq_yes_interact.RData"))
load(here::here("data", "working", "model_objects", "neighbor_model_bayes_no_interact.RData"))
load(here::here("data", "working", "model_objects", "neighbor_model_bayes_yes_interact.RData"))

library(rstanarm)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(broom)


######################################################################################
# Examine Coefficients
######################################################################################

bayes_coefs_no <- basic_model_bayes_no_interact$coefficients
bayes_coefs_yes <- basic_model_bayes_yes_interact$coefficients


freq_coefs_no <- basic_model_freq_no_interact$coefficients
freq_coefs_yes <- basic_model_freq_yes_interact$coefficients

bayes_credint_no <- posterior_interval(basic_model_bayes_no_interact, prob = 0.95)
bayes_credint_yes <- posterior_interval(basic_model_bayes_yes_interact, prob = 0.95)


freq_confint_no <- confint(basic_model_freq_no_interact)
freq_confint_yes <- confint(basic_model_freq_yes_interact)

#################### no interaction

freq_no_table <- tibble(variable = rownames(freq_confint_no),
                        estimate = freq_coefs_no,
                        lower_2_5 = freq_confint_no[,1],
                        upper_9_5 = freq_confint_no[,2],
                        method = "freq")

bayes_no_table <- tibble(variable = rownames(bayes_credint_no[1:23,]),
                        estimate = bayes_coefs_no,
                        lower_2_5 = bayes_credint_no[1:23,1],
                        upper_9_5 = bayes_credint_no[1:23,2],
                        method = "bayes")


no_inter_models <- bind_rows(bayes_no_table,
                             freq_no_table)

transform_to_minutes <- function(x, method) {
  if_else(method == "bayes",
          exp((x != bayes_coefs_no[1] & x != bayes_credint_no[1,1] & x != bayes_credint_no[1,2]) * bayes_coefs_no[1] + x),
          exp((x != freq_coefs_no[1] &  x != freq_confint_no[1,1] & x != freq_confint_no[1,2]) * freq_coefs_no[1] + x))
}

transform_to_mult <- function(x) {
  if_else(x == freq_coefs_no[1] | x == bayes_coefs_no[1] |
          x == freq_confint_no[1,1] | x == freq_confint_no[1,2] |
          x == bayes_credint_no[1,1] | x == bayes_credint_no[1,2],
          1,
          exp(x))
}


scaled_estimates_no <- no_inter_models %>%
  mutate(estimate_trans = transform_to_minutes(estimate, method),
         estimate_scale = transform_to_mult(estimate),
         lower_2_5_trans = transform_to_minutes(lower_2_5, method),
         lower_2_5_scale = transform_to_mult(lower_2_5),
         upper_9_5_trans = transform_to_minutes(upper_9_5, method),
         upper_9_5_scale = transform_to_mult(upper_9_5))



ggplot(scaled_estimates_no) +
  geom_point(aes(x = variable, y = estimate_trans)) +
  geom_errorbar(aes(x = variable, ymin = lower_2_5_trans, ymax = upper_9_5_trans)) +
  theme_minimal() +
  coord_flip() +
  labs(x = NULL, y = "Minutes")

ggplot(scaled_estimates_no) +
  geom_point(aes(x = variable, y = estimate_scale)) +
  geom_errorbar(aes(x = variable, ymin = lower_2_5_scale, ymax = upper_9_5_scale)) +
  theme_minimal() +
  coord_flip() +
  labs(x = NULL, y = "Times 9.5 Minutes")

################# with interactions:

freq_yes_table <- tibble(variable = rownames(freq_confint_yes),
                        estimate = freq_coefs_yes,
                        lower_2_5 = freq_confint_yes[,1],
                        upper_9_5 = freq_confint_yes[,2],
                        method = "freq")

bayes_yes_table <- tibble(variable = rownames(bayes_credint_yes[1:44,]),
                         estimate = bayes_coefs_yes,
                         lower_2_5 = bayes_credint_yes[1:44,1],
                         upper_9_5 = bayes_credint_yes[1:44,2],
                         method = "bayes")


yes_inter_models <- bind_rows(bayes_yes_table,
                             freq_yes_table)

transform_to_minutes <- function(x, method) {
  if_else(method == "bayes",
          exp((x != bayes_coefs_yes[1] & x != bayes_credint_yes[1,1] & x != bayes_credint_yes[1,2]) * bayes_coefs_yes[1] + x),
          exp((x != freq_coefs_yes[1] &  x != freq_confint_yes[1,1] & x != freq_confint_yes[1,2]) * freq_coefs_yes[1] + x))
}

transform_to_mult <- function(x) {
  if_else(x == freq_coefs_yes[1] | x == bayes_coefs_yes[1] |
            x == freq_confint_yes[1,1] | x == freq_confint_yes[1,2] |
            x == bayes_credint_yes[1,1] | x == bayes_credint_yes[1,2],
          1,
          exp(x))
}


scaled_estimates_yes <- yes_inter_models %>%
  mutate(estimate_trans = transform_to_minutes(estimate, method),
         estimate_scale = transform_to_mult(estimate),
         lower_2_5_trans = transform_to_minutes(lower_2_5, method),
         lower_2_5_scale = transform_to_mult(lower_2_5),
         upper_9_5_trans = transform_to_minutes(upper_9_5, method),
         upper_9_5_scale = transform_to_mult(upper_9_5))



ggplot(scaled_estimates_yes) +
  geom_point(aes(x = variable, y = estimate_trans)) +
  geom_errorbar(aes(x = variable, ymin = lower_2_5_trans, ymax = upper_9_5_trans)) +
  theme_minimal() +
  coord_flip() +
  labs(x = NULL, y = "Minutes")

ggplot(scaled_estimates_yes) +
  geom_point(aes(x = variable, y = estimate_scale)) +
  geom_errorbar(aes(x = variable, ymin = lower_2_5_scale, ymax = upper_9_5_scale)) +
  theme_minimal() +
  coord_flip() +
  labs(x = NULL, y = "Times 9.5 Minutes")




######################################################################################
# Examine Residuals
######################################################################################


pp_check(basic_model_bayes_no_interact)
pp_check(neighbor_model_bayes_no_interact)
