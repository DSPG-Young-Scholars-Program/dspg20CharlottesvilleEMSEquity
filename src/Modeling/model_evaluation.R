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
library(sf)
library(broom)
library(purrr)
library(glue)
library(stringr)
library(ggthemes)

prepared_data <- readr::read_csv(here::here("data", "final", "response_time_model_data_prepared.csv"))
prepared_data_neighborhoods <- sf::st_read(here::here("data", "final", "response_time_model_data_prepared_sp.geojson"))


freq_models <- list("basic_model_freq_no_interact" = basic_model_freq_no_interact,
                    "basic_model_freq_yes_interact" = basic_model_freq_yes_interact)

bayes_models <- stanreg_list("basic_model_bayes_no_interact" = basic_model_bayes_no_interact,
                             "basic_model_bayes_yes_interact" = basic_model_bayes_yes_interact,
                             "neighbor_model_bayes_no_interact" = neighbor_model_bayes_no_interact,
                             "neighbor_model_bayes_yes_interact" = neighbor_model_bayes_yes_interact)



######################################################################################
# Examine Coefficients
######################################################################################

freq_model_coefs <- map(freq_models, tidy,
                        conf.int = TRUE,
                        conf.level = 0.95,
                        exponentiate = FALSE)

bayes_model_coefs <- map(bayes_models, ~tidy(.x$stanfit,
                                             estimate.method = "median",
                                             conf.int = TRUE,
                                             conf.level = 0.95)) %>%
  map(~filter(.x, !(term %in% c("sigma", "mean_PPD", "log-posterior"))))


freq_model_coefs_trans <- freq_model_coefs %>%
  map(~mutate(.x, across(c(estimate,
                           conf.low,
                           conf.high),
                         list(scale_factor = ~exp(.x * (.x != .x[1])), # scales to exponential, expression insures intercept is right
                              time_to_incident = ~exp(.x * (.x != .x[1]) + .x[1]))))) # scales to exponential, expression insures intercept is right

bayes_model_coefs_trans <- bayes_model_coefs %>%
  map(~mutate(.x, across(c(estimate,
                           conf.low,
                           conf.high),
                         list(scale_factor = ~exp(.x * (.x != .x[1])),
                              time_to_incident = ~exp(.x * (.x != .x[1]) + .x[1])))))


plot_scale_factor <- function(data) {
  ggplot(data, aes(y = term)) +
    geom_point(aes(x = estimate_scale_factor)) +
    geom_errorbar(aes(xmin = conf.low_scale_factor, xmax = conf.high_scale_factor)) +
    theme_minimal() +
    labs(x = NULL, y = glue("Scale Factor from {round(data$estimate_time_to_incident[1],
                                                      digits = 1)} Minutes")) +
    coord_cartesian(xlim = c(0.25, 1.75)) +
    geom_vline(xintercept = 1, alpha = 0.5)
}

plot_time_to_incident <- function(data) {
  ggplot(data) +
    geom_point(aes(x = term, y = estimate_time_to_incident)) +
    geom_errorbar(aes(x = term, ymin = conf.low_time_to_incident, ymax = conf.high_time_to_incident)) +
    theme_minimal() +
    coord_flip() +
    labs(x = NULL, y = glue("Travel Time in Minutes"))
}


map(bayes_model_coefs_trans,
    plot_scale_factor)

map(bayes_model_coefs_trans,
    plot_time_to_incident)

map(freq_model_coefs_trans,
    plot_scale_factor)

map(freq_model_coefs_trans,
    plot_time_to_incident)


######################################################################################
# Lets look at how estimates change when an interaction term is added
######################################################################################

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



bayes_model_coefs_trans[[2]] %>%
  mutate(after_covid = str_detect(term, "(after_covidTRUE.*)")) %>%
  mutate(term = str_replace(term, "after_covidTRUE:", "")) %>%
  ggplot(aes(y = term, color = after_covid)) +
  geom_point(aes(x = estimate_scale_factor)) +
  geom_errorbar(aes(xmin = conf.low_scale_factor, xmax = conf.high_scale_factor)) +
  theme_minimal() +
  scale_color_manual(values = cbbPalette) +
  coord_cartesian(xlim = c(0.5, 1.5)) +
  geom_vline(xintercept = 1, alpha = 0.5) +
  labs(y = NULL, x = glue("Scale Factor from {round(bayes_model_coefs_trans[[2]]$estimate_time_to_incident[1],
                                                      digits = 1)} Minutes"),
       color = "During Covid")

bayes_model_coefs_trans[[4]] %>%
  mutate(after_covid = str_detect(term, "(after_covidTRUE.*)")) %>%
  mutate(term = str_replace(term, "after_covidTRUE:", "")) %>%
  ggplot(aes(y = term, color = after_covid)) +
  geom_point(aes(x = estimate_scale_factor)) +
  geom_errorbar(aes(xmin = conf.low_scale_factor, xmax = conf.high_scale_factor)) +
  theme_minimal() +
  scale_color_manual(values = cbbPalette) +
  coord_cartesian(xlim = c(0.5, 1.5)) +
  geom_vline(xintercept = 1, alpha = 0.5) +
  labs(y = NULL, x = glue("Scale Factor from {round(bayes_model_coefs_trans[[4]]$estimate_time_to_incident[1],
                                                      digits = 1)} Minutes"),
       color = "During Covid")

######################################################################################
# Examine More Techinal Aspects.
# The rest of the analysis will look only at the models fit using Bayesian estimation
# Because the estimates are almost identical to the frequentist estimates
######################################################################################


loo_list <- map(bayes_models, loo)
loo_compare(loo_list)

# Looks like the neighborhood models are far better predictors of out of sample data, but there is no significant differnece between including covid or not.


map(bayes_models, pp_check)


