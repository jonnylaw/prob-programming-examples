library(tidyverse)
library(coda)
library(ggmcmc)
library(ggthemes)
library(patchwork)

source("scripts/mcmc_summarise.R")
theme_set(theme_tufte(base_size = 12))

######
# LM #
######

sims = read_csv("data/lm_sims.csv")

GGally::ggpairs(sims)
ggsave("lm_pairs.pdf")

actual_values = tibble(
    Parameter = c("alpha", "beta", "sigma"),
    actual_value = c(4.0, -1.5, 0.5))

files = c("data/lm_1.csv",
          "data/lm_2.csv")
chains = read_chains(files, param_names = T, drop = 0, nth = 1)

plot_diagnostics_sim(chains, actual_values)
ggsave("ehmc_lm.pdf")

# Empirical distribution of Ls

els = tibble(
    l = 1:17,
    count = c(177,544,544,7,29,57,104,32,62,98,91,43,58,75,42,30,7))

ggplot(els, aes(x = l, weight = count)) +
    geom_bar()

ggsave("empirical_dist_lm.pdf")

#################
# Mixture Model #
#################

actual_values_mix = tibble(
    Parameter = c("theta1", "theta2", "theta3", "mu1", "mu2", "mu3", "sigma"),
    actual_value = c(0.5, 0.2, 0.3, 3, -2, 1, 0.5))

sims = read_csv("data/mixture_model.csv", col_names = "y")

ggplot(sims, aes(x = y)) +
    geom_histogram()

ggsave("mixture_model.pdf")

files = c("data/mixture_1.csv",
          "data/mixture_2.csv")
chains = read_chains(files, param_names = T, drop = 0, nth = 1)

plot_diagnostics_sim(chains %>% filter(Parameter != "sigma"), actual_values_mix)

latex_table_sim(chains, actual_values_mix)

chains %>%
    inner_join(actual_values_mix) %>%
    traceplot() +
    geom_hline(aes(yintercept = actual_value), linetype = "dashed")

ggsave("mixture_model_posterior.pdf")

##################
# Random Effects #
##################

sims = read_csv("data/random_effects_sims.csv")

ggplot(sims, aes(x = x, y = y, colour = as.factor(id))) +
    geom_line() +
    theme(legend.position = "none")

actual_values_re = tibble(
    Parameter = c("alphaC", "betaC", "sigmaA", "sigmaB", "sigma"),
    actual_value = c(3.0, 2.5, 1.0, 0.5, 0.5))

files = c("data/random_effects_1.csv",
          "data/random_effects_2.csv")
chains = read_chains(files, nth = 1, drop = 0, param_names = T)

latex_table_sim(chains, actual_values_re)

plot_diagnostics_sim(chains, actual_values_re)
ggsave("random_effects_diagnostics.pdf")
