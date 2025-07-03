rm(list = ls())

library(tidyverse)
library(cmdstanr)
library(rutils)
library(ggrepel)
library(grid)
library(gridExtra)
library(furrr)
library(loo)

utils_loc <- c("R/utils/plotting-utils.R", "R/utils/utils.R")
walk(utils_loc, source)

if (!dir.exists("data/")) dir.create("data/")
if (!dir.exists("data/infpro_task-cat_beh/")) dir.create("data/infpro_task-cat_beh/")
if (!dir.exists("data/infpro_task-cat_beh/models/")) dir.create("data/infpro_task-cat_beh/models/")
if (!dir.exists("data/infpro_task-cat_beh/model-plots/")) dir.create("data/infpro_task-cat_beh/model-plots/")
if (!dir.exists("data/infpro_task-cat_beh/figures/")) dir.create("data/infpro_task-cat_beh/figures/")


# Load Data and Preprocess Them -------------------------------------------

file_loc_train <- "data/infpro_task-cat_beh/infpro_task-cat_beh.csv"
file_loc_transfer <- "data/infpro_task-cat_beh/infpro_task-cat2_beh.csv"
# tbl_train <- read_csv(file_loc_train, show_col_types = FALSE)
# tbl_transfer <- read_csv(file_loc_transfer, show_col_types = FALSE)
tbl_train <- read_csv(file_loc_train)
tbl_transfer <- read_csv(file_loc_transfer)
colnames(tbl_transfer) <- str_replace(colnames(tbl_transfer), "cat2", "cat")
tbl_train$session <- "train"
tbl_transfer$session <- "transfer"

# re-coding category and response due to ordering constraints in the Bayesian models
tbl_both <- tbl_train  %>% 
  rbind(tbl_transfer) %>%
  mutate(
    d1i_z = scale(d1i)[, 1],
    d2i_z = scale(d2i)[, 1],
    category = recode_factor(category, "B" = "C", "C" = "B"),
    response = recode_factor(response, "B" = "C", "C" = "B"),
    category_int = as.numeric(factor(category, levels = c("A", "B", "C"), ordered = TRUE)),
    response_int = as.numeric(factor(response, levels = c("A", "B", "C"), ordered = TRUE)),
  )

# keep these summary stats for plotting results in untransformed space
mean_d1i <- mean(tbl_both$d1i)
sd_d1i <- sd(tbl_both$d1i)
mean_d2i <- mean(tbl_both$d2i)
sd_d2i <- sd(tbl_both$d2i)


tbl_train <- tbl_both %>% filter(session == "train")
tbl_transfer <- tbl_both %>% filter(session == "transfer")

if (!is_saved) {
  
  # save train, transfer, and combined data as rds and csv
  saveRDS(tbl_both, file = "data/infpro_task-cat_beh/tbl_both.RDS")
  saveRDS(tbl_train, file = "data/infpro_task-cat_beh/tbl_train.RDS")
  saveRDS(tbl_transfer, file = "data/infpro_task-cat_beh/tbl_transfer.RDS")
  
  write_csv(tbl_train, file = "data/infpro_task-cat_beh/tbl_train.csv")
  write_csv(tbl_transfer, file = "data/infpro_task-cat_beh/tbl_transfer.csv")
  write_csv(tbl_both, file = "data/infpro_task-cat_beh/tbl_both.csv")
}


tbl_stim_id <- tbl_train %>% count(d1i, d2i, d1i_z, d2i_z, category) %>%
  arrange(d1i, d2i) %>% mutate(stim_id = seq_along(d1i + d2i)) %>%
  dplyr::select(-n)
tbl_stim_id_transfer <- tbl_transfer %>% count(d1i, d2i, d1i_z, d2i_z, category) %>%
  arrange(d1i, d2i) %>% mutate(stim_id = seq_along(d1i + d2i)) %>%
  dplyr::select(-n)
tbl_train <- tbl_train %>% 
  left_join(tbl_stim_id, by = c("d1i", "d2i", "d1i_z", "d2i_z", "category")) %>%
  relocate(stim_id, .before = d1i)
tbl_transfer <- tbl_transfer %>%
  left_join(tbl_stim_id_transfer, by = c("d1i", "d2i", "d1i_z", "d2i_z", "category")) %>%
  relocate(stim_id, .before = d1i)

# define how many trials starting from the last trial should be analyzed
n_last_trials <- 500

tbl_train_last <- tbl_train %>% group_by(participant) %>%
  mutate(
    rwn_fwd = row_number(block),
    rwn_bkwd = row_number(desc(rwn_fwd))
  ) %>% ungroup() %>%
  filter(rwn_bkwd <= n_last_trials) %>%
  dplyr::select(-c(rwn_fwd, rwn_bkwd))

tbl_both <- rbind(tbl_train_last, tbl_transfer)

# Plot Overall Proportion Responses By Stimulus and Category --------------

# only correct responses
pl_train <- plot_average_categorization_accuracy(tbl_train_last, "Train")
pl_tf <- plot_average_categorization_accuracy(tbl_transfer, "Transfer")
marrangeGrob(list(pl_train, pl_tf), ncol = 2, nrow = 1)

# Aggregate table with length = participants*categories*stimIDs
tbl_train_agg <- aggregate_by_stimulus_and_response(tbl_stim_id, tbl_train_last)
tbl_transfer_agg <- aggregate_by_stimulus_and_response(tbl_stim_id_transfer, tbl_transfer)
tbl_train_agg_overall <- tbl_train_agg %>%
  group_by(d1i, d2i, d1i_z, d2i_z, stim_id, category, response) %>%
  summarize(
    n_responses = sum(n_responses),
    n_trials = sum(n_trials)
  ) %>%
  mutate(prop_responses = n_responses / n_trials)

tbl_transfer_agg_overall <- tbl_transfer_agg %>%
  group_by(d1i, d2i, d1i_z, d2i_z, stim_id, category, response) %>%
  summarize(
    n_responses = sum(n_responses),
    n_trials = sum(n_trials)
  ) %>%
  mutate(prop_responses = n_responses / n_trials)

# all responses
participant_sample <- "Average of All"
plot_proportion_responses(
  tbl_train_agg_overall %>% 
    mutate(response = str_c("Response = ", response)) %>%
    filter(prop_responses > .025),
  participant_sample,
  facet_by_response = TRUE
)
plot_proportion_responses(
  tbl_transfer_agg_overall %>% 
    mutate(response = str_c("Response = ", response)) %>%
    filter(prop_responses > .025),
  participant_sample,
  facet_by_response = TRUE
)

tbl_train_agg$response_int <- as.numeric(factor(
  tbl_train_agg$response, levels = c("A", "B", "C"), ordered = TRUE
))
tbl_transfer_agg$response_int <- as.numeric(factor(
  tbl_transfer_agg$response, levels = c("A", "B", "C"), ordered = TRUE
))
tbl_train_agg$category_int <- as.numeric(factor(
  tbl_train_agg$category, levels = c("A", "B", "C"), ordered = TRUE
))
tbl_transfer_agg$category_int <- as.numeric(factor(
  tbl_transfer_agg$category, levels = c("A", "B", "C"), ordered = TRUE
))


# General settings --------------------------------------------------------

# mcmc settings for all models
l_stan_params <- list(
  n_samples = 1000,
  n_warmup = 250,
  n_chains = 3
)


# GCM ---------------------------------------------------------------------

tbl_both_agg <- rbind(tbl_train_agg, tbl_transfer_agg)
l_tbl_both_agg <- split(tbl_both_agg, tbl_both_agg$participant)

stan_gcm <- write_gcm_stan_file_predict()
mod_gcm <- cmdstan_model(stan_gcm)
safe_gcm <- safely(bayesian_gcm)

n_workers_available <- parallel::detectCores()
plan(multisession, workers = n_workers_available - 2)

options(warn = -1)
l_gcm <- furrr::future_map(
  l_tbl_both_agg, safe_gcm, 
  l_stan_params = l_stan_params, 
  mod_gcm = mod_gcm, 
  .progress = TRUE
)
options(warn = 0)
plan("sequential")
saveRDS(l_loo_gcm, file = "data/infpro_task-cat_beh/gcm-loos.RDS")


# Prototype: Multivariate Gaussian ---------------------------------------

l_tbl_both <- split(tbl_both, tbl_both$participant)

stan_gaussian <- write_gaussian_naive_bayes_stan()
mod_gaussian <- cmdstan_model(stan_gaussian)
safe_gaussian <- safely(bayesian_gaussian_naive_bayes)

n_workers_available <- parallel::detectCores()
plan(multisession, workers = n_workers_available - 2)

l_loo_gaussian <- furrr::future_map2(
  l_tbl_both, l_tbl_both_agg, safe_gaussian, 
  l_stan_params = l_stan_params,
  mod_gaussian = mod_gaussian, 
  .progress = TRUE
)
plan("sequential")
saveRDS(l_loo_gaussian, file = "data/infpro_task-cat_beh/gaussian-loos.RDS")


