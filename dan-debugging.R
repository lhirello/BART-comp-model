## packages
library(readxl)
library(tidyverse)
library(hBayesDM)
library(rstan)
library(tictoc)
library(blastula)

## read in data
bart_df <- read_excel("bart_df.xlsx")

## arrange data
bart_df <- bart_df %>%
  select(-date, -time, -blockcode, -trialnum, -timebefore1stpump, -rotation, -d_n, -totalearningsBART, -ballooncount,   -explosionpoint, -wantedpumps, -pumpresult) %>% #remove unneeded columns
  mutate(subjID = paste0(participant, shift, activity)) %>% #Add in unique subject x trial identifier
  relocate(subjID, .before = participant) %>% #move subjID to the front (personal preference)
  rename(pumps = pumpcount) #rename column as needed to match model files
subset_df <- bart_df[bart_df$subjID %in% unique(bart_df$subjID)[1:30], ]

## compile the model
stan_file <- file.path("models", "asap_bart_ewmv_db.stan")
mod <- stan_model(file = stan_file)

## reorder the dataframe
subset_df <- subset_df %>%
  arrange(subjID)

## get unique subjects
subjects <- unique(subset_df$subjID) 
N <- length(subjects)

## get number of trials per subject:
trials_per_subject <- subset_df %>% 
  group_by(subjID) %>%
  summarise(n_trials = n())
T <- max(trials_per_subject$n_trials)
Tsubj <- trials_per_subject$n_trials

## specify maximum pumps
P <- 128  # max pumps + 1 (127 + 1)

## add trial numbers to data
subset_df <- subset_df %>% # Create trial numbers for each subject assuming rows are ordered by trial within subjID
  group_by(subjID) %>%
  mutate(trial = row_number()) %>%
  ungroup()

## Reshape to wide format for pumps and explosion
pumps <- subset_df %>% 
  select(subjID, trial, pumps) %>%
  pivot_wider(names_from = trial, values_from = pumps) %>%
  select(-subjID) %>%
  as.matrix()
explosion <- subset_df %>%
  select(subjID, trial, explosion) %>%
  pivot_wider(names_from = trial, values_from = explosion) %>%
  select(-subjID) %>%
  as.matrix()

## put all data in one structure
data_list <- list(
  N = N,
  T = T,
  Tsubj = Tsubj,
  P = P,
  pumps = pumps,
  explosion = explosion
)

tic()
output <- sampling( #run the actual models
  object = mod,
  data = data_list,
  chains = 2,
  iter = 1000,
  warmup = 500
  # seed = 123
)
toc()

#get results from model
posterior_samples <- rstan::extract(output)
phi_means    <- apply(posterior_samples$phi,    2, mean)
eta_means    <- apply(posterior_samples$eta,    2, mean)
rho_means    <- apply(posterior_samples$rho,    2, mean)
# tau_means    <- apply(posterior_samples$tau,    2, mean)
lambda_means <- apply(posterior_samples$lambda, 2, mean)
N <- dim(posterior_samples$phi)[2]  # or just hard-code if you know N

param_summary <- data.frame(
  subjID = 1:N,
  phi = phi_means,
  eta = eta_means,
  rho = rho_means,
  # tau = tau_means,
  lambda = lambda_means
)

print(param_summary)


