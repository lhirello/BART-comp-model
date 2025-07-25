## packages
library(readxl)
library(cmdstanr)
library(tictoc)
library(tidyverse)

## read in data
bart_df <- read_excel("bart_df.xlsx")

## set the cmdstan path
if (Sys.info()["login"] == "DBENNETT1"){
  set_cmdstan_path("/Users/dbennett1/.cmdstan/cmdstan-2.35.0")
}

## arrange data
bart_df <- bart_df %>%
  select(-date, -time, -blockcode, -trialnum, -timebefore1stpump, -rotation, -d_n, -totalearningsBART, -ballooncount,   -explosionpoint, -wantedpumps, -pumpresult) %>% #remove unneeded columns
  # select(-date, -time, -blockcode, -trialnum, -timebefore1stpump, -rotation, -d_n, -totalearningsBART, -ballooncount,   -explosion, -wantedpumps, -pumpresult) %>% #remove unneeded columns
  mutate(subjID = paste0(participant, shift, activity)) %>% #Add in unique subject x trial identifier
  relocate(subjID, .before = participant) %>% #move subjID to the front (personal preference)
  rename(pumps = pumpcount) #rename column as needed to match model files
# subset_df <- bart_df[bart_df$subjID %in% unique(bart_df$subjID)[1:20], ]

## pre-compile model
file.remove(file.path(getwd(), "models", "bart_par4"))
compiled_model <- cmdstan_model(file.path(getwd(), "models", "bart_par4.stan"), force_recompile = F)

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

## sample from model
tic()
fit <- compiled_model$sample(
  data            = data_list,
  # chains          = 1,
  # parallel_chains = 1,
  # refresh         = 10,
  # iter_warmup     = 100,
  # iter_sampling   = 125,
  chains          = 4,
  parallel_chains = 4,
  refresh         = 100,
  iter_warmup     = 1000,
  iter_sampling   = 1250,
  # adapt_delta     = 0.9,
  save_warmup     = FALSE
)
toc()

## extract parameter samples (individual-level)
indiv_pars <- c(
  "phi", 
  "eta", 
  # "rho", 
  "tau",
  # "lambda",
  "gamma"
  )

indiv_par_samples_all <- read_cmdstan_csv(
  files=fit$output_files(),
  variables = indiv_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)
indiv_par_samples <- vector(mode="list", length=length(indiv_pars))
indiv_par_est <- matrix(NA, nrow= indiv_par_samples_all$metadata$stan_variable_sizes[[indiv_pars[1]]], ncol=length(indiv_pars))
colnames(indiv_par_est) <- indiv_pars
for (i in 1:length(indiv_par_samples)){
  indiv_par_samples[[i]] <- as.matrix(indiv_par_samples_all$post_warmup_draws[seq(
    from       = 1 + (i-1) * dim(indiv_par_est)[1],
    to         = i * dim(indiv_par_est)[1],
    length.out = dim(indiv_par_est)[1])
  ])
  indiv_par_est[,i] <- apply(indiv_par_samples[[i]], MARGIN=2, FUN=median)
  hist(indiv_par_est[,i], main=indiv_pars[i], 25)
}

