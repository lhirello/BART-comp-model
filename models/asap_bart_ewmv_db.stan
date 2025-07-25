// source for model code: https://github.com/CCS-Lab/hBayesDM/blob/develop/commons/stan_files/bart_ewmv.stan

data {
  int<lower=1> N;                               // Number of subjects
  int<lower=1> T;                               // Maximum number of trials
  array[N] int<lower=0> Tsubj;                  // Number of trials for each subject
  int<lower=2> P;                               // Number of max pump + 1 ** CAUTION **
  array[N, T] int<lower=0> pumps;               // Number of pump
  array[N, T] int<lower=0, upper=1> explosion;  // Whether the balloon exploded (0 or 1)
}

transformed data {
  
  // Whether a subject pump the button or not (0 or 1)
  array[N, T, P] int d;

  for (j in 1:N) {
    for (k in 1:Tsubj[j]) {
      for (l in 1:P) {
        if (l <= pumps[j, k])
          d[j, k, l] = 1;
        else
          d[j, k, l] = 0;
      }
    }
  }
}

parameters {
  
  // No hierarchical estimation
  vector<lower=0,upper=1>[N] phi;
  vector<lower=0>[N]         eta;
  vector[N] rho;
  vector<lower=0>[N]         tau;
  vector<lower=0>[N]         lambda;
  
}

transformed parameters {
  
  // vector[N] tau = rep_vector(1, N);
  // vector<lower=-0.5, upper=0.5>[N] rho = 0.5 - rho_pr;

}

model {
  
  // Prior
  phi    ~ beta(1, 1);
  eta    ~ exponential(1);
  rho    ~ normal(0,10);
  tau    ~ exponential(1);
  lambda ~ exponential(1);

  // Likelihood
  for (j in 1:N) {
    // Initialize n_succ and n_pump for a subject
    int n_succ = 0;  // Number of successful pumps
    int n_pump = 0;  // Number of total pumps
    real p_burst = phi[j];

    for (k in 1:Tsubj[j]) {
      real u_gain = 1;
      real u_loss;
      real u_pump;
      real u_stop = 0;
      real delta_u;

      for (l in 1:(pumps[j, k] + 1 - explosion[j, k])) {
        
        u_loss = (l - 1);

        u_pump = (1 - p_burst) * u_gain - lambda[j] * p_burst * u_loss +
        rho[j] * p_burst * (1 - p_burst) * (u_gain + lambda[j] * u_loss)^2;
        // u_stop always equals 0.

        delta_u = u_pump - u_stop;

        // Calculate likelihood with bernoulli distribution
        d[j, k, l] ~ bernoulli_logit(tau[j] * delta_u);
        // d[j, k, l] ~ bernoulli_logit(delta_u);
      }

      // Update n_succ and n_pump after each trial ends
      n_succ += pumps[j, k] - explosion[j, k];
      n_pump += pumps[j, k];

      if(n_pump>0){
        p_burst = phi[j] + (1 - exp(-n_pump * eta[j])) * ((0.0 + n_pump - n_succ) / n_pump - phi[j]);
      }
    }
  }
}

generated quantities {

  // Log-likelihood for model fit
  array[N] real log_lik;

  // For posterior predictive check
  array[N, T, P] real y_pred;

  // Set all posterior predictions to 0 (avoids NULL values)
  for (j in 1:N)
    for (k in 1:T)
      for(l in 1:P)
        y_pred[j, k, l] = -1;

  { // Local section to save time and space
    for (j in 1:N) {
      // Initialize n_succ and n_pump for a subject
      int n_succ = 0;  // Number of successful pumps
      int n_pump = 0;  // Number of total pumps
      real p_burst = phi[j];

      log_lik[j] = 0;

      for (k in 1:Tsubj[j]) {
        real u_gain = 1;
        real u_loss;
        real u_pump;
        real u_stop = 0;
        real delta_u;

        for (l in 1:(pumps[j, k] + 1 - explosion[j, k])) {
          // u_gain always equals r ^ rho.
          u_loss = (l - 1);

          u_pump = (1 - p_burst) * u_gain - lambda[j] * p_burst * u_loss +
          rho[j] * p_burst * (1 - p_burst) * (u_gain + lambda[j] * u_loss)^2;
          // u_stop always equals 0.

          delta_u = u_pump - u_stop;

          log_lik[j] += bernoulli_logit_lpmf(d[j, k, l] | tau[j] * delta_u);
          y_pred[j, k, l] = bernoulli_logit_rng(tau[j] * delta_u);
          // log_lik[j] += bernoulli_logit_lpmf(d[j, k, l] | delta_u);
          // y_pred[j, k, l] = bernoulli_logit_rng(delta_u);
        }

        // Update n_succ and n_pump after each trial ends
        n_succ += pumps[j, k] - explosion[j, k];
        n_pump += pumps[j, k];

        if(n_pump>0){
          p_burst = phi[j] + (1 - exp(-n_pump * eta[j])) * ((0.0 + n_pump - n_succ) / n_pump - phi[j]);
        }
      }
    }
  }
}

