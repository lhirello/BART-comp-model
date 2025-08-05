data {
  int<lower=1> N;            // Number of subjects
  int<lower=1> T;            // Maximum number of trials
  array[N] int<lower=0> Tsubj;                  // Number of trials for each subject
  int<lower=2> P;            // Number of max pump + 1 ** CAUTION **
  array[N, T] int<lower=-1> pumps;               // Number of pump (-1 = missing)
  array[N, T] int<lower=0> is_missing;          // Whether data is missing for this trial
  array[N, T] int<lower=-1, upper=1> explosion;  // Whether the balloon exploded (0 or 1; -1 = missing)
}

transformed data{
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

  // Normally distributed error for Matt trick
  vector<lower=0, upper=1>[N] phi;
  vector<lower=0>[N] eta;
  vector<lower=0>[N] gamma;
  // vector<lower=0>[N] tau;
  // vector[N] eta_pr;
  // vector[N] gamma_pr;
  vector[N] tau_pr;
}

transformed parameters {
  
  // vector[N] eta   = Phi_approx(eta_pr);
  // vector[N] gamma = Phi_approx(gamma_pr) * 5;
  vector[N] tau   = Phi_approx(tau_pr) * 20;

}

model {
  
  // Prior
  phi    ~ beta(1, 1);
  eta    ~ exponential(10);
  gamma  ~ exponential(1);
  // tau    ~ exponential(1);
  // eta_pr    ~ normal(0,1);
  // gamma_pr  ~ normal(0,1);
  tau_pr    ~ normal(0,1);

  
  // Likelihood
  for (j in 1:N) {
    // Initialize n_succ and n_pump for a subject
    int n_succ = 0;  // Number of successful pumps
    int n_pump = 0;  // Number of total pumps
    
    for (k in 1:Tsubj[j]) {
      real p_burst;  // Belief on a balloon to be burst
      real omega;    // Optimal number of pumps
      
      p_burst = 1 - ((phi[j] + eta[j] * n_succ) / (1 + eta[j] * n_pump));
      omega = -gamma[j] / log1m(p_burst);
      
      // Calculate likelihood with bernoulli distribution
      if (is_missing[j,k] == 0){
        for (l in 1:(pumps[j, k] + 1 - explosion[j, k])){
          d[j, k, l] ~ bernoulli_logit(tau[j] * (omega - l));
        }
        
        // Update n_succ and n_pump after each trial ends
        n_succ += pumps[j, k] - explosion[j, k];
        n_pump += pumps[j, k];
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
      int n_succ = 0;
      int n_pump = 0;
      
      log_lik[j] = 0;
      
      for (k in 1:Tsubj[j]) {
        real p_burst;  // Belief on a balloon to be burst
        real omega;    // Optimal number of pumps
        
        p_burst = 1 - ((phi[j] + eta[j] * n_succ) / (1 + eta[j] * n_pump));
        omega = -gamma[j] / log1m(p_burst);
        
        if (is_missing[j,k] == 0){
          for (l in 1:(pumps[j, k] + 1 - explosion[j, k])){
            log_lik[j] += bernoulli_logit_lpmf(d[j, k, l] | tau[j] * (omega - l));
            y_pred[j, k, l] = bernoulli_logit_rng(tau[j] * (omega - l));
          }
          
          n_succ += pumps[j, k] - explosion[j, k];
          n_pump += pumps[j, k];
          
        }
      }
    }
  }
}

