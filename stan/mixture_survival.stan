data {
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  real dur_unc[N_unc];
  real dur_cen[N_cen];
  int<lower=1> K;
}
parameters {
  real<lower=0> rate[K];
  simplex[K] theta;
}
model {
  real ps[K];
  real pc[K];
  
  for(i in 1:K) 
    rate[i] ~ exponential(0.001);

  for(i in 1:N_unc) {
    for(j in 1:K) {
      ps[j] <- log(theta[j]) + exponential_log(dur_unc[i], rate[j]);
    }
    increment_log_prob(log_sum_exp(ps));
  }
  for(i in 1:N_cen) {
    for(j in 1:K) {
      pc[j] <- log(theta[j]) + exponential_log(dur_cen[i], rate[j]);
    }
    increment_log_prob(log_sum_exp(pc));
  }
}

