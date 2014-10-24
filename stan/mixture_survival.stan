data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  real<lower=1> dur_unc[N_unc];
  real<lower=1> dur_cen[N_cen];
  vector[N_unc] size_unc;
  vector[N_cen] size_cen;
  vector[N_unc] occ_unc;
  vector[N_cen] occ_cen;
  vector[N_unc] hab_unc;
  vector[N_cen] hab_cen;
  vector[N_unc] aff_unc;
  vector[N_cen] aff_cen;
  int<lower=1> K;
}
parameters {
  matrix[4,K] beta;
  real<lower=0> alpha[K];
  simplex[K] theta;
}
model {
  real ps[K];
  real pc[K];

  for(i in 1:K) {
    beta[1,i] ~ student_t(4, 0, 100);
    beta[2,i] ~ student_t(4, 0, 100);
    beta[3,i] ~ student_t(4, 0, 100);
    beta[4,i] ~ student_t(4, 0, 100);
    alpha[i] ~ gamma(1, 0.001);
  }


  for(i in 1:N_unc) {
    for(j in 1:K) {
      ps[j] <- log(theta[j]) + weibull_log(dur_unc[i], alpha[j], 
          exp(-(beta[1,K] * size_unc[i] + beta[2,K] * occ_unc[i] + 
              beta[3,K] * hab_unc[i] + beta[4,K] * aff_unc[i]) / alpha[j]));
    }
    increment_log_prob(log_sum_exp(ps));
  }
  for(i in 1:N_cen) {
    for(j in 1:K) {
      pc[j] <- log(theta[j]) + weibull_ccdf_log(dur_cen[i], alpha[j], 
          exp(-(beta[1,K] * size_cen[i] + beta[2,K] * occ_cen[i] + 
              beta[3,K] * hab_cen[i] + beta[4,K] * aff_cen[i]) / alpha[j]));
    }
    increment_log_prob(log_sum_exp(pc));
  }
}
