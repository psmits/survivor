data {
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  real dur_unc[N_unc];
  real dur_cen[N_cen];
  vector[N_unc] size_unc;
  vector[N_cen] size_cen;
  vector[N_unc] aff_unc;
  vector[N_cen] aff_cen;
  vector[N_unc] occ_unc;
  vector[N_cen] occ_cen;
  vector[N_unc] hab_unc;
  vector[N_cen] hab_cen;
}
parameters {
  real<lower=0> alpha;
  vector[5] beta;
}
model {
  alpha ~ gamma(1, 1);
  beta[1] ~ normal(0, 100);
  beta[2] ~ normal(0, 100);
  beta[3] ~ normal(0, 100);
  beta[4] ~ normal(0, 100);
  beta[5] ~ normal(0, 100);

  dur_unc ~ gamma(alpha, exp(beta[1] + beta[2] * size_unc + beta[3] * aff_unc
                               + beta[4] * occ_unc + beta[5] * hab_unc));
  increment_log_prob(gamma_ccdf_log(dur_cen, alpha, exp(beta[1]
                     + beta[2] * size_cen + beta[3] * aff_cen + 
                     beta[4] * occ_cen + beta[5] * hab_cen)));
}

