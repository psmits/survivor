data {
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  real dur_unc[N_unc];
  real dur_cen[N_cen];
  vector[N_unc] size_unc;
  vector[N_cen] size_cen;
  vector[N_unc] occ_unc;
  vector[N_cen] occ_cen;
  vector[N_unc] hab_unc;
  vector[N_cen] hab_cen;
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  beta[1] ~ normal(0, 100);
  beta[2] ~ normal(0, 100);
  beta[3] ~ normal(0, 100);
  beta[4] ~ normal(0, 100);
  sigma ~ inv_gamma(1, 1);

  dur_unc ~ lognormal(exp(beta[1] + beta[2] * size_unc + beta[3] * occ_unc
                          + beta[4] * hab_unc), 
                      sigma);
  increment_log_prob(lognormal_ccdf_log(dur_cen, exp(beta[1]
                     + beta[2] * size_cen + beta[3] * occ_cen
                     + beta[4] * hab_cen), 
                     sigma));
}

