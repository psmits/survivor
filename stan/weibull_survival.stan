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
  real<lower=0> alpha;
}
model {
  beta ~ normal(0, 100);
  alpha ~ gamma(1, 1);

  dur_unc ~ weibull(alpha, exp(beta[1] + beta[2] * size_unc + beta[3] * occ_unc + beta[4] * hab_unc));
  increment_log_prob(weibull_ccdf_log(dur_cen, alpha, exp(beta[1]
                     + beta[2] * size_cen + beta[3] * occ_cen +
                     beta[4] * hab_cen)));
}
