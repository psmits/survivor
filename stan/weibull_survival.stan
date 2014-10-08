data {
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
}
parameters {
  vector[4] beta;
  real<lower=0> alpha;
}
model {
  beta ~ student_t(4, 0, 100);
  alpha ~ gamma(1, 1);

  dur_unc ~ weibull(alpha, exp(-(beta[1] * size_unc + beta[2] * occ_unc + beta[3] * hab_unc + beta[4] * aff_unc) / alpha));
  increment_log_prob(weibull_ccdf_log(dur_cen, alpha, exp(-(beta[1] * size_cen
            + beta[2] * occ_cen + beta[3] * hab_cen + beta[4] * aff_cen) / alpha)));
}
