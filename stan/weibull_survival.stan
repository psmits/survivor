data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  int G;  // number of orders
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
  matrix[N_unc, G] ord_unc;
  matrix[N_cen, G] ord_cen;
}
parameters {
  vector[4] beta;
  vector[G] beta_ord;
  real<lower=0> alpha;
}
transformed parameters {
}
model {
  beta ~ student_t(4, 0, 100);
  beta_ord ~ normal(0, 100);
  alpha ~ gamma(1, 0.0001);

  dur_unc ~ weibull(alpha, exp(-(beta[1] * size_unc + beta[2] * occ_unc + 
          beta[3] * hab_unc + beta[4] * aff_unc + ord_unc * beta_ord) / alpha));
  increment_log_prob(weibull_ccdf_log(dur_cen, alpha, exp(-(beta[1] * size_cen + 
            beta[2] * occ_cen + beta[3] * hab_cen + 
            beta[4] * aff_cen + ord_cen * beta_ord) / alpha)));
}
