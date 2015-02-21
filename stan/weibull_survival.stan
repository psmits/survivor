data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  int O;  // number of orders
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
  matrix[N_unc, O] ord_mat_unc;
  matrix[N_cen, O] ord_mat_cen;
}
parameters {
  vector[5] beta;
//  vector[O] beta_ord;
  real<lower=0> alpha;
}
transformed parameters {
}
model {
  beta ~ student_t(4, 0, 100);
//  beta_ord ~ normal(0, 100);
  alpha ~ gamma(1, 0.0001);

  dur_unc ~ weibull(alpha, exp(-(beta[1] + 
          beta[2] * size_unc + 
          beta[3] * occ_unc + 
          beta[4] * hab_unc + 
          beta[5] * aff_unc) / alpha))];
  increment_log_prob(weibull_ccdf_log(dur_cen, alpha, exp(-(beta[1] + 
            beta[2] * size_cen + 
            beta[3] * occ_cen +
            beta[4] * hab_cen + 
            beta[5] * aff_cen) / alpha)));
}
