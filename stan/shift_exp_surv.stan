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
transformed data {
  real shift_unc[N_unc];
  real shift_cen[N_cen];

  for(i in 1:N_unc) {
    shift_unc[i] <- dur_unc[i] - 1;
  }
  for(i in 1:N_cen) {
    shift_cen[i] <- dur_cen[i] - 1;
  }
}
parameters {
  vector[5] beta;
#  real<lower=0> alpha;
}
model {
  beta ~ student_t(4, 0, 100);
#  alpha ~ gamma(1, 1);

  shift_unc ~ exponential(exp(-(beta[1] * size_unc + beta[2] * occ_unc + beta[3] * hab_unc + beta[4] * aff_unc + beta[5])));
  increment_log_prob(exponential_ccdf_log(shift_cen, exp(-(beta[1] * size_cen
            + beta[2] * occ_cen + beta[3] * hab_cen + beta[4] * aff_cen + beta[5]))));
}
