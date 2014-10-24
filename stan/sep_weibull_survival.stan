data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  int O;  // number of orders
  int C;  // number of cohorts
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
  matrix[N_unc, O] ord_unc;
  matrix[N_cen, O] ord_cen;
  int coh_unc[N_unc];  // cohort membership
  int coh_cen[N_cen];  // cohort membership
}
parameters {
  vector[2] beta;
  real beta_inter;
  #vector[O] beta_ord;
  real<lower=0,upper=10> alpha[C];
  real<lower=0> scale[2];
}
model {
  for(i in 1:2) {
    beta[i] ~ student_t(4, 0, 100);
  }
  beta_inter ~ normal(0, 100);
  #beta_ord ~ normal(0, 100);

  alpha ~ gamma(scale[1], scale[2]);

  // go through each sample and index the alpha of the sample membership
  for(i in 1:N_unc) {
    dur_unc ~ weibull(alpha[coh_unc[i]], 
        exp(-(beta_inter + 
            beta[1] * occ_unc + 
            beta[2] * size_unc)/ alpha[coh_unc[i]]));
  }
  for(i in 1:N_cen) {
    increment_log_prob(weibull_ccdf_log(dur_cen, alpha[coh_cen[i]], 
          exp(-(beta_inter + 
              beta[1] * occ_cen + 
              beta[2] * size_cen) / alpha[coh_cen[i]])));
  }
}
