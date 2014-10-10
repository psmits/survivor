data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  int G;  // number of groups
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
  int mem_unc[N_unc];  // group membership
  int mem_cen[N_cen];  // group membership
}
parameters {
  vector[4] beta;
  real<lower=0> alpha[G];
  real<lower=0> scale;
}
transformed parameters {
}
model {
  beta ~ student_t(4, 0, 100);

  alpha ~ lognormal(1, scale);

  // go through each sample and index the alpha of the sample membership
  for(i in 1:N_unc) {
    dur_unc ~ weibull(alpha[mem_unc[i]], exp(-(beta[1] * size_unc[i] + 
            beta[2] * occ_unc[i] + beta[3] * hab_unc[i] + 
            beta[4] * aff_unc[i]) / alpha[mem_unc[i]]));
  }
  for(i in 1:N_cen) {
    increment_log_prob(weibull_ccdf_log(dur_cen, alpha[mem_cen[i]], 
          exp(-(beta[1] * size_cen[i] + beta[2] * occ_cen[i] + 
              beta[3] * hab_cen[i] + beta[4] * aff_cen[i]) / alpha[mem_cen[i]])));
  }
}
