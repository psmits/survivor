data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  int C;  // cohort
  int O;  // order
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
  int coh_unc[N_unc];  // cohort membership
  int coh_cen[N_cen];  // cohort membership
  int ord_unc[N_unc];  // cohort membership
  int ord_cen[N_cen];  // cohort membership
}
parameters {
  // regression coefficients for scale parameter
  real inter[O];
  vector[4] beta;
  real<lower=0> alpha;

  real mu;
  real<lower=0> sigma;
}
model {
  mu ~ normal(0, 100);

  for(i in 1:4) {
    beta ~ student_t(4, 0, 100);
  }
  inter ~ normal(mu, sigma);

  alpha ~ gamma(1, 0.0001);

  // likelihood
  for(i in 1:N_unc) {
    dur_unc ~ weibull(alpha, exp(-(inter[ord_unc[i]] + 
            beta[1] * size_unc + 
            beta[2] * occ_unc + 
            beta[3] * hab_unc + 
            beta[4] * aff_unc) / alpha));
  }
  // censored observations
  for(i in 1:N_cen) {  
    increment_log_prob(weibull_ccdf_log(dur_cen, alpha,
          exp(-(inter[ord_cen[i]] +
              beta[1] * size_cen +
              beta[2] * occ_cen +
              beta[3] * hab_cen +
              beta[4] * aff_cen) / alpha)));
  }
}
