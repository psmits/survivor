data {
  int<lower=0> N;
  int<lower=0> N_unc;
  int<lower=0> N_cen;
  #int G;  // number of groups
  int C;  // number of groups
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
  #int mem_unc[N_unc];  // group membership
  #int mem_cen[N_cen];  // group membership
  int coh_unc[N_unc];  // cohort membership
  int coh_cen[N_cen];  // cohort membership
}
parameters {
  // regression coefficients for scale parameter
  #real beta_inter[C];
  real beta_size[C];
  real beta_occ[C];
  real beta_hab[C];
  real beta_aff[C];
  #real inter_mu;
  real size_mu;
  real occ_mu;
  real hab_mu;
  real aff_mu;
  #real<lower=0,upper=100> inter_sigma;
  real<lower=0,upper=100> size_sigma;
  real<lower=0,upper=100> occ_sigma;
  real<lower=0,upper=100> hab_sigma;
  real<lower=0,upper=100> aff_sigma;

  // shape parameter
  real<lower=0> alpha[C];
  real<lower=0> scale[2];
}
model {
  // hyperpriors
  #inter_mu ~ normal(0, 100);
  size_mu ~ student_t(4, 0, 100);
  occ_mu ~ student_t(4, 0, 100);
  hab_mu ~ student_t(4, 0, 100);
  aff_mu ~ student_t(4, 0, 100);

  #inter_sigma ~ cauchy(0, 2.5);
  size_sigma ~ cauchy(0, 2.5);
  occ_sigma ~ cauchy(0, 2.5);
  hab_sigma ~ cauchy(0, 2.5);
  aff_sigma ~ cauchy(0, 2.5);

  // priors
  #beta_inter ~ normal(inter_mu, inter_sigma);
  beta_size ~ student_t(4, size_mu, size_sigma);
  beta_occ ~ student_t(4, occ_mu, occ_sigma);
  beta_hab ~ student_t(4, hab_mu, hab_sigma);
  beta_aff ~ student_t(4, aff_mu, aff_sigma);

  alpha ~ gamma(scale[1], scale[2]);

  // likelihood
  for(i in 1:N_unc) {
    dur_unc ~ weibull(alpha[coh_unc[i]], 
        exp(-(#beta_inter[coh_unc[i]] +
            beta_size[coh_unc[i]] * size_unc[i] + 
            beta_occ[coh_unc[i]] * occ_unc[i] + 
            beta_hab[coh_unc[i]] * hab_unc[i] + 
            beta_aff[coh_unc[i]] * aff_unc[i]) / alpha[coh_unc[i]]));
  }
  // censored observations
  for(i in 1:N_cen) {  
    increment_log_prob(weibull_ccdf_log(dur_cen, alpha[coh_cen[i]], 
          exp(-(#beta_inter[coh_cen[i]] +
              beta_size[coh_cen[i]] * size_cen[i] + 
              beta_occ[coh_cen[i]] * occ_cen[i] + 
              beta_hab[coh_cen[i]] * hab_cen[i] + 
              beta_aff[coh_cen[i]] * aff_cen[i]) / alpha[coh_cen[i]])));
  }
}
