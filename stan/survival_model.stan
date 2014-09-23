data {
  int<lower=0> N;
  vector[N] duration;
  vector[N] siz;
  vector[N] aff;
}
parameters {
  vector[3] beta;
  real<lower=0>  alpha;
}
model {
  beta[1] ~ normal(0, 1000);
  beta[2] ~ normal(0, 1000);
  beta[3] ~ normal(0, 1000);
  alpha ~ gamma(1, 1);

  duration ~ weibull(alpha, exp(beta[1] + beta[2] * siz + beta[3] * aff));
}
