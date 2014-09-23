data {
  int<lower=0> N;
  vector[N] dur;
}
parameters {
  real<lower=0> sigma;
#  real<lower=0>  alpha;
}
model {
  sigma ~ gamma(1, 0.0001);

  dur ~ exponential(sigma);
}
