functions {
  real relative_diff(real x, real y) {
    real abs_diff;
    real avg_scale;
    abs_diff = abs(x - y);
    avg_scale = (abs(x) + abs(y)) / 2;
    return abs_diff / avg_scale;
  }
}

data {
  int<lower=0> J;
  vector[J] y;
  vector<lower=0>[J]      sigma;
}

parameters {
  real mu;
  real<lower=0> tau;
  vector[J] theta;
}




model {
  real a = 3;
  real b = 2;
  real c;
  c = a - b;
  mu ~ normal(0, relative_diff(c, 3) + 4);
  tau ~ cauchy(0, 5);
  theta ~ normal(mu, tau);
  y ~ normal(theta, sigma);
}
