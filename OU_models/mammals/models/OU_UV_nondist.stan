data {
  int<lower=1> N;                   //number of observations
  int<lower=1> T;                   //number of tips in tree
  matrix[T,T] phy_cov;
  matrix[T,T] coph_dist;
  int<lower=0,upper=T> taxon_ID[N]; //species associated with each data point
  real<lower=0> y[N];               //observations
}
parameters {
  real theta_0;
  real<lower=0> alpha_0;
  real<lower=0> sigma_0;         //sigma^2
  real<lower=0> tau;
  vector[T] eps;
}
transformed parameters {
  matrix[T,T] V = cholesky_decompose(sigma_0*exp(-alpha_0*coph_dist)+diag_matrix(rep_vector(.001,T)));
  vector[T] y_pred = rep_vector(theta_0,T) + V*eps;
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = lognormal_lpdf(y[i]|y_pred[taxon_ID[i]],tau);
  }
}
model {
  theta_0 ~ normal(0,1);
  alpha_0 ~ gamma(1,1);
  sigma_0 ~ gamma(1,1);
  tau ~ gamma(1,1);
  eps ~ normal(0,1);
  target += sum(log_lik);
}
