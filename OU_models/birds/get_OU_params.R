require(rstan)

files <- dir('output')

files <- files[startsWith(files,'OU_UV_dist')]

theta <- c()
sigma <- c()
alpha <- c()

for (fn in files) {
  fit <- readRDS(paste('output/',fn,sep=''))
  theta <- c(theta,extract(fit)$theta_0)
  sigma <- c(sigma,extract(fit)$sigma_0)
  alpha <- c(alpha,extract(fit)$alpha_0)
}

saveRDS(file='OU_params.RDS',list(theta,sigma,alpha))