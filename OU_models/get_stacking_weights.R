require(rstan)
require(loo)

model.types <- c('BM_UV_nondist',
                 'BM_UV_dist',
                 'OU_UV_nondist',
                 'OU_UV_dist')


logliks <- list()

for (j in 1:length(model.types)) {
  loo.j <- NULL
  for (i in 1:50) {
    fit <- readRDS(paste('output_new/',model.types[j],'_',i,'.RDS',sep=''))
    print(fit)
    loo.ij <- extract_log_lik(fit)
    loo.j <- rbind(loo.j,loo.ij)
  }
  logliks[[j]] <- loo.j
}

loos <- lapply(logliks,loo)

print(loos)

loo_model_weights(loos)

saveRDS(file='frequency_loos.RDS',loos)