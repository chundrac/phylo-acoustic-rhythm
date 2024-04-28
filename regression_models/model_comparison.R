require(brms)

loglik.pred.dist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_dominant_frequency_pred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.dist <- rbind(loglik.pred.dist,loglik)
}

loglik.pred.nodist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_dominant_frequency_pred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.nodist <- rbind(loglik.pred.nodist,loglik)
}

loglik.nopred.dist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_dominant_frequency_nopred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.dist <- rbind(loglik.nopred.dist,loglik)
}

loglik.nopred.nodist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_dominant_frequency_nopred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.nodist <- rbind(loglik.nopred.nodist,loglik)
}

print("dominant frequency")
dominant.compare <- loo_compare(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))
dominant.stack <- loo_model_weights(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))

loglik.pred.dist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_frequency_pred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.dist <- rbind(loglik.pred.dist,loglik)
}

loglik.pred.nodist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_frequency_pred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.nodist <- rbind(loglik.pred.nodist,loglik)
}

loglik.nopred.dist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_frequency_nopred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.dist <- rbind(loglik.nopred.dist,loglik)
}

loglik.nopred.nodist <- NULL

for (i in 1:10) {
  load(paste('output/model_fit_frequency_nopred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.nodist <- rbind(loglik.nopred.nodist,loglik)
}

print("frequency")
rhythm.compare <- loo_compare(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))
rhythm.stack <- loo_model_weights(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))

saveRDS(list(dominant.compare,dominant.stack,rhythm.compare,rhythm.stack),'model.comparisons.RDS')