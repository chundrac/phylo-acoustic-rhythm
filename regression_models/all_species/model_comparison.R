require(brms)

loglik.pred.dist <- NULL

dom.inds <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46, 47, 48, 49, 50)

for (i in dom.inds) {
  load(paste('output/model_fit_dominant_frequency_pred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.dist <- rbind(loglik.pred.dist,loglik)
}

loglik.pred.nodist <- NULL

for (i in dom.inds) {
  load(paste('output/model_fit_dominant_frequency_pred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.nodist <- rbind(loglik.pred.nodist,loglik)
}

loglik.nopred.dist <- NULL

for (i in dom.inds) {
  load(paste('output/model_fit_dominant_frequency_nopred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.dist <- rbind(loglik.nopred.dist,loglik)
}

loglik.nopred.nodist <- NULL

for (i in dom.inds) {
  load(paste('output/model_fit_dominant_frequency_nopred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.nodist <- rbind(loglik.nopred.nodist,loglik)
}

print("dominant frequency")
dominant.compare <- loo_compare(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))
dominant.stack <- loo_model_weights(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))

#inds <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46, 47, 48, 49, 50)
inds <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 42, 43, 44, 45, 47, 48, 49, 50)

loglik.pred.dist <- NULL

for (i in inds) {
  load(paste('output/model_fit_frequency_pred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.dist <- rbind(loglik.pred.dist,loglik)
}

loglik.pred.nodist <- NULL

for (i in inds) {
  load(paste('output/model_fit_frequency_pred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.pred.nodist <- rbind(loglik.pred.nodist,loglik)
}

loglik.nopred.dist <- NULL

for (i in inds) {
  load(paste('output/model_fit_frequency_nopred.dist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.dist <- rbind(loglik.nopred.dist,loglik)
}

loglik.nopred.nodist <- NULL

for (i in inds) {
  load(paste('output/model_fit_frequency_nopred.nodist_',i,'.Rdata',sep=''))
  loglik <- log_lik(model)
  loglik.nopred.nodist <- rbind(loglik.nopred.nodist,loglik)
}

print("frequency")
rhythm.compare <- loo_compare(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))
rhythm.stack <- loo_model_weights(list(loo(loglik.pred.dist),loo(loglik.pred.nodist),loo(loglik.nopred.dist),loo(loglik.nopred.nodist)))

saveRDS(list(dominant.compare,dominant.stack,rhythm.compare,rhythm.stack),'model.comparisons.RDS')
