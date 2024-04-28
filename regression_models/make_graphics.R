require(brms)
require(bayesplot)

set.seed(1)

x.dom <- NULL
for (i in 1:10) {
  load(paste('output/model_fit_dominant_frequency_pred.dist_',i,'.Rdata',sep=''))
  x.i <- as.matrix(model$fit)[,1:36]
  x.dom <- rbind(x.dom,x.i[sample(1:nrow(x.i),400),])
}

x.dom2 <- NULL
for (i in 1:10) {
  load(paste('output/model_fit_dominant_frequency_pred.nodist_',i,'.Rdata',sep=''))
  x.i <- as.matrix(model$fit)[,1:36]
  x.dom2 <- rbind(x.dom2,x.i[sample(1:nrow(x.i),400),])
}
  
#pdf('post_CIs_dominant_frequency.pdf')
#mcmc_intervals(x,prob_outer=.95,prob=.85)
#dev.off()

x.freq <- NULL
for (i in 1:10) {
  load(paste('output/model_fit_frequency_pred.dist_',i,'.Rdata',sep=''))
  x.i <- as.matrix(model$fit)[,1:36]
  x.freq <- rbind(x.freq,x.i[sample(1:nrow(x.i),400),])
}

#pdf('post_CIs_frequency.pdf')
#mcmc_intervals(x,prob_outer=.95,prob=.85)
#dev.off()

saveRDS(list(x.dom,x.freq,x.dom2),file='model_params.RDS')