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

#saveRDS(list(x.dom,x.freq,x.dom2),file='model_params.RDS')

param.list <- readRDS('model_params.RDS')
x.dom = param.list[[1]]
x.freq = param.list[[2]]
x.dom2 = param.list[[3]]

x.dom <- x.dom[,1:(ncol(x.dom)/2)]

props.dom <- apply(x.dom,2,function(x){length(which(x>0))/length(x)})

label.dom <- ifelse(props.dom > .5, paste(round(props.dom,2),'>','0'), paste(round(1-props.dom,2),'<','0'))

pdf('post_CIs_dom_dist.pdf')
mcmc_intervals(x.dom,prob=.8,prob_outer=.9)+xlim(-5,10)+
  annotate('text',x=10,y=c(1:length(props.dom)),label=rev(label.dom),size=3,family='serif')
dev.off()

x.dom2 <- x.dom2[,1:(ncol(x.dom2)/2)]

props.dom <- apply(x.dom2,2,function(x){length(which(x>0))/length(x)})

label.dom <- ifelse(props.dom > .5, paste(round(props.dom,2),'>','0'), paste(round(1-props.dom,2),'<','0'))

pdf('post_CIs_dom_nodist.pdf')
mcmc_intervals(x.dom2,prob=.8,prob_outer=.9)+xlim(-5,10) + 
  annotate('text',x=10,y=c(1:length(props.dom)),label=rev(label.dom),size=3,family='serif')
dev.off()

x.freq <- x.freq[,1:(ncol(x.freq)/2)]

props.dom <- apply(x.freq,2,function(x){length(which(x>0))/length(x)})

label.dom <- ifelse(props.dom > .5, paste(round(props.dom,2),'>','0'), paste(round(1-props.dom,2),'<','0'))

pdf('post_CIs_frequency.pdf')
mcmc_intervals(x.freq,prob=.8,prob_outer=.9)+xlim(-5,10) + 
  annotate('text',x=10,y=c(1:length(props.dom)),label=rev(label.dom),size=3,family='serif')
dev.off()