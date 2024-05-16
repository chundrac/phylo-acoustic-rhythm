require(rstan)
require(phytools)
require(phangorn)
require(condMVNorm)

trees <- read.nexus('../BEAST/Coding_mtDNA_HKY_Yule_simple.trees')

mcc.tree <- maxCladeCred(trees)

data.df <- read.csv('../Data_Rhythm_Final_English.csv',sep=';')

mcc.tree$tip.label <- tolower(gsub(' |_','',mcc.tree$tip.label))

taxon.new <- ifelse(tolower(gsub(' |_','',data.df[,8])) %in% mcc.tree$tip.label, 
                    tolower(gsub(' |_','',data.df[,8])), 
                    tolower(gsub(' |_','',data.df[,7])))

taxon.new[taxon.new=='burhinusoedicnemus'] <- 'burhinusbistriatus'
taxon.new[taxon.new=='zaedyuspichyi'] <- 'zaedyuspichiy'
taxon.new[taxon.new=='pantroglodytes'] <- 'pantroglodytestroglodytes'
taxon.new[taxon.new=='chlorocebuspygerythus'] <- 'chlorocebuspygerythrus'
taxon.new[taxon.new=='opisthocomushoazin'] <- 'ophisthocomushoazin'

taxon.data <- data.df[,c('Log_Frequency','Log_Weight','Environement','Mastication')]
colnames(taxon.data) <- c('frequency','weight','environment','mastication')
taxon.data$taxon <- as.factor(taxon.new)

taxon.data <- taxon.data[,c('taxon','frequency')]
taxon.data <- na.omit(taxon.data) #find better way to deal with missing values
taxon.data <- taxon.data[taxon.data$taxon %in% mcc.tree$tip.label,]
mcc.tree <- keep.tip(mcc.tree, which(mcc.tree$tip.label %in% taxon.data$taxon))

taxon.data$frequency <- 10^(taxon.data$frequency)
taxon.data <- droplevels(taxon.data)

#phy.cov.full <- vcvPhylo(mcc.tree)

coph.dist <- dist.nodes(mcc.tree)
coph.dist <- coph.dist / max(coph.dist)

T = length(mcc.tree$tip.label)

coph.dist <- coph.dist[-(T+1),-(T+1)]

N = nrow(coph.dist)

y.internal.all <- NULL

for (i in 1:50) {
  
  print(i)
  
  my.seed <- i
  
  set.seed(my.seed)
  
  fit <- readRDS(paste('output/OU_UV_dist_',i,'.RDS',sep=''))
  
  n.tree = sample(length(trees),1)
  
  tree <- trees[[n.tree]]
  
  data.df <- read.csv('../Data_Rhythm_Final_English.csv',sep=';')
  
  tree$tip.label <- tolower(gsub(' |_','',tree$tip.label))
  
  taxon.new <- ifelse(tolower(gsub(' |_','',data.df[,8])) %in% tree$tip.label, 
                      tolower(gsub(' |_','',data.df[,8])), 
                      tolower(gsub(' |_','',data.df[,7])))
  
  taxon.new[taxon.new=='burhinusoedicnemus'] <- 'burhinusbistriatus'
  taxon.new[taxon.new=='zaedyuspichyi'] <- 'zaedyuspichiy'
  taxon.new[taxon.new=='pantroglodytes'] <- 'pantroglodytestroglodytes'
  taxon.new[taxon.new=='chlorocebuspygerythus'] <- 'chlorocebuspygerythrus'
  taxon.new[taxon.new=='opisthocomushoazin'] <- 'ophisthocomushoazin'
  
  taxon.data <- data.df[,c('Log_Frequency','Log_Weight','Environement','Mastication')]
  colnames(taxon.data) <- c('frequency','weight','environment','mastication')
  taxon.data$taxon <- as.factor(taxon.new)
  
  taxon.data <- taxon.data[,c('taxon','frequency')]
  taxon.data <- na.omit(taxon.data) #find better way to deal with missing values
  taxon.data <- taxon.data[taxon.data$taxon %in% tree$tip.label,]
  tree <- keep.tip(tree, which(tree$tip.label %in% taxon.data$taxon))
  
  stopifnot(all(tree$tip.label==mcc.tree$tip.label))
  
  taxon.data$frequency <- 10^(taxon.data$frequency)
  taxon.data <- droplevels(taxon.data)
  
  for (j in 1:10) {
    
    t = sample(1:4000,1)
    
    y.pred <- extract(fit)$y_pred[t,]
    theta.0 <- extract(fit)$theta_0[t]
    alpha.0 <- extract(fit)$alpha_0[t]
    sigma.0 <- extract(fit)$sigma_0[t]
    
    mu = rep(theta.0,N)
    Sigma = sigma.0*exp(-alpha.0*coph.dist) + diag(rep(.001,N))
    
    y.internal <- exp(condMVN(mu, Sigma, (T+1):N, 1:T, y.pred, check.sigma=TRUE)[[1]])
    y.internal.all <- rbind(y.internal.all,y.internal)
    
  }
  
}

saveRDS(y.internal.all,file='internal_reconstructions.RDS')

medians.internal <- apply(y.internal.all,2,median)

medians.taxon <- aggregate(frequency ~ taxon, FUN=median, taxon.data)
medians.taxon <- as.data.frame(medians.taxon)
rownames(medians.taxon) <- medians.taxon$taxon

medians.node <- c(
  medians.taxon[mcc.tree$tip.label,]$frequency,NA,
  medians.internal
)

ggtree(mcc.tree) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_nodepoint(aes(size = medians.node, color = medians.node)) + 
  geom_tippoint(aes(size = frequency, color = medians.node)) + 
  scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
  theme(legend.position = "right")
