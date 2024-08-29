require(rstan)
require(phytools)
require(phangorn)
require(condMVNorm)
require(ggtree)
require(ggplot2)
require(RColorBrewer)

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

#saveRDS(y.internal.all,file='internal_reconstructions.RDS')

#y.internal.all <- readRDS('internal_reconstructions.RDS')

medians.internal <- apply(y.internal.all,2,median)

medians.taxon <- aggregate(frequency ~ taxon, FUN=median, taxon.data)
medians.taxon <- as.data.frame(medians.taxon)
rownames(medians.taxon) <- medians.taxon$taxon

medians.node <- c(
  medians.taxon[mcc.tree$tip.label,]$frequency,2.9,
  medians.internal
)

#ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
#  geom_tiplab(offset = .01, hjust = 0, size=2) + 
#  geom_nodepoint(aes(size = medians.node, color = medians.node),alpha=.3) + 
#  geom_tippoint(aes(size = frequency, color = medians.node)) + 
#  geom_text(aes(label=as.character(round(medians.node,3))),size=1) + 
#  scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
#  theme(legend.position = "right")

mcc.tree$tip.label <- c('Acanthisitta chloris',
'Alectoris chukar',
'Ameerega hahneli',
'Anthoscopus minutus',
'Apis mellifera',
'Aprasia parapulchella',
'Aquila heliaca',
'Ardeotis kori',
'Bidyanus bidyanus',
'Burhinus bistriatus',
'Cacatua moluccensis',
'Canis lupus familiaris',
'Cariama cristata',
'Cavia porcellus',
'Cervus elaphus',
'Chlorocebus pygerythrus',
'Chorthippus fallax',
'Cinclosoma punctatum',
'Cisticola juncidis',
'Columba livia',
'Cormobates leucophaea',
'Corythaeola cristata',
'Crypturellus cinnamomeus',
'Cuculus poliocephalus',
'Cygnus olor',
'Cynomys ludovicianus',
'Dacelo novaeguineae',
'Daption capense',
'Desmodus rotundus',
'Donacobius atricapilla',
'Dromaius novaehollandiae',
'Dryoscopus gambensis',
'Eptesicus serotinus',
'Equus caballus',
'Eurystomus gularis',
'Falco peregrinus',
'Gavia stellata',
'Gryllus bimaculatus',
'Haematopus ostralegus',
'Helarctos malayanus',
'Hippopotamus amphibius',
'Homo sapiens',
'Jacana jacana',
'Lepidothrix coronata',
'Lycaon pictus',
'Macaca mulatta',
'Marmota flaviventris',
'Megaptera novaeangliae',
'Melanocharis versteri',
'Merops viridis',
'Mesitornis unicolor',
'Mus musculus',
'Myadestes myadestinus',
'Neophoca cinerea',
'Nipponia nippon',
'Numenius phaeopus',
'Nyctibius griseus',
'Ochotona dauurica',
'Origma solitaria',
'Pan paniscus',
'Pan troglodytes troglodytes',
'Panthera leo',
'Panthera onca',
'Papio hamadryas',
'Paradisaea raggiana',
'Passer domesticus',
'Pelobates fuscus',
'Phoenicopterus roseus',
'Phoeniculus purpureus',
'Phyllobates terribilis',
'Phylloscopus trochilus',
'Picus canus',
'Piranga olivacea',
'Podiceps cristatus',
'Pomponia linearis',
'Pongo pygmaeus',
'Procavia capensis',
'Pterocles burchelli',
'Rana temporaria',
'Rhynochetos jubatus',
'Sclerurus mexicanus',
'Sitta europaea',
'Sorex araneus',
'Spheniscus magellanicus',
'Stercorarius parasiticus',
'Strix leptogrammica',
'Sus scrofa domesticus',
'Trogon melanurus',
'Tupaia belangeri',
'Turnagra capensis',
'Urocolius indicus',
'Zaedyus pichiy')

#ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
#  geom_tiplab(offset = .01, hjust = 0, size=2) + 
#  #geom_nodepoint(aes(size = medians.node, color = medians.node),alpha=.3) + 
#  #geom_tippoint(aes(size = frequency, color = medians.node)) + 
#  geom_text(aes(label=as.character(round(medians.node,1)),color=medians.node),size=1.5) + 
#  scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
#  theme(legend.position = "right") + scale_color_distiller(palette='YlGn')

#ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
#  geom_tiplab(offset = .01, hjust = 0, size=2) + 
#  geom_nodepoint(aes(size = medians.node, color = medians.node),alpha=.3) + 
#  geom_tippoint(aes(size = frequency, color = medians.node[1:T])) + 
#  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5) + 
#  scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
#  theme(legend.position = "right") + scale_color_distiller(palette='YlGn')

#ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
#  geom_tiplab(offset = .01, hjust = 0, size=2) + 
#  geom_nodepoint(aes(size = medians.node, color = medians.node),alpha=.3) + 
#  geom_text(aes(label=as.character(round(medians.node,1)), color = medians.node),size=1.5) + scale_color_distiller(palette='YlGn')

#ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
#  geom_tiplab(offset = .01, hjust = 0, size=2) + 
#  geom_nodepoint(aes(color = medians.node),alpha=.3) + 
#  geom_tippoint(aes(color = frequency),alpha=.3) + 
#  scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
#  scale_color_distiller(palette='YlGn') + 
#  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5)

pdf('reconstruction1.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_nodepoint(aes(size = medians.node, color = medians.node),alpha=.3) + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5) + scale_color_distiller(palette='BuGn')
dev.off()

pdf('reconstruction2.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_nodepoint(aes(size = medians.node, color = medians.node),alpha=.3) + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5) + scale_color_distiller(palette='YlGn')
dev.off()

pdf('reconstruction3.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(size = medians.node, color = medians.node),alpha=.3) + 
  #scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
  scale_color_distiller(palette='BuGn') + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5)
dev.off()

pdf('reconstruction4.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(size = medians.node, color = medians.node),alpha=.3) + 
  #scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
  scale_color_distiller(palette='YlGn') + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5)
dev.off()

pdf('reconstruction5.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(color = medians.node),size=.8,alpha=.9) + 
  #scale_size_continuous(name='median frequency',range = c(.5, 2)) + 
  scale_color_gradient2(low = brewer.pal(9, "YlGn")[1], midpoint = 6.5, mid = brewer.pal(9, "YlGn")[5], high = brewer.pal(9, "YlGn")[9], name = "Median rhythm value") + 
  theme(legend.position = c(.25,.85))
dev.off()
