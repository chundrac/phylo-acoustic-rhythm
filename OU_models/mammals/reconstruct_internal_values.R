require(rstan)
require(phytools)
require(phangorn)
require(condMVNorm)

trees <- read.nexus('../../BEAST/Coding_mtDNA_HKY_Yule_simple.trees')

mcc.tree <- maxCladeCred(trees)

orig.taxa <- mcc.tree$tip.label

taxon.key <- c('Acanthisitta chloris',
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

data.df <- read.csv('../../Data_Rhythm_Final_English.csv',sep=';')

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

names(taxon.key) <- mcc.tree$tip.label

data.df <- read.csv('../../Data_Rhythm_Final_English.csv',sep=';')

mammal.orders<- c('Artiodactyla',
                  'Carnivora',
                  'Chiroptera',
                  'Cingulata',
                  'Eulipotyphla',
                  'Hyracoidea',
                  'Lagomorpha',
                  'Perissodactyla',
                  'Primates',
                  'Rodentia',
                  'Scadentia',
                  'Sciuromorpha')

data.df <- data.df[data.df$Order %in% mammal.orders,]

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
  
  data.df <- read.csv('../../Data_Rhythm_Final_English.csv',sep=';')
  
  mammal.orders<- c('Artiodactyla',
                    'Carnivora',
                    'Chiroptera',
                    'Cingulata',
                    'Eulipotyphla',
                    'Hyracoidea',
                    'Lagomorpha',
                    'Perissodactyla',
                    'Primates',
                    'Rodentia',
                    'Scadentia',
                    'Sciuromorpha')
  
  data.df <- data.df[data.df$Order %in% mammal.orders,]
  
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

medians.internal <- apply(y.internal.all,2,median)

medians.taxon <- aggregate(frequency ~ taxon, FUN=median, taxon.data)
medians.taxon <- as.data.frame(medians.taxon)
rownames(medians.taxon) <- medians.taxon$taxon

medians.node <- c(
  medians.taxon[mcc.tree$tip.label,]$frequency,2.9,
  medians.internal
)


mcc.tree$tip.label <- taxon.key[mcc.tree$tip.label]

saveRDS(list(mcc.tree,medians.node,medians.taxon,taxon.data,taxon.new), file='tree_ingredients.RDS')