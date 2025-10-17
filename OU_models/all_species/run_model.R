require(rstan)
require(phytools)

dir.create('output')

mod.type <- commandArgs(trailingOnly = TRUE)[1]

my.seed <- as.integer(commandArgs(trailingOnly = TRUE)[2])

set.seed(my.seed)

trees <- read.nexus('../../BEAST/Coding_mtDNA_HKY_Yule_simple.trees')

n.tree = sample(length(trees),1)

tree <- trees[[n.tree]]

data.df <- read.csv('../../Data_Rhythm_Final_English.csv',sep=';')

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

taxon.data$frequency <- 10^(taxon.data$frequency)
taxon.data <- droplevels(taxon.data)

phy.cov <- vcv.phylo(tree)

coph.dist <- cophenetic.phylo(tree)
coph.dist <- coph.dist / max(coph.dist)

taxon.data$taxon <- factor(taxon.data$taxon,levels=tree$tip.label)
taxon.ID <- as.numeric(taxon.data$taxon)

y <- taxon.data$frequency

B <- nrow(tree$edge)

T = length(tree$tip.label)

data.list <- list(
  N=length(y),
  T=T,
  phy_cov = phy.cov,
  coph_dist = coph.dist,
  taxon_ID=taxon.ID,
  y=y
)

fit <- stan(file=paste('models/',mod.type,'.stan',sep=''),data=data.list,cores=4)

saveRDS(fit,file=paste('output/',mod.type,'_',my.seed,'.RDS',sep=''))