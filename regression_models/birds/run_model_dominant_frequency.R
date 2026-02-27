require(brms)
require(phytools)

dir.create('output')

mod.type <- commandArgs(trailingOnly = TRUE)[1]

my.seed <- as.integer(commandArgs(trailingOnly = TRUE)[2])

set.seed(my.seed)

trees <- read.nexus('../../BEAST/Coding_mtDNA_HKY_Yule_simple.trees')

n.tree = sample(length(trees),1)

tree <- trees[[n.tree]]

data.df <- read.csv('../../Data_Rhythm_Final_English.csv',sep=';')

bird.orders <- c('Accipitriformes',
                 'Anseriformes',
                 'Apodiformes',
                 'Bucerotiformes',
                 'Cariamiformes',
                 'Casuariiformes',
                 'Charadriiformes',
                 'Colliformes',
                 'Columbiformes',
                 'Coraciiformes',
                 'Cuculiformes',
                 'Eurypygiformes',
                 'Falconiformes',
                 'Galiiformes',
                 'Gaviiformes',
                 'Gruiformes',
                 'Leptosomiformes',
                 'Mesitornithiformes',
                 'Musophagiformes',
                 'Nyctibiiformes',
                 'Opisthocomiformes',
                 'Otidiformes',
                 'Passeriformes',
                 'Phaethontiformes',
                 'Phoenicopteriformes',
                 'Piciformes',
                 'Podecipediformes',
                 'Procellariiformes',
                 'Psittaciformes',
                 'Pterocliformes',
                 'Sphenisciformes',
                 'Strigiformes',
                 'Tinamiformes',
                 'Trogoniformes')

data.df <- droplevels(data.df[data.df$Order %in% bird.orders,])

phylo.cov <- vcv.phylo(tree)

rownames(phylo.cov) <- tolower(gsub(' |_','',rownames(phylo.cov)))
colnames(phylo.cov) <- tolower(gsub(' |_','',colnames(phylo.cov)))

taxon.new <- ifelse(tolower(gsub(' |_','',data.df[,8])) %in% rownames(phylo.cov), 
                    tolower(gsub(' |_','',data.df[,8])), 
                    tolower(gsub(' |_','',data.df[,7])))

taxon.new[taxon.new=='burhinusoedicnemus'] <- 'burhinusbistriatus'
taxon.new[taxon.new=='zaedyuspichyi'] <- 'zaedyuspichiy'
taxon.new[taxon.new=='pantroglodytes'] <- 'pantroglodytestroglodytes'
taxon.new[taxon.new=='chlorocebuspygerythus'] <- 'chlorocebuspygerythrus'
taxon.new[taxon.new=='opisthocomushoazin'] <- 'ophisthocomushoazin'

taxon.data <- data.df[,c('Log_Dominant_Frequency','Log_Weight','Environement','Mastication')]
colnames(taxon.data) <- c('frequency','weight','environment','mastication')
taxon.data$taxon <- as.factor(taxon.new)

taxon.data <- na.omit(taxon.data) #find better way to deal with missing values

taxon.data <- taxon.data[taxon.data$taxon %in% rownames(phylo.cov),]

taxon.data <- droplevels(taxon.data)

phylo.cov <- phylo.cov[levels(taxon.data$taxon),levels(taxon.data$taxon)]

#phylo.dist <- 1 - (phylo.cov/max(phylo.cov))

#taxon.data$weight <- log(taxon.data$weight)

taxon.data$frequency <- 10^(taxon.data$frequency)

taxon.data$weight <- (taxon.data$weight - mean(taxon.data$weight))/sd(taxon.data$weight)

if (mod.type == 'pred.dist') {
  mod.formula <- bf(frequency ~ weight*mastication + environment + 
                      (1 + weight*mastication + environment | gr(taxon, cov = phylo.cov)), 
                    sigma ~ weight*mastication + environment + 
                      (1 + weight*mastication + environment | gr(taxon, cov = phylo.cov)))
}
if (mod.type == 'pred.nodist') {
  mod.formula <- bf(frequency ~ weight*mastication + environment  + 
                      (1 + weight*mastication + environment | gr(taxon, cov = phylo.cov)))
}
if (mod.type == 'nopred.dist') {
  mod.formula <- bf(frequency ~ (1 | gr(taxon, cov = phylo.cov)), 
                    sigma ~ (1 | gr(taxon, cov = phylo.cov)))
}
if (mod.type == 'nopred.nodist') {
  mod.formula <- bf(frequency ~ (1 | gr(taxon, cov = phylo.cov))
  )
}

model <- brm(formula=mod.formula,
             family = lognormal,
             data = taxon.data,
             data2 = list(phylo.cov = phylo.cov),
             cores = 4
)

save.image(paste('output/model_fit_dominant_frequency_',mod.type,'_',my.seed,'.Rdata',sep=''))
