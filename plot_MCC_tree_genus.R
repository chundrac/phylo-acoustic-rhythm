require(ape)

mcc.tree <- read.nexus('Coding_mtDNA_HKY_Yule_simple.tree')

tip.labels.new <- sapply(mcc.tree$tip.label,function(x){strsplit(x,'_')[[1]][1]})

mcc.tree$tip.label <- tip.labels.new

pdf('mcc_tree_genus.pdf',width=7,height=7)
plot(mcc.tree,type='fan',cex=.5)
dev.off()