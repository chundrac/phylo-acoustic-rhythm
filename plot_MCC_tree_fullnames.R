require(ape)

mcc.tree <- read.nexus('Coding_mtDNA_HKY_Yule_simple.tree')

pdf('mcc_tree.pdf',width=7,height=7)
plot(mcc.tree,type='fan',cex=.5)
dev.off()