require(phytools)
require(phangorn)

#read in the trees
trees <- read.nexus('BEAST/Coding_mtDNA_HKY_Yule_simple.trees')

#make MCC tree
mcc.tree <- maxCladeCred(trees)

#write the MCC tree
write.nexus(mcc.tree,file='Coding_mtDNA_HKY_Yule_simple.tree')

#plot(mcc.tree,type='fan',cex=.5)