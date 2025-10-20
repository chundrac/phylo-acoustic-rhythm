require(ggtree)
require(ggplot2)
require(RColorBrewer)
require(viridis)

#saveRDS(list(mcc.tree,medians.node,medians.taxon,taxon.data,taxon.new), file='tree_ingredients.RDS')

data <- readRDS('tree_ingredients.RDS')

mcc.tree <- data[[1]]
medians.node <- data[[2]]
medians.taxon <- data[[3]]
taxon.data <- data[[4]]
taxon.new <- data[[5]]

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

pdf('reconstruction6.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(color = medians.node, size = medians.node), alpha=.5) + 
  scale_size_continuous(name='Median rhythm value') + 
  scale_color_gradient2(low = brewer.pal(9, "YlGn")[1], midpoint = 6.5, mid = brewer.pal(9, "YlGn")[5], high = brewer.pal(9, "YlGn")[9], name = "Median rhythm value") + 
  theme(legend.position = c(.25,.725))
dev.off()

pdf('reconstruction7.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(color = medians.node, size = medians.node), alpha=.5) + 
  scale_size_continuous(name='Median rhythm value') + 
  scale_color_gradient2(low = brewer.pal(9, "YlGn")[1], midpoint = 6.5, mid = brewer.pal(9, "YlGn")[5], high = brewer.pal(9, "YlGn")[9], name = "Median rhythm value") + 
  theme(legend.position = c(.25,.725)) + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5)
dev.off()

pdf('reconstruction8.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(color = medians.node), alpha=.5) + 
  scale_color_gradient2(low = brewer.pal(9, "YlGn")[1], midpoint = 6.5, mid = brewer.pal(9, "YlGn")[5], high = brewer.pal(9, "YlGn")[9], name = "Median rhythm value") + 
  theme(legend.position = c(.25,.85)) + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5)
dev.off()

pdf('reconstruction9.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(color = medians.node), size=2.5, alpha=.8) + 
  scale_color_viridis(name = "Median rhythm value") + 
  theme(legend.position = c(.25,.85)) + 
  geom_text(aes(label=as.character(round(medians.node,1))),size=1.5, color='orange')
dev.off()

pdf('reconstruction10.pdf',width=7.5,height=7.5)
ggtree(mcc.tree,alpha=.3) %<+% medians.taxon + xlim(-.1, 1) + 
  geom_tiplab(offset = .01, hjust = 0, size=2) + 
  geom_point(aes(color = medians.node), size=2.5, alpha=.8) + 
  scale_color_viridis(name = "Median rhythm value") + 
  theme(legend.position = c(.25,.85))
dev.off()

