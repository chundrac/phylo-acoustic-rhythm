require(ggplot2)
require(HDInterval)
library(cowplot)
library(doBy)

setwd("C:/Users/piette/Desktop")


### LOAD DATA

{
## Get data

filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Data_Rhythm_Final.csv")

Rhythm <- read.csv(filename, sep=";", dec=",", header=TRUE)

## Get Median Rhythm


Median.Rhythm <- summaryBy(Log_Frequency ~ Common_Name*Log_Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = Rhythm, 
                       FUN = list(median))

colnames(Median.Rhythm) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "Rhythm")


## Read BM vs OU Model comparison

filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Phylogeny/ELPD_Full_Null.csv")

Model_comp <- read.csv(filename, sep=";", dec=",", header=TRUE)

}


### PLOT DATA

{

## Plot ELPD Difference

ELPD <- ggplot(data=Model_comp, aes(x=reorder(Model, -ELPD), y=ELPD))
ELPD <- ELPD + geom_point()
ELPD <- ELPD + scale_color_manual(values = c("black", "black", "black", "black"))
ELPD <- ELPD + labs (title = , x= "Model", y=" ELPD difference ")
ELPD <- ELPD + geom_errorbar(aes(ymin=ELPD-SE, ymax=ELPD+SE))
ELPD <- ELPD + theme_classic(base_size=7)
ELPD <- ELPD + scale_y_reverse()
ELPD <- ELPD + theme (legend.position = "none")
ELPD <- ELPD + scale_x_discrete(labels=c("N dist","F dist", "N ndist", "F ndist"))

ELPD

png(filename="ELPD_Rhythm.png", width=4.1, height=3.7, units="cm", res=300)
ELPD
dev.off()

## Plot Stacking Weight

Stack <- ggplot(data=Model_comp, aes(x=reorder(Model, -Stacking), y=Stacking, fill=Model))
Stack <- Stack + labs (title = , x= "Model", y="Stacking weight")
Stack <- Stack + scale_fill_manual(values = c("black","black","black", "black"))
Stack <- Stack + geom_col()
Stack <- Stack + theme_classic (base_size=7)
Stack <- Stack + theme (legend.position= "none")
Stack <- Stack + scale_x_discrete(labels=c("Full", "Null"))
Stack <- Stack + scale_x_discrete(labels=c("N dist","F dist", "N ndist", "F ndist"))
Stack


png(filename="Stacking_Rhythm.png", width=4.1, height=3.7, units="cm", res=300)
Stack
dev.off()


## Plot PCI

filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Phylogeny/rhythm_CIs.csv")

PCI_Rhythm <- read.csv(filename, sep=",", dec=".", header=TRUE)
PCI_Rhythm <- as.data.frame(PCI_Rhythm)
PCI_Rhythm <- PCI_Rhythm[,4:11]



calculate_stats <- function(column) {
  median_value <- median(column)
  lower_ci_85 <- quantile(column, probs = 0.075)
  upper_ci_85 <- quantile(column, probs = 0.925)
  lower_ci_95 <- quantile(column, probs = 0.025)
  upper_ci_95 <- quantile(column, probs = 0.975)
  return(c(median_value, lower_ci_85, upper_ci_85, lower_ci_95, upper_ci_95))
}



stats <- t(apply(PCI_Rhythm, 2, calculate_stats))


stats_Rhythm <- data.frame(
  Median = stats[, 1],
  Lower_CI_85 = stats[, 2],
  Upper_CI_85 = stats[, 3],
  Lower_CI_95 = stats[, 4],
  Upper_CI_95 = stats[, 5]
)


stats_Rhythm$Parameter <- rownames(stats_Rhythm)
stats_Rhythm$Parameter <- factor(stats_Rhythm$Parameter, levels = c("b_weight","b_masticationYes","b_environmentOpen",
                                                            "b_environmentSemiMClosed",
                                                            "b_environmentShore","b_environmentWater",
                                                            "b_environmentWetland","b_weight.masticationYes"))


MCMC <- ggplot(stats_Rhythm, aes(x = Parameter , y = Median)) +
  geom_point(size = 1)+
  geom_segment(aes(x=1,xend=1, y=-0.3573291, yend=0.13837551), linewidth=0.5) + 
  geom_segment(aes(x=1,xend=1, y=-0.4431210, yend=0.2298341), linewidth=0.25) +
  geom_segment(aes(x=2,xend=2, y=-0.6238020, yend=1.07432465), linewidth=0.5) + 
  geom_segment(aes(x=2,xend=2, y=-1.0290638, yend=1.4321138), linewidth=0.25) +
  geom_segment(aes(x=3,xend=3, y=-1.0318616, yend=0.21277889), linewidth=0.5) + 
  geom_segment(aes(x=3,xend=3, y=-1.4061750, yend=0.5261247), linewidth=0.25) +
  geom_segment(aes(x=4,xend=4, y=-0.4604888, yend=0.56470251), linewidth=0.5) + 
  geom_segment(aes(x=4,xend=4, y=-0.7330821, yend=0.9774881), linewidth=0.25) +  
  geom_segment(aes(x=5,xend=5, y=-0.4364330, yend=0.95537699), linewidth=0.5) + 
  geom_segment(aes(x=5,xend=5, y=-0.8640142, yend=1.3359996), linewidth=0.25) +  
  geom_segment(aes(x=6,xend=6, y=-1.9169498, yend=2.28321476), linewidth=0.5) + 
  geom_segment(aes(x=6,xend=6, y=-3.3707539, yend=3.8001998), linewidth=0.25) +
  geom_segment(aes(x=7,xend=7, y=-1.5147178, yend=1.44980738), linewidth=0.5) + 
  geom_segment(aes(x=7,xend=7, y=-2.5106531, yend=2.3918739), linewidth=0.25) +
  geom_segment(aes(x=8,xend=8, y=-0.7008800, yend=0.00830054), linewidth=0.5) + 
  geom_segment(aes(x=8,xend=8, y=-0.8303196, yend=0.1190029), linewidth=0.25) +
  geom_hline (yintercept=0, linewidth=0.1, linetype="dashed") +
  theme_classic(base_size=7) +
  scale_x_discrete(labels=c("Weight","Mastication","Open","Semi-closed","Shore","Water","Wetland","W*M")) +
  labs (x="Predictors", y="Posterior credible intervals")


MCMC

png(filename="PCI_Rhythm.png", width=8.3, height=4, units="cm", res=300)
MCMC
dev.off()


## Plot Rhythm vs Weight


Rhythm_plot <- ggplot (Median.Rhythm, aes(x=Weight, y=Rhythm, color=Group_Simple))
Rhythm_plot  <- Rhythm_plot  + geom_point()
Rhythm_plot  <- Rhythm_plot  + theme_classic(base_size=7)
Rhythm_plot  <- Rhythm_plot  + scale_color_manual (values = c("#52b09e","#efc475","#d9b7a4"), name="Group")
Rhythm_plot  <- Rhythm_plot  + labs (title = , x= "Weight (log)", y=" Rhythm (log) ")
Rhythm_plot  <- Rhythm_plot  +  theme (legend.position = c (0.8,0.8))
Rhythm_plot 


png(filename="Rhythm_Weight.png", width=7.1, height=7.9, units="cm", res=300)
Rhythm_plot
dev.off()


## Plot Rhythm distribution


Rhythm_dist <- ggplot(Median.R, aes(x = Rhythm, y = Group_Simple, fill=Group_Simple)) + geom_density_ridges2 (rel_min_height = 0.01, linewidth=1.5,jittered_points = TRUE,position = position_points_jitter(width = 0.05, height = 0),
                                                                                                              point_shape = '|', point_colour="Black",point_size = 5, point_alpha = 1, alpha = 0.9)
Rhythm_dist <- Rhythm_dist + scale_fill_manual (values = c("#52b09e","#efc475","#d9b7a4"), name="Group")
Rhythm_dist <- Rhythm_dist + theme_classic (base_size=7)
Rhythm_dist <- Rhythm_dist + labs (title = , x= "Rhythm (Hz)", y="")
Rhythm_dist <- Rhythm_dist + theme (legend.position = c (0.9,0.5))
Rhythm_dist <- Rhythm_dist + coord_cartesian(ylim=c(0.5,5.2), xlim=c(-2,15))
Rhythm_dist <- Rhythm_dist + scale_x_continuous(breaks = round(seq(min(0), max(14), by = 1),1))
Rhythm_dist <- Rhythm_dist + scale_y_discrete(labels=c("1","2","3"))


png(filename="Rhythm_Weight.png", width=7.1, height=7.9, units="cm", res=300)

Rhythm_dist 
dev.off()

Rhythm_dist 

}