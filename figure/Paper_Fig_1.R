## Necessary Packages
library (sjPlot)
library (ggplot2)
library (doBy)
library (GGRidge)
library (ggridges)
library (extrafont)
library (showtext)
library (ggpubr)
library (RColorBrewer)
require (dplyr)
library (purrr)

### LOAD DATA 

{

## Load Rhythm Data
setwd("C:/Users/piette/Desktop")


filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Data_Rhythm_Final.csv")

Rhythm <- read.csv(filename, sep=";", dec=",", header=TRUE)


## Get Median Rhythn

Median.R <- summaryBy(Frequency ~ Scientific_Name*Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = Rhythm, 
                      FUN = list(median))

colnames(Median.R) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "Rhythm")


## Get Median Log Rhythn

Median.R.Log <- summaryBy(Log_Frequency ~ Common_Name*Log_Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = Rhythm, 
                          FUN = list(median))

colnames(Median.R.Log) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "Rhythm")

## Get Median DF

DF <- Rhythm[!(is.na(Rhythm$Log_Dominant_Frequency)),]
DF <- DF[!(is.na(DF$Log_Weight)),]

Median.DF <- summaryBy(Log_Dominant_Frequency ~ Common_Name*Log_Weight*Environement*Group*Group_Simple*Group_All*Order*Mastication*Congregatory*Log_Group_Size, data = DF, 
                       FUN = list(median))

colnames(Median.DF) <- c("Common_Name","Weight","Environment","Group","Group_Simple","Group_All","Order","Mastication","Congregatory", "Group_Size", "DF")

## Merged Median files

Median.DF.small <- subset (Median.DF, select=c("Common_Name", "DF"))

Median.Merge    <- merge(Median.R.Log, Median.DF.small, by=c("Common_Name")) 


## LOAD SNR 

filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/SNR/master_snr_data.csv")

SNR      <- read.csv(filename, sep=";", dec=",", header=TRUE)

## LOAD Length
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Length/master_length_data.csv")

Length   <- read.csv(filename, sep=";", dec=",", header=TRUE)

## LOAD Context
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Context/context.csv")

Context  <- read.csv(filename, sep=";", dec=",", header=TRUE)

## Load Method
filename <- paste("C:/Users/piette/Desktop/Rhythm_Study/Rhythm_Alternative_Methods.csv")

Method   <- read.csv(filename, sep=";", dec=",", header=TRUE)

## creating merged

Merged_SNR    <- merge(Rhythm, SNR, by=c("Common_Name","Recording"))
Merged_Length <- merge(Rhythm, Length, by=c("Common_Name","Recording"))
Merged_All <- merge(Merged_SNR, Merged_Length, by=c("Common_Name","Recording"))

}


### PLOT CONTROLS

{
# Create the new dataframe with species and the number of iterations
species_count <- Rhythm %>% count(Common_Name)

# Rename the columns 
colnames(species_count) <- c("species", "iterations")

# Plot Number of Sequence Per Species

p <-ggplot(data=species_count, aes(x=species, y=iterations)) 
p <- p + geom_bar(stat="identity", fill="black")
p <- p + labs (title = , x= "Species", y="Number of sequence")
p <- p + theme_classic(base_size = 7)
p <- p + theme (axis.text.x = element_blank())
p

png(filename="Species_seq_number.png", width=7, height=3, units="cm", res=300)
p
dev.off()


## plot SNR

SNR_Plot <- ggplot(data = Merged_SNR, aes(x = SNR.x, y = Frequency)) 
SNR_Plot <- SNR_Plot + geom_point(aes(x = SNR.x, y = Frequency ), size=0.5) 
SNR_Plot <- SNR_Plot + theme_classic (base_size=7)
SNR_Plot <- SNR_Plot + labs (title = , x= "SNR (dB)", y="Rhythm (Hz)")
SNR_Plot <- SNR_Plot + theme (legend.position = "none")


png(filename="SNR.png", width=3.5, height=3, units="cm", res=300)
SNR_Plot
dev.off()


## plot length



Length_Plot <- ggplot(data = Merged_Length, aes(x = Length.x, y = Frequency)) 
Length_Plot <- Length_Plot + scale_colour_manual (values = c("#475596","#EBAF21","#6B6045"), name="Group")
Length_Plot <- Length_Plot + geom_point(aes(x = Length.x, y = Frequency ), size=0.5) 
Length_Plot <- Length_Plot + theme_classic (base_size=7)
Length_Plot <- Length_Plot + labs (title = , x= "Length (s)", y=" ")
Length_Plot <- Length_Plot + theme (legend.position = "none")




png(filename="Length.png", width=3.5, height=3, units="cm", res=300)
Length_Plot
dev.off()



## plot Method

Method_Plot <- ggplot(data = Method, aes(x = Species, y = Rhythm, fill=Type))
Method_Plot <- Method_Plot + geom_boxplot()
Method_Plot <- Method_Plot + scale_fill_manual(values=c("#fde725","#35b779","#3e4a89"))
Method_Plot <- Method_Plot + theme_classic (base_size=7)
Method_Plot <- Method_Plot + labs (title = , x= "Species", y="Rhythm (Hz)")
Method_Plot <- Method_Plot + theme (legend.position = c(0.9, 0.8))
Method_Plot <- Method_Plot + scale_x_discrete(labels=c("1","2","3","4","5","6","7","8","9","10"))
Method_Plot <- Method_Plot + coord_cartesian(xlim=c(0.9,13))
Method_Plot <- Method_Plot + theme(legend.key.size = unit(0.3, 'cm'), #change legend key size
                                   legend.key.height = unit(0.3, 'cm'), #change legend key height
                                   legend.key.width = unit(0.3, 'cm'), #change legend key width
                                   legend.title = element_text(size=5), #change legend title font size
                                   legend.text = element_text(size=5)) #change legend text font size
Method_Plot

png(filename="Method.png", width=7, height=3.5, units="cm", res=300)
Method_Plot
dev.off()


}

### PLOT METHOD

{

library (seewave)
require (warbleR)
library (WaveletComp)
library (tuneR)
require (ggplot2)
require (cowplot)
library (lattice)
library (latticeExtra)
library (hrbrthemes)
library (viridisLite)
library (grid)
library (gtable)


## Get oscillo data and env

filename <- paste ("C:/Users/piette/Desktop/Rhythm_Recordings/Recordings/Polar_Skua/Voc_1/i1_2.WAV")

Seq=readWave(filename)

SR <- 22500
Min_Freq <- 100
Max_Freq <- 10000
FFTWL <- 500
OVER  <- 80
LOW   <- 100
UP    <- 10000
RF    <- 150

Seq_Filter <- bwfilter(Seq, f=SR, channel = 1, n = 1, from = Min_Freq, to = Max_Freq, bandpass = TRUE, listen = FALSE, output = "matrix")
Seq_Env_Hil <- env (Seq_Filter, f=SR, envt="hil", norm=TRUE, plot=F)
Seq_Env <- bwfilter (Seq_Env_Hil, f=SR, channel=1, n=4, to=20, listen=FALSE, output="matrix")


Seq_Filter=as.data.frame(Seq_Filter)
Seq_Filter$x=1:nrow(Seq_Filter)/SR

Seq_Env=as.data.frame(Seq_Env)
Seq_Env$x=1:nrow(Seq_Env)/SR

Seq_Filter$V1=scales::rescale_mid(Seq_Filter$V1, to=c(-1,1), mid=0)
Seq_Env$V1=scales::rescale(Seq_Env$V1, to=c(0,1))

## Plot Envelope and Oscillo

greenposter=rgb(51, 151, 116, maxColorValue = 255)

p1=ggplot()+geom_line(data = Seq_Filter, aes(y=V1, x=x))+geom_line(data = Seq_Env, aes(y=V1, x=x), size=1, color=greenposter)+xlim(0, 1.5)
p1=p1+xlab('Time (s)')+ylab(NULL)+theme_classic(base_size = 7)+theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))



p1

setwd("C:/Users/piette/Desktop")


png(filename="Oscillo.png", width=4.51, height=3.59, units="cm", res=300)
p1
dev.off()



## Plot Power spectrum and time frenquency represention


Voc      <- readWave (filename)


Voc_Fil  <- bwfilter (Voc, f=SR, channel = 1, from = 100, to = 5000, bandpass = TRUE,
                      output="matrix")

Envelope <- env(Voc_Fil, colwave=2, f=SR, tcl=1, envt="hil", norm=TRUE)
S_Env    <- bwfilter(Envelope, f=SR, channel=1, n = 4, to=20, bandpass = TRUE)
R_S_Env  <- resamp(S_Env, f=SR, g=RF, channel = 1, output="matrix")
t <- oscillo(R_S_Env, f=RF)
tf <- as.data.frame(t)

WT = analyze.wavelet(tf, loess.span = 0, dt = 1, dj = 1/20,
                     make.pval = TRUE, method = "white.noise", params = NULL,
                     n.sim = 100, date.format = "%Y", date.tz = NULL, verbose = TRUE)

FreqA  <- WT[["Period"]]
IndexA <- WT[["axis.1"]]
AmpA   <- WT[["Power"]]
Amp    <- WT[["Power.avg"]]
FreqA  <- (1/FreqA)*150


tab <- expand.grid(FreqA = FreqA, IndexA= IndexA)[,2:1]
tab$Amp <- c(AmpA)

col.l <- viridis(50)

p2 <- levelplot (AmpA ~ IndexA * FreqA, data=tab, ylim=c(2,20), zlim=c(0,2),
                 col.regions=col.l, cuts=10, pretty=TRUE, maxpixels = 1e6,
                 xlab= "", ylab= "", main ="",
                 par.settings=list(panel.background=list(col="skyblue")))


png(filename="Time_freq.png")
p2
dev.off()

VA <- as.vector(Amp)
VB <- as.vector(FreqA)

MM <- cbind(VB,VA)
MM <- as.data.frame(MM)


p3 <- ggplot(data=MM, aes(x=VB, y=VA)) + geom_line()
p3 <- p3 + theme_classic (base_size = 7)
p3 <- p3 + scale_x_continuous(position = "top", limits = c(1,20))
p3 <- p3 + scale_y_continuous(trans = "reverse",breaks=c(0.250, 0.750))
p3 <- p3 + labs (title = , x= "Rhythm (Hz)", y="Amplitude")

p3

p3_rotated <- p3 + coord_flip()


p3_rotated

setwd("C:/Users/piette/Desktop")

png(filename="PS_rotated.png", width=2.32, height=3.4, units="cm", res=300)
p3_rotated
dev.off()



## Plot Power spectrum for all individuals

## GENERAL CRITERIAS

{
  library (tidyverse)
  library (seewave)
  library (tuneR)
  library (audio)
  library (rgl)
  library (ggplot2)
  library (soundgen)
  library (rpanel)
  library (fftw)
  library (eegkit)
  library (spectral)
  
  #specify species
  
  SPECIES<- "Polar_Skua"
  IND <- "i1"
  
  
}

#Individual

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i1_1.csv")
FS_i1_1 <- read.csv(filename, sep=",", dec=".", header=TRUE)

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i1_2.csv")
FS_i1_2 <- read.csv(filename, sep=",", dec=".", header=TRUE)

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i1_3.csv")
FS_i1_3 <- read.csv(filename, sep=",", dec=".", header=TRUE)

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i1_4.csv")
FS_i1_4 <- read.csv(filename, sep=",", dec=".", header=TRUE)

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i2_1.csv")
FS_i2_1 <- read.csv(filename, sep=",", dec=".", header=TRUE)

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i3_1.csv")
FS_i3_1 <- read.csv(filename, sep=",", dec=".", header=TRUE)

filename <- paste ("C:/Users/piette/Desktop/Polar_Skua/Voc_1/Peaks/FS_i4_1.csv")
FS_i4_1 <- read.csv(filename, sep=",", dec=".", header=TRUE)


p4 <- ggplot() + geom_line(data=FS_i1_1, aes(x=VBF, y=VA))
p4 <- p4 + geom_line(data=FS_i1_2, aes(x=VBF, y=VA))
p4 <- p4 + geom_line(data=FS_i1_3, aes(x=VBF, y=VA))
p4 <- p4 + geom_line(data=FS_i1_4, aes(x=VBF, y=VA))
p4 <- p4 + geom_line(data=FS_i2_1, aes(x=VBF, y=VA))
p4 <- p4 + geom_line(data=FS_i3_1, aes(x=VBF, y=VA))
p4 <- p4 + geom_line(data=FS_i4_1, aes(x=VBF, y=VA))
p4 <- p4 + labs (title = , x= "Rhythm (Hz)", y="Amplitude")
p4 <- p4 + theme_classic (base_size = 7)
p4 <- p4 + xlim (1,20)

p4



png(filename="Ps_all.png", width=4.51, height=3.59, units="cm", res=300)
p4
dev.off()


}
