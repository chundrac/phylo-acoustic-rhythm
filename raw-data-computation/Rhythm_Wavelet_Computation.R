### Computing Dominant Peak Frequency in Vocalisations

## Necessary Packages

library (seewave)
library (WaveletComp)
library (tuneR)

## Necesaary Information

# Species Name

Species <- "Chimpanzee"

# Sampling Rate

SR <- 44100

# Max/Min Frequency for Denoising

Min_Freq <- 100
Max_Freq <- 10000

# Voc Type

Voc_Type <- 1

## Folder Specification

for ( i in 1:12) {

Record_Location  <- paste ("C:/Users/piette/Desktop/Rhythm_Recordings/Recordings/",Species,"/Voc_",Voc_Type,"/", sep="")

Results_Location <- paste ("C:/Users/piette/Desktop/Rhythm_Recordings/Results/",Species,"/Voc_",Voc_Type,"/i",i,"", sep="")

Results_Location_Table <- paste ("C:/Users/piette/Desktop/Rhythm_Recordings/Results/",Species,"/Voc_",Voc_Type,"/Table", sep="")
  
dir.create (Results_Location)

dir.create (Results_Location_Table)

NLN <- list.files(Record_Location, pattern=(paste("i",i,"_", sep="")))

NI <- length (NLN)


for ( n in 1:NI) { 
  
setwd (Results_Location)
    
## Creating Envelope  
  
Seq_name <- paste ("C:/Users/piette/Desktop/Rhythm_Recordings/Recordings/",Species,"/voc_",Voc_Type,"/i",i,"_",n,".WAV", sep="")
Seq <- readWave (Seq_name)

png(filename= paste ("i",i,"_",n,"oscillo.png"), width=1980, height=1080, units="px", res=300)
oscillo (Seq, f=SR, channel=1)
dev.off()

Seq_Filter <- bwfilter(Seq, f=SR, channel = 1, n = 1, from = Min_Freq, to = Max_Freq, bandpass = TRUE, listen = FALSE, output = "matrix")

png(filename= paste ("i",i,"_",n,"oscillo_filter.png"), width=1980, height=1080, units="px", res=300)
oscillo (Seq_Filter, f=SR, channel=1)
dev.off()

Seq_Env_Hil <- env (Seq_Filter, f=SR, envt="hil", norm=TRUE, plot=TRUE)
Seq_Env <- bwfilter (Seq_Env_Hil, f=SR, channel=1, n=4, to=20, listen=FALSE, output="matrix")

png(filename= paste ("i",i,"_",n,"oscillo_filter.png"), width=1980, height=1080, units="px", res=300)
oscillo (Seq_Filter, f=SR, channel=1)
par (new=TRUE)
oscillo (Seq_Env, f=SR, channel=1, colwave = "red", )
dev.off()


Seq_Env_Resamp <- resamp(Seq_Env, f=SR, g=150, channel=1, output="matrix")
Seq_Env_Resamp <- as.data.frame(Seq_Env_Resamp)

## Apply Wavelet Transform (Morlet)

WT <- analyze.wavelet(Seq_Env_Resamp, my.series = 1, loess.span = 0.75,
                dt = 1, dj = 1/20,
                lowerPeriod = 2*1,
                upperPeriod = floor(nrow(Seq_Env_Resamp)/3)*1,
                make.pval = TRUE, method = "white.noise", params = NULL,
                n.sim = 100,
                date.format = NULL, date.tz = NULL,
                verbose = TRUE)



wt.image (WT)

## Extract Important Information from the Transform

Frequmency      <- 1/WT[["Period"]]*150
Avg_Wave_Power <- WT[["Power.avg"]]
Power          <- WT[["Power"]]
Index          <- WT[["axis.1"]]


setwd (Results_Location_Table)

df <- cbind(Frequency,Avg_Wave_Power)
peaks <- fpeaks (df)
peaks <- as.data.frame (peaks, header=TRUE)

name_table <-paste("Peak_Frequency_i",i,"_",n,".csv")
write.table (peaks, file=name_table, sep=";", dec=",")

Power_Spectrum <- plot(Avg_Wave_Power~Frequency, type="l")

png(filename= paste ("i",i,"_",n,"peak.png"), width=1980, height=1080, units="px", res=300)
Power_Spectrum <- plot(Avg_Wave_Power~Frequency, type="l")
dev.off()


}
}

