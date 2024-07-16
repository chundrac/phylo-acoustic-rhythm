## GENERAL CRITERIAS

{
  
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
  library(WaveletComp)
  library(dplyr)
  library(Hmisc)
  library(ggpmisc)
  
  #specify species
  
  SPECIES <- "Crested_Gecko"
  
  #Set Sampling Rate in Hz
  
  SR    <- 44100
  
  # Set Wavelength for FFT and Envelope
  
  FFTWL <- 500
  
  #Set Overlap for FFT and 
  
  OVER  <- 80
  
  # Set Parameters for Band-Pass Filter in Hz
  
  LOW   <- 100
  UP    <- 7000
  
  #Set Resample Frequency
  
  RF    <- 150
  
}


  ## ACOUSTIC ANALYSIS

#Individual
for (s in 1:4) {

I  <- paste("i",s,"", sep="")
   
Ib <- paste("i",s,"_", sep="")

#Voc Number

Type<- "1"

# Assign Sound and apply band-pass filter.


Directory_G <- paste("C:/Users/piette/Desktop/Rhythm/Call_Results_Final/", SPECIES, "/Voc_",Type,"/", sep="")
dir.create (Directory_G) 

Directory_Graph <- paste("C:/Users/piette/Desktop/Rhythm/Call_Results_Final/", SPECIES, "/Voc_",Type,"/",I,"", sep="")
dir.create (Directory_Graph)  

Directory_Peak <- paste("C:/Users/piette/Desktop/Rhythm/Call_Results_Final/", SPECIES,"/Voc_",Type,"/Peaks", sep="")
dir.create (Directory_Peak)

Directory_Files <- paste("C:/Users/piette/Desktop/Rhythm/Call_Sequences_Final/", SPECIES, "/Voc_",Type,"/", sep = "")
NFI <- length(list.files((Directory_Files), pattern=(Ib)))

setwd (paste("C:/Users/piette/Desktop/Rhythm/Call_Results_Final/", SPECIES, "/Voc_",Type,"", sep = ""))



for (n in 1:1) {
  
  setwd (Directory_Graph)
  
  
  Voc      <- readWave (paste("C:/Users/piette/Desktop/Rhythm/Call_Sequences_Final/",SPECIES,"/Voc_",Type,"/",Ib,"",n,".WAV", sep =""))
  
  Voc_Fil  <- bwfilter (Voc, f=SR, channel = 1, from = 100, to = 5000, bandpass = TRUE,
                        output="matrix")
  
  subject <- paste("Oscillo_",I,"_",n,".png", sep="")
  png(file=(subject), width=1280, height=720, res=128, units = "px")
  oscillo (Voc, f=SR)
  dev.off()
  
  subject <- paste("Oscillo_Fil_",I,"_",n,".png", sep="")
  png(file=(subject), width=1280, height=720, res=128, units = "px")
  oscillo (Voc_Fil, f=SR)
  dev.off()
  
  
  
  # Compute Sound Envelope by Hilbert Transform and Normalize it
  
  subject <- paste("Hil_",I,"_",n,".png", sep="")
  png(file=(subject), width=1280, height=720, res=128, units = "px")
  oscillo (Voc_Fil, f=SR)
  par (new=TRUE)
  Envelope <- env(Voc_Fil, colwave=2, f=SR, fftw=TRUE, tcl=1, envt="hil", norm=TRUE)
  dev.off()
  
  
  # Smooth Envelope with Bwfilter
  
  S_Env    <- bwfilter(Envelope, f=SR, channel=1, n = 4, to=20, bandpass = TRUE)

  subject <- paste("Env_",I,"_",n,".png", sep="")
  png(file=(subject), width=1280, height=720, res=128, units = "px")
  oscillo (Voc_Fil, f=SR)
  par(new=TRUE)
  oscillo (S_Env, f=SR, colwave="red")
  dev.off()
  
  # Resample the Envelope
  
  R_S_Env  <- resamp(S_Env, f=SR, g=RF, channel = 1, output="matrix")
  
  
  # Compute Power Spectrum of this Envelope
  
  
  FS <- spec(R_S_Env, f=RF, wl=FFTWL)
  
  FS <- cbind(FS[,1]*1000, FS[,2])
  
  subject <- paste("Peak_",I,"_",n,".png", sep="")
  png(file=(subject), width=1280, height=720, res=128, units = "px")
  f  <- fpeaks (FS,f=RF, nmax=10, xlim=c(0,20), xlab= "Frequency (Hz)")
  dev.off()
  
  setwd (Directory_Peak)
  
  subject <- paste("FS_",I,"_",n,".csv", sep="")
  write.table (FS, file=(subject), sep=",", dec=".",col.names=TRUE)  
  
  subject <- paste("Peak_",I,"_",n,".csv", sep="")
  write.table (f, file=(subject), sep=",", dec=".",col.names=TRUE)        } }


