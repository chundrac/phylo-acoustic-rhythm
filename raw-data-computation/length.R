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
  
  SPECIES <- "Polar_Skua"
  
  #Set Sampling Rate in Hz
  
  SR    <- 22050
  
  # Set Wavelength for FFT and Envelope
  
  FFTWL <- 500
  
  #Set Overlap for FFT and 
  
  OVER  <- 80
  
  # Set Parameters for Band-Pass Filter in Hz
  
  LOW   <- 2000
  UP    <- 5000
  
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
  

  
  Directory_Files <- paste("C:/Users/piette/Desktop/Rhythm/Call_Sequences_Final/", SPECIES, "/Voc_",Type,"/", sep = "")
  NFI <- length(list.files((Directory_Files), pattern=(Ib)))
  

  
  for (n in 1:NFI) {
    

    Voc    <- readWave (paste("C:/Users/piette/Desktop/Rhythm/Call_Sequences_Final/",SPECIES,"/Voc_",Type,"/",Ib,"",n,".WAV", sep =""))
    
    nam <- paste ("Length_",s,"_",n,"", sep="")
    
    Length <- length(Voc)
    
    assign (nam, Length/SR)

    } }

Length*
