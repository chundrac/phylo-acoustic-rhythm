# Load the required libraries
install.packages("tuneR")
install.packages("signal")
library(tuneR)
library(signal)

# Create a function to calculate SNR for a single audio file
calculateSNR <- function(audio_file_path) {
  audio_file <- readWave(audio_file_path)
  audio_file <- bwfilter (Voc, f=44100, channel = 1, from = 100, to = 5000, bandpass = TRUE,
                          output="matrix")
  
  noise_samples <- 0.05 * 44100  # Adjust to your desired noise duration
  
  # Extract the first 'noise_samples' as the noise region
  noise_region <- audio_file[1:noise_samples]
  
  # Extract the signal (remove the first 'noise_samples' as noise)
  signal_region <- audio_file[-(1:noise_samples)]
  
  snr <- 10 * log10(sum(signal_region^2) / sum(noise_region^2))
  
  return(snr)
}

# Replace 'your_data_directory' with the root directory containing your audio recordings
root_directory <- "C:/Users/piette/Desktop/Rhythm_Recordings/Recordings/"

# Get a list of subdirectories (species)
species_directories <- list.dirs(root_directory, full.names = FALSE, recursive = FALSE)

# Loop through species directories
for (species_dir in species_directories) {
  # Create an empty data frame to store SNR values for this species
  snr_data <- data.frame(Recording = character(0), SNR_dB = numeric(0))
  
  # Get a list of audio files for this species
  audio_files <- list.files(file.path(root_directory, species_dir, "Voc_1"), full.names = TRUE, pattern = ".wav")
  
  # Loop through audio files
  for (audio_file_path in audio_files) {
    recording_name <- tools::file_path_sans_ext(basename(audio_file_path))
    snr_value <- calculateSNR(audio_file_path)
    
    # Add the SNR value to the data frame
    snr_data <- rbind(snr_data, data.frame(Recording = recording_name, SNR_dB = snr_value))
  }
}
  
  
  # Create a CSV file for this species
  species_name <- basename(species_dir)
  output_file <- file.path(root_directory, paste0(species_name, "_SNR.csv"))
  write.csv(snr_data, file = output_file, row.names = FALSE)
}