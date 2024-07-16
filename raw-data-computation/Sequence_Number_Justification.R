require (dplyr)
library (purrr)

## Load Rhythm Data
setwd("C:/Users/piette/Desktop")


filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Data_Rhythm_Final.csv")

Rhythm <- read.csv(filename, sep=";", dec=",", header=TRUE)


# Create the new dataframe with species and the number of iterations
species_count <- Rhythm %>% count(Common_Name)

# Rename the columns 
colnames(species_count) <- c("species", "iterations")

# plot

p <-ggplot(data=species_count, aes(x=species, y=iterations)) 
p <- p + geom_bar(stat="identity", fill="black")
p <- p + labs (title = , x= "Species", y="Number of sequence")
p <- p + theme_classic(base_size = 7)
p <- p + theme (axis.text.x = element_blank())
p

png(filename="Species_seq_number.png", width=7, height=3, units="cm", res=300)
p
dev.off()

## rhythm comparison

Bonobo <- subset(Rhythm, Common_Name == "Bonobo")

Bonobo <- Bonobo %>% select(Common_Name, Frequency)


# Set seed for reproducibility
set.seed(123)

# Define a function to perform one iteration
one_iteration <- function(data) {
  # Randomly sample 5 rows from data
  sample_data <- sample_n(data, 50)
  
  # Calculate the median rhythm of the sampled data
  sampled_median <- median(sample_data$Frequency)
  
  # Calculate the overall median rhythm of the entire data
  overall_median <- median(data$Frequency)
  
  # Perform Wilcoxon signed-rank test
  test_result <- wilcox.test(sample_data$Frequency,data$Frequency)
  
  # Return the p-value from the test
  return(test_result$p.value)
}

# Step 4: Perform 1000 iterations and collect p-values
num_iterations <- 1000
p_values <- replicate(num_iterations, one_iteration(Bonobo))

adjusted_p_values <- p.adjust(p_values, method = "fdr")

# Step 5: Calculate percentage of p-values less than or equal to 0.05
percentage_significant <- mean(adjusted_p_values <= 0.05) * 100

# Step 6: Print the results
cat(sprintf("Percentage of iterations where sampled median rhythm <= overall median rhythm: %.2f%%\n", percentage_significant))
