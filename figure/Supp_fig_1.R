# Load necessary library
library(boot)
library(ggplot2)

# Example dataset
set.seed(42)  # For reproducibility
data <- rnorm(131)  # Replace with your actual dataset

require (dplyr)
library (purrr)

## Load Rhythm Data
setwd("C:/Users/piette/Desktop")

greenposter=rgb(51, 151, 116, maxColorValue = 255)


filename <- paste ("C:/Users/piette/Desktop/Rhythm_Study/Data_Rhythm_Final.csv")

Rhythm <- read.csv(filename, sep=";", dec=",", header=TRUE)

# Create the new dataframe with species and the number of iterations
species_count <- Rhythm %>% count(Common_Name)

# Get species >= 20

filtered_df <- subset(species_count, iterations >= 20)

# Subset from original data
Bonobo <- subset(Rhythm, Common_Name == "Bonobo")

Bonobo <- Bonobo %>% select(Common_Name, Frequency)

Human <- subset(Rhythm, Common_Name == "Human")

Human <- human %>% select(Common_Name, Frequency)

Sea_lion <- subset(Rhythm, Common_Name == "Australian Sea Lion")

Sea_lion <- Sea_lion  %>% select(Common_Name, Frequency)

Baboon <- subset(Rhythm, Common_Name == "Hamadryas Baboon")

Baboon <- Baboon  %>% select(Common_Name, Frequency)
## rhythm comparison


# Bootstrap function to compute the mean
boot_median <- function(data, indices) {
  sample_data <- data[indices]
  return(median(sample_data))
}

# Bootstrap parameters
n_iterations <- 10000
sample_size <- 5
alpha <- 0.05


# Perform bootstrap
boot_results_Bonobo <- boot(Bonobo$Frequency, statistic = boot_median, R = n_iterations)

# Calculate bootstrap confidence intervals
boot_ci_Bonobo <- boot.ci(boot_results_Bonobo , type = "perc", conf = 1 - alpha)

# Prepare data for ggplot2
bootstrap_means_Bonobo <- data.frame(mean = boot_results_Bonobo$t)

# Mean of the full dataset
full_data_median_Bonobo <- median(Bonobo$Frequency)

# Plot the results

b <- ggplot(bootstrap_means_Bonobo, aes(x = mean)) +
  geom_histogram(aes(y=..density..), binwidth = 0.05, fill = "white", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = full_data_median_Bonobo, color = "Full Data Mean"), linetype = "solid", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Bonobo$percent[4], color = "95% CI Lower Bound"), linetype = "dashed", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Bonobo$percent[5], color = "95% CI Upper Bound"), linetype = "dashed", show.legend = FALSE) +
  scale_color_manual(values = c("#339774", "#339774","red")) +
  labs(x = "Median rhythms", y = "density") +
  theme(legend.position='none') +
  theme_classic(base_size=7) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

png(filename="Bootsrap_ci_Bonobo.png", width=4, height=4, units="cm", res=300)
b
dev.off()



# Perform bootstrap
boot_results_Human <- boot(Human$Frequency, statistic = boot_median, R = n_iterations)

# Prepare data for ggplot2
bootstrap_means_Human <- data.frame(mean = boot_results_Human$t)

# Calculate bootstrap confidence intervals
boot_ci_Human <- boot.ci(boot_results_Human , type = "perc", conf = 1 - alpha)

# Mean of the full dataset
full_data_median_Human <- median(Human$Frequency)

# Plot the results
h <- ggplot(bootstrap_means_Human, aes(x = mean)) +
  geom_histogram(aes(y=..density..), binwidth = 0.05, fill = "white", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = full_data_median_Human, color = "Full Data Mean"), linetype = "solid", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Human$percent[4], color = "95% CI Lower Bound"), linetype = "dashed", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Human$percent[5], color = "95% CI Upper Bound"), linetype = "dashed", show.legend = FALSE) +
  scale_color_manual(values = c("#339774", "#339774","red")) +
  labs(x = "Median rhythms", y = "density") +
  theme(legend.position='none') +
  theme_classic(base_size=7) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

png(filename="Human_Bootstrap_ci.png", width=4, height=4, units="cm", res=300)
h
dev.off()




# Perform bootstrap
boot_results_Sea_lion <- boot(Sea_lion$Frequency, statistic = boot_median, R = n_iterations)

# Prepare data for ggplot2
bootstrap_means_Sea_lion <- data.frame(mean = boot_results_Sea_lion$t)

# Calculate bootstrap confidence intervals
boot_ci_Sea_lion <- boot.ci(boot_results_Sea_lion , type = "perc", conf = 1 - alpha)

# Mean of the full dataset
full_data_median_Sea_lion <- median(Sea_lion$Frequency)

s <- ggplot(bootstrap_means_Sea_lion, aes(x = mean)) +
  geom_histogram(aes(y=..density..), binwidth = 0.025, fill = "white", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = full_data_median_Sea_lion, color = "Full Data Mean"), linetype = "solid", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Sea_lion$percent[4], color = "95% CI Lower Bound"), linetype = "dashed", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Sea_lion$percent[5], color = "95% CI Upper Bound"), linetype = "dashed", show.legend = FALSE) +
  scale_color_manual(values = c("#339774", "#339774","red")) +
  labs(x = "Median rhythms", y = "density") +
  theme(legend.position='none') +
  theme_classic(base_size=7) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

s

png(filename="Sea_lion_Bootstrap_ci.png", width=4, height=4, units="cm", res=300)
s
dev.off()

# Perform bootstrap
boot_results_Baboon <- boot(Baboon$Frequency, statistic = boot_median, R = n_iterations)

# Prepare data for ggplot2
bootstrap_means_Baboon <- data.frame(mean = boot_results_Baboon$t)

# Calculate bootstrap confidence intervals
boot_ci_Baboon <- boot.ci(boot_results_Baboon , type = "perc", conf = 1 - alpha)

# Mean of the full dataset
full_data_median_Baboon <- median(Baboon$Frequency)

Ba <- ggplot(bootstrap_means_Baboon, aes(x = mean)) +
  geom_histogram(aes(y=..density..), binwidth = 0.025, fill = "white", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = full_data_median_Baboon, color = "Full Data Mean"), linetype = "solid", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Baboon$percent[4], color = "95% CI Lower Bound"), linetype = "dashed", show.legend = FALSE) +
  geom_vline(aes(xintercept = boot_ci_Baboon$percent[5], color = "95% CI Upper Bound"), linetype = "dashed", show.legend = FALSE) +
  scale_color_manual(values = c("#339774", "#339774","red")) +
  labs(x = "Median rhythms", y = "density") +
  theme(legend.position='none') +
  theme_classic(base_size=7) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
Ba

png(filename="Baboon_Bootstrap_ci.png", width=4, height=4, units="cm", res=300)
Ba
dev.off()

