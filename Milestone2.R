read.csv("/Users/unnatigaglani/Downloads/parkinsons_disease_data.csv")

# Load necessary libraries
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# Question 1: Difference in mean MoCA scores between patients with and without Parkinson's disease
# Hypotheses:
# H0: µ1 = µ2 (Mean MoCA scores are the same for both groups)
# H1: µ1 ≠ µ2 (Mean MoCA scores are different for both groups)

# Two-sample t-test
park_data <- data %>% filter(Diagnosis == 1) %>% select(MoCA)
no_park_data <- data %>% filter(Diagnosis == 0) %>% select(MoCA)