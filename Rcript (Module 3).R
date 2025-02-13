# Dia Khosla  
# ALY6010.MERGED.81239.81386.202435
# AssignmentsModule 3 R Practice

library(ggplot2)

# Load the data
library(readr)
Heart_Attack <- read_csv("Desktop/Module 3/Heart Attack Data Set.csv")
View(Heart_Attack)
nrow(Heart_Attack)
ncol(Heart_Attack)

library(dplyr)
# View the first few rows of the dataset
head(Heart_Attack)

# Data Cleaning
library(magrittr)
sum(is.na(Heart_Attack))
Heart_Attack %>% filter_all(any_vars(is.na(.)))
str(Heart_Attack)

# One-Sample T-Test
# Assume we are testing the mean age of patients (replace 'Age' with the actual column name in your dataset)
# Null Hypothesis: The mean age of patients is 50.
# Alternative Hypothesis: The mean age of patients is not 50.

age <- Heart_Attack$age  # Replace 'Age' with the actual column name

# Performing t-test
test_result1 <- t.test(age, mu = 50)

print(test_result1)

# Reject the null hypothesis (Average age of individuals in the dataset is different from 50 years)
# Mean age is not 50 years as initially assumed, but rather closer to 54.42 years
# Small p-value (like 9.559e-16 as p-value less than 0.05) suggests strong evidence against the null hypothesis
# p value for 9.559e-16 (0.0000000000000009559)
# 95% confident that the true population mean age falls between 53.39 and 55.45 years

# Hypothesis Testing for P-Value
# Assume we are testing the proportion of patients with high cholesterol (replace 'Cholesterol' with actual column name)
# Null Hypothesis: The proportion of patients with high cholesterol is 0.5.
# Alternative Hypothesis: The proportion of patients with high cholesterol is not 0.5.


# Calculate the proportion of patients with high cholesterol
minimum_chol <- min(Heart_Attack$chol)
maximum_chol <- max(Heart_Attack$chol)

minimum_chol
maximum_chol
# Calculate the proportion of patients with high cholesterol
cholesterol_high <- Heart_Attack$chol > 300  
test_result2 <- prop.test(sum(cholesterol_high), length(cholesterol_high), p = 0.5)
test_result2

# Reject the null hypothesis
# Concluded that the true proportion of patients with high cholesterol is significantly different from 0.5. 
# Approximately 14.24% of patients in the dataset have cholesterol levels above 300.

cholesterol_low <- Heart_Attack$chol < 200  
test_result3 <- prop.test(sum(cholesterol_low), n = length(cholesterol_low), p = 0.5)
test_result3

# The null hypothesis states that the proportion of patients with low cholesterol is 0.5 (50%)
# Given the very low p-value, we reject the null hypothesis 
# Approximately 16.23% of patients have cholesterol levels below 200. (Proportion of 0.1622517)
# 95% confidence interval (0.12 to 0.21) indicates that true proportion of patients with low cholesterol b/w 12.00% and 21.00%


# Correlations between risk factors and age 
correlations <- cor(Heart_Attack[, c("age", "chol", "trestbps", "thalach", "oldpeak", "ca")])
print(correlations)

library(ggplot2)

# Scatter plot 1: Age vs. Cholesterol Levels
plot1 <- ggplot(Heart_Attack, aes(x = age, y = chol)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age",
       y = "Cholesterol Levels",
       title = "Relationship Between Age and Cholesterol Levels") +
  theme_minimal()

ggsave("plot1.png", plot = plot1 , width = 8, height = 6, units = "in", dpi = 300)

# Scatter plot 2: Age vs. Resting Blood Pressure (trestbps)
plot2 <- ggplot(Heart_Attack, aes(x = age, y = trestbps)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "Resting Blood Pressure (trestbps)", title = "Age vs Resting Blood Pressure") +
  theme_minimal()

# Save plot 2 as JPG with specific settings
ggsave(filename = "Age_vs_RestingBP.jpg", plot = plot2, width = 8, height = 6, units = "in", dpi = 300)

# Scatter plot 3: Age vs. Max Heart Rate (thalach)
plot3 <- ggplot(Heart_Attack, aes(x = age, y = thalach)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "Max Heart Rate (thalach)", title = "Age vs Max Heart Rate") +
  theme_minimal()

# Save plot 3 as JPG with specific settings
ggsave(filename = "Age_vs_MaxHeartRate.jpg", plot = plot3, width = 8, height = 6, units = "in", dpi = 300)

# Scatter plot 4: Age vs. ST Depression (oldpeak)
plot4 <- ggplot(Heart_Attack, aes(x = age, y = oldpeak)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "ST Depression (oldpeak)", title = "Age vs ST Depression") +
  theme_minimal()

# Save plot 4 as JPG with specific settings
ggsave(filename = "Age_vs_STDepression.jpg", plot = plot4, width = 8, height = 6, units = "in", dpi = 300)

# Scatter plot 5: Age vs. Number of Major Vessels (ca)
plot5 <- ggplot(Heart_Attack, aes(x = age, y = ca)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "Number of Major Vessels (ca)", title = "Age vs Number of Major Vessels") +
  theme_minimal()

# Save plot 5 as JPG with specific settings
ggsave(filename = "Age_vs_NumberOfVessels.jpg", plot = plot5, width = 8, height = 6, units = "in", dpi = 300)


# logistic regression model
model <- glm(target ~ age + chol + trestbps + thalach + oldpeak + ca,
             data = Heart_Attack,
             family = binomial)

summary(model)
