# Module 6 Assignment
# ALY 6010
# By Dia Khosla

install.packages("ggplot2")  # For advanced plotting
install.packages("dplyr")    # For data manipulation
install.packages("tidyr")    # For data tidying
install.packages("MASS")     # For regression analysis

# Loading libraries

library(tidyverse)
library(tibble)
library(ggpubr)
library(rstatix)
library(lubridate)
library(stringr)

# install.packages("readr")
library(readr)
Data <- read_csv("50_Startups.csv")
View(Data)
summary(Data) # finding summary stats

nrow(Data)
ncol(Data)
colnames(Data)

library(ggplot2)
library(dplyr)
library(tidyr) 
# Taking 'State' is a categorical variable in your dataset
# Converting 'State' to factor and creating dummy variables
Data <- Data %>%
  mutate(State = as.factor(State)) %>%
  mutate(State_California = as.numeric(State == "California"),
         State_Florida = as.numeric(State == "Florida"),
         State_NewYork = as.numeric(State == "New York"))

# Rerunning regression with dummy variables
model <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend` + State_California + State_Florida + State_NewYork, data = Data)

# Regression model
summary(model)

# Scatterplot with multiple regression lines
ggplot(Data, aes(x = `R&D Spend`, y = Profit, color = State)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of R&D Spend vs Profit by State",
       x = "R&D Spend",
       y = "Profit",
       color = "State") +
  theme_minimal()


# Separating regression lines for each subset (State)

# California 
model_california <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend`, data = filter(Data, State == "California"))

# Florida 
model_florida <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend`, data = filter(Data, State == "Florida"))

# New York 
model_newyork <- lm(Profit ~ `R&D Spend` + Administration + `Marketing Spend`, data = filter(Data, State == "New York"))

# Comparing the models
summary(model_california)
summary(model_florida)
summary(model_newyork)

correlation_matrix <- cor(Data[, c("R&D Spend", "Administration", "Marketing Spend", "Profit")])
print("Correlation Matrix:")
print(correlation_matrix)

# Using ANOVA to test the significance of State on Profit
anova_model <- aov(Profit ~ State, data = Data)
summary(anova_model)

# Conducting Residual analysis
residuals <- residuals(model)

# Normal Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals)

# Plotting of residuals vs fitted values
plot(fitted(model), residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Plot")
abline(h = 0, col = "red")

# Plotting regression line with 95% confidence interval
plot_regression_line <- function(model, title) {
  ggplot(Data, aes(x = `R&D Spend`, y = Profit)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = title,
         x = "R&D Spend",
         y = "Profit") +
    theme_minimal()
}

# Plotting for state
plot_regression_line(model_california, "Regression Line for State wise- California/ NY/ Florida")

# Plot predictors against Profit
profit <- t(Data$Profit)
X <- colnames(Data)
X <- X[!X %in% c("Profit")]
Xinp <- as.matrix(Data[, X])
Xinp <- cbind(1, Xinp)  # Add bias term

ggplot(Data, aes(x = Profit)) +
  geom_line(aes(y = `R&D Spend`, color = "R&D Spend")) +
  geom_line(aes(y = Administration, color = "Administration")) +
  geom_line(aes(y = `Marketing Spend`, color = "Marketing Spend")) +
  geom_line(aes(y = as.numeric(State), color = "State")) +
  labs(x = "Profit", y = "Predictors") +
  scale_color_manual(name = "Predictors",
                     values = c("R&D Spend" = "blue", 
                                "Administration" = "green", 
                                "Marketing Spend" = "red", 
                                "State" = "purple"),
                     labels = c("R&D Spend", "Administration", "Marketing Spend", "State")) +
  theme_minimal()

