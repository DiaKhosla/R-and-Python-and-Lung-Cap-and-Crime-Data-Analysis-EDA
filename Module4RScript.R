# Dia Khosla
# Module 4 
# AlY 6010 

library(MASS)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)


data("cats", package = "MASS")
head(cats)

set.seed(123)
sampled_data <- cats %>%
  group_by(Sex) %>%
  sample_n(size = 1, replace = FALSE)
sampled_data # Show a sample of the data by group

summary(cats) # provides summary of cats dataset 

summary_stats <- cats %>%
  group_by(Sex) %>%
  summarise(
    mean_Bwt = mean(Bwt),
    sd_Bwt = sd(Bwt),
    mean_Hwt = mean(Hwt),
    sd_Hwt = sd(Hwt)
  )
summary_stats

str(cats) # Structure of contains Sex= "F" and "M", body weight (kg), heart weight(g)

male <- subset(cats, Sex == "M")
female <- subset(cats, Sex == "F") # Sub setting data in category wise

female_weight <- cats %>%
  filter(Sex == "F") %>%
  pull(Bwt)

male_weight <- cats %>%
  filter(Sex == "M") %>%
  pull(Bwt)

# Perform t-test
ttest <- t.test(female_weight, male_weight)
ttest



## Welch Two Sample t-test 
#investigates if there is a significant difference between the mean of two independent groups that may have unequal variance

# data:  male$Bwt and female$Bwt

sample_size <- 20. # Sample Testing
set.seed(123)

sample_male <- sample(cats$Bwt[cats$Sex == "M"], size = sample_size)
sample_female <- sample(cats$Bwt[cats$Sex == "F"], size = sample_size)

t_test_sample <- t.test(sample_male, sample_female, var.equal = FALSE)
print(t_test_sample)

ggplot(cats, aes(x = Sex, y = Bwt, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cat Bodyweights by Sex", x = "Sex", y = "Bodyweight (kg)", fill = "Sex") +
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.background = element_rect(fill = "white"), 
    panel.background = element_rect(fill = "white"),  
    legend.position = "none" 
  ) +
  scale_fill_manual(values = c("pink", "blue")) 

### Evaluating whether meditation has an effect on sleep quality based on the provided data
  
## Null Hypothesis (H0): The mean sleeping quality scores before and after the meditation workshop are equal.
# H0: µ1 = µ2 (where µ1 is the mean before meditation and µ2 is the mean after meditation)

## Alternative Hypothesis (HA): The mean sleeping quality scores after meditation are higher than before.
# HA: µ2 > µ1

# Sleeping quality scores 
before <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)

# Perform t-test with significance level α = 0.05
two_sample_meditation <- t.test(before, after, paired = FALSE, alternative = "less", conf.level = 0.95)
print(two_sample_meditation)

# Perform two-sample t-test with significance level α = 0.1
two_sample_meditation_1 <- t.test(before, after, paired = FALSE, alternative = "less", conf.level = 0.90)
print(two_sample_meditation_1)

par(mfrow = c(1, 2))  
boxplot(before, main = "Sleep Before Meditation", ylim = c(0, 10), col = "blue")
boxplot(after, main = "Sleep After Meditation", ylim = c(0, 10), col = "green")

# Paired Test

test_meditation <- t.test(before, after, paired = TRUE, alternative = "less", conf.level = 0.95)
print(test_meditation)

test_meditation_90 <- t.test(before, after, paired = TRUE, alternative = "less", conf.level = 0.90) # Significance level α = 0.1
print(test_meditation_90)

mean_before <- mean(before)
mean_after <- mean(after)

scatter_data <- data.frame(
  Condition = c("Before", "After"),
  MeanSleepQuality = c(mean_before, mean_after)
)

ggplot(scatter_data, aes(x = Condition, y = MeanSleepQuality, group = 1)) +
  geom_point(size = 3, color = "blue") +
  geom_segment(aes(x = "Before", xend = "After", y = mean_before, yend = mean_after),
               linetype = "dashed", color = "green") +
  labs(title = "Mean Sleep Quality Before and After Meditation",
       x = "Condition", y = "Mean Sleep Quality") +
  theme_minimal()



