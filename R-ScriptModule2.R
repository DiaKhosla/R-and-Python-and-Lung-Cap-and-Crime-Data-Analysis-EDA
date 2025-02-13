# Module 2 ALY6010 
## Week 2
### Dia Khosla

# Load the libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(readr)
library(gmodels)
library(psych)
library(knitr)
library(tidyr)

# Read the data
data <- read.csv("data.csv")
print(head(data))

column_names <- colnames(data)
print(column_names)

nrow(data)
ncol(data)

# Null Values

null_values <- colSums(is.na(data))

print("Null values in each column:")
print(null_values)

# Descriptive statistics for the entire dataset
desc_data <- describe(data)
write.csv(desc_data, file = "descriptive_data.csv")

# Grouping by 'Target' variable and summarizing
group_summary <- data %>%
  group_by(Target) %>%
  summarize(
    Mean = mean(Age.at.enrollment, na.rm = TRUE),
    SD = sd(Age.at.enrollment, na.rm = TRUE),
    Min = min(Age.at.enrollment, na.rm = TRUE),
    Max = max(Age.at.enrollment, na.rm = TRUE),
    N = n()
  )

# Print group summary
print(group_summary)

# Descriptive statistics for the entire sample
summary_data <- summary(data)
summary_table <- as.data.frame(summary_data)
names(summary_table) <- c("Stats", "Age at Enrollment")

# Create the three-line table format
table_format <- kable(summary_table[1:217, ], format = "markdown", align = "c", caption = "Descriptive Statistics for Age at Enrollment")
print(table_format)

# Scatter Plot
scatter_plot <- ggplot(data, aes(x = Age.at.enrollment, y = GDP)) +
  geom_point() +
  labs(x = "Age at Enrollment", y = "GDP", title = "Scatter Plot") +
  geom_hline(yintercept = mean(data$GDP, na.rm = TRUE), col = "red") +
  geom_vline(xintercept = mean(data$Age.at.enrollment, na.rm = TRUE), col = "blue")

print(scatter_plot)
ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 6, dpi = 300)

# Jitter Plot
gender_summary <- table(data$Gender)
gender_summary

# Proportion of Male and Female
prop_male <- round(gender_summary[1] / sum(gender_summary), 2)
prop_female <- round(gender_summary[2] / sum(gender_summary), 2)

# Jitter Plot
jitter_plot <- ggplot(data, aes(x = as.factor(Gender), y = Age.at.enrollment)) +
  geom_jitter() +
  labs(x = "Gender", y = "Age at Enrollment", title = "Jitter Plot") +
  annotate("text", x = c(1, 2), y = c(70, 70), label = c(paste("Male:", prop_male * 100, "%"), 
  paste("Female:", prop_female * 100, "%")), size = 5, color = "darkblue") +
  theme_bw()
jitter_plot
ggsave("jitter_plot.png", plot = jitter_plot, width = 8, height = 6, units = "in", dpi = 300)

# Boxplot
boxplot_plot <- ggplot(data, aes(x = Target, y = Age.at.enrollment)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Target", y = "Age at Enrollment", title = "Boxplot") +
  theme_bw()

print(boxplot_plot)
ggsave("boxplot.png", plot = boxplot_plot, width = 8, height = 6, units = "in", dpi = 300)

# Count plot
count_plot <- ggplot(data, aes(x = Target)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Target Variable") +
  theme_minimal()

ggsave("count_plot.png", plot = count_plot, width = 8, height = 6, units = "in", dpi = 300)

print(count_plot)

# Calculate the percentage of each class
target_distribution <- prop.table(table(data$Target)) * 100
print(target_distribution)

# Histograms with facet grid
data_long <- gather(data, key = "variable", value = "value", c("Unemployment.rate", "Inflation.rate", "GDP"))

histograms <- ggplot(data_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.8) +
  facet_grid(Target ~ variable, scales = "free_y") +
  labs(x = "", y = "Frequency", title = "Histograms of Numerical Features by Target") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(histograms)
ggsave("histograms.png", plot = histograms, width = 8, height = 6, units = "in", dpi = 300)

# Target count
target_counts <- data %>%
  count(Target)

# Plot pie chart for the target variable in the original dataset
custom_colors <- c("lightblue", "lightgrey", "pink")

# Plot pie chart for the target variable in the original dataset

pie_chart <- ggplot(target_counts, aes(x = "", y = n, fill = Target)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Distribution of Target Variable in Original Dataset") +
  geom_text(aes(label = scales::percent(n / sum(n), accuracy = 1)), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = custom_colors)
pie_chart
ggsave("pie_chart.png", plot = pie_chart, width = 8, height = 6, units = "in", dpi = 300)


# Confidence interval

confidence_interval <- t.test(data$Age.at.enrollment, na.rm = TRUE)$conf.int
print(confidence_interval)


numerical_vars <- sapply(data, is.numeric)
confidence_intervals <- lapply(data[, numerical_vars], function(x) {
  t.test(x, na.rm = TRUE)$conf.int
})

# Print the confidence intervals
confidence_intervals_df <- data.frame(
  Variable = names(confidence_intervals),
  Lower = sapply(confidence_intervals, `[[`, 1),
  Upper = sapply(confidence_intervals, `[[`, 2)
)

print(confidence_intervals_df)
