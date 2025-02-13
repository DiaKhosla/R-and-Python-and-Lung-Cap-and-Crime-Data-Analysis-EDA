# Module 1 ALY6010 
## Week 1
### Dia Khosla

# Import necessary libraries

install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("lubridate")
install.packages("readr")
install.packages("gmodels")

library(dplyr) # data manipulation
library(ggplot2) # create visualizations
library(plotly) # create plots
library(lubridate) # handles data complexity
library(readr) # reads csv files

# Reading the csv and finding summary

Netflix_Dataset <- read.csv("Netflix Dataset.csv") # reading csv
Netflix_Dataset$Date <- as.Date(Netflix_Dataset$Date) # Applying data structures
Netflix_Dataset[, 2:6] <- sapply(Netflix_Dataset[, 2:6], as.numeric) # making them numerical
print(Netflix_Dataset)

summary(Netflix_Dataset) # finding summary

library(lubridate) # handles data complexity
Netflix_Dataset$Date <- ymd(Netflix_Dataset$Date) # adjusting dates
head(Netflix_Dataset) # verifying

# there are no missing values
sum(is.na(Netflix_Dataset)) # Check for missing values # there are no missing values 

year_freq <- table(year(Netflix_Dataset$Date)) # Generating frequency table for date variable
print(year_freq)

# Generating cross-tabulations 
library(gmodels)
Netflix_Dataset$Volume_Category <- cut(Netflix_Dataset$Volume, 
                                       breaks = 5, 
                                       labels = c("Very Low", "Low", "Medium", "High", "Very High"))
Netflix_Dataset$Adj_Close_Category <- cut(Netflix_Dataset$Adj.Close, 
                                          breaks = 5)
Adj_Close_Volume_cross <- CrossTable(Netflix_Dataset$Volume_Category, Netflix_Dataset$Adj_Close_Category)
print(Adj_Close_Volume_cross) # Binning Adj.Close into categories

# Convert Date column to Date type
Netflix_Dataset$Date <- as.Date(Netflix_Dataset$Date)

# Finding High Volume indicator
volume_threshold <- mean(Netflix_Dataset$Volume)

# Add a column to indicate high (1) or low (0) trading volume
Netflix_Dataset$High_Volume <- ifelse(Netflix_Dataset$Volume > volume_threshold, 1, 0)

# Display the first few rows to verify
head(Netflix_Dataset)

# Finding daily returns
library(dplyr)
# Calculating daily returns
Netflix_Dataset <- mutate(Netflix_Dataset, Daily_Return = (Close - lag(Close))/lag(Close))
head(Netflix_Dataset)

# Time series plot for Date
ggplot(Netflix_Dataset, aes(x = Date, y = Open)) +
  geom_line(color = "blue") +
  labs(title = "Netflix Stock Opening Prices Over Time",
       x = "Date",
       y = "Open")  +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))

# Plots for Netflix Stock Close Prices Over Time 

ggplot(Netflix_Dataset, aes(x = Date, y = Close)) +
  geom_line(color = "brown") +
  labs(title = "Netflix Stock Close Prices Over Time",
       x = "Date",
       y = "Close Price") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))


# Line plots for Netflix Stock Prices Over Time
ggplot(Netflix_Dataset, aes(x = Date)) +
  geom_line(aes(y = Open), color = "green", linetype = "dashed") +
  geom_line(aes(y = High), color = "red") +
  geom_line(aes(y = Low), color = "blue") +
  geom_line(aes(y = Close), color = "pink") +
  labs(title = "Netflix Stock Prices Over Time",
       x = "Date",
       y = "Price") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))

ggplot(subset(Netflix_Dataset, year(Date) >= 2022), aes(x = Date, y = Adj.Close)) +
  geom_line(color = "purple") +
  labs(title = "Netflix Stock Adjusted Close Prices from 2022 Onwards",
       x = "Date",
       y = "Adjusted Close Price")

# Line plots for Netflix Stock Trading Volume Over Time
library(ggplot2)
library(scales)

ggplot(Netflix_Dataset, aes(x = Date, y = Volume)) +
  geom_line(color = "darkblue") +  # Line plot
  labs(title = "Netflix Stock Trading Volume Over Time",
       x = "Date",
       y = "Volume") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))

library(ggplot2)
library(scales)

first_quarter_2024 <- subset(Netflix_Dataset, Date >= as.Date("2024-01-01") & Date < as.Date("2024-04-01"))
total_volume <- sum(first_quarter_2024$Volume)

ggplot(first_quarter_2024, aes(x = Date, y = Volume)) +
  geom_line(color = "blue") +
  annotate("text", x = as.Date("2024-03-15"), y = max(first_quarter_2024$Volume), 
           label = paste("Total Volume:", comma(total_volume)), color = "black", size = 4) +
  labs(title = "Netflix Stocks Volume in First Quarter of 2024",
       x = "Date",
       y = "Volume") +
  scale_y_continuous(labels = comma) +  # Formatting volume numbers with commas
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))

# Plot box plot of opening prices by year
Netflix_Dataset$Date <- as.Date(Netflix_Dataset$Date)
Netflix_Dataset$Year <- format(Netflix_Dataset$Date, "%Y")
Netflix_Dataset$Month <- format(Netflix_Dataset$Date, "%B")

# Subset data for the year 2024
Netflix_2024 <- subset(Netflix_Dataset, Year == "2024")

# Plot box plot of opening prices by month for the year 2024
ggplot(Netflix_2024, aes(x = Open, y = Month)) +
  geom_boxplot(fill = "red") +
  labs(x = "Opening Price (USD)", 
       y = "Month", 
       title = "Box Plot of Opening Prices by Month for 2024") +
  scale_y_discrete(limits = month.name) +  # Set the order of months
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"))

write.csv(Netflix_Dataset, "Cleaned_Netflix_Dataset.csv", row.names = FALSE) # exporting cleaned dataset



