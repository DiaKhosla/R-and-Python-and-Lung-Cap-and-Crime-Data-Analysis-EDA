# Milestone 1 - Exploratory Data Analysis 
# ALY 6010
# June 30, 2024
# Group Assignment ( By Dia Khosla, Unnati and Bhoomika)


library(ggplot2)
library(tidyverse)
library(tibble)
library(ggpubr)
library(rstatix)
library(lubridate)
library(stringr)

install.packages("readr")
library(readr)
Crime_Data <- read_csv("Crime_Data_from_2020_to_Present.csv")
View(Crime_Data)
summary(Crime_Data) # finding summary stats

nrow(Crime_Data)
ncol(Crime_Data)
  
library(janitor) # data exploration and cleaning
Crime_Data <- Crime_Data %>% clean_names()
  head(Crime_Data)
  names(Crime_Data)
  
colSums(is.na(Crime_Data))
Crime_Data <- Crime_Data[, !(names(Crime_Data) %in% c("crm_cd_1", "crm_cd_2", "crm_cd_3", "crm_cd_4", "cross_street", "area"))]
str(Crime_Data)


library(dplyr) # piping (%>%)
  Crime_Data <- Crime_Data %>%
    mutate(
      date_rptd = as.Date(date_rptd, format = "%m/%d/%Y"), 
      date_occ = as.Date(date_occ, format = "%m/%d/%Y")   
    )
  

# Creating table - Top 50 crime by activity / counts in descending order out of 138 crimes
  
table_crm_cd_desc <- sort(table(Crime_Data$crm_cd_desc), decreasing = TRUE)
  print(table_crm_cd_desc)
  
crm_cd_desc_unique <- unique(table_crm_cd_desc)
crm_cd_desc <- unique(Crime_Data$crm_cd_desc) # Getting unique category
  

#  Histogram
  
  Crime_Data$date_occ <- as.Date(Crime_Data$date_occ, format = "%Y-%m-%d")
  
  library(ggplot2)
  
  ggplot(Crime_Data, aes(x = date_occ)) +
    geom_histogram(binwidth = 1, fill = "#0072B2", color = "#00558A", alpha = 0.8) +  # Adjusting bar colors and transparency
    labs(
      title = "Histogram of Crime Occurrence Dates",
      x = "Date of Occurrence",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  
      axis.title = element_text(size = 16),  
      axis.text = element_text(size = 14),   
      panel.grid.major = element_blank(),    
      panel.background = element_rect(fill = "white"),  
      axis.line = element_line(color = "black"),  
      legend.position = "none",              
      plot.background = element_rect(fill = "lightgray") 
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")  
 
  # Crimes in Area Frequency Heatmap
  
  library(dplyr)
  library(ggplot2)
  
  freq <- Crime_Data %>%
    count(area_name) %>%
    arrange(desc(n))
  

  ggplot(freq, aes(x = reorder(area_name, n), y = n)) +
    geom_tile(aes(fill = n), color = "white") +
    scale_fill_gradient(low = "pink", high = "darkblue", name = "Number Distribution") +  
    labs(
      title = "Crimes in Area Frequency Heatmap",
      x = "Area Name",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

  
# Distribution of Crimes by Area
  library(ggplot2)
  
  ggplot(Crime_Data, aes(x = area_name, fill = stat(count))) +
    geom_bar(color = "black") +
    scale_fill_gradient(low = "#EFF3FF", high = "#08519C") +
    labs(title = "Distribution of Crimes by Area",
         x = "Area Name in the city of Los Angeles till Present",
         y = "Crime Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# Creating Barplot for 20 Crimes by highest number of activity
  
# Setting up the plotting area
  
  counts = table(Crime_Data$crm_cd_desc)
  counts = counts[order(counts, decreasing=T)]
  print(counts[1:20])
  
  par(mar = c(2, 20, 1, 1))
  
  y <- barplot(counts[1:20], horiz = TRUE, las = 1, cex.names = 0.7, col = "lightblue",
               main = "Top 20 Crimes by Highest Number of Activities", xlab = "Number of Crimes",
               cex.main = 1.2, font.main = 1, width = 1.5)
  
  text(counts[1:20], y, labels = counts[1:20], pos = 4, cex = 0.7)
  

# Creating pie chart  
library(ggplot2) 
counts <- Crime_Data %>%
    count(crm_cd_desc) %>%
    arrange(desc(n)) %>%
    slice(1:10)  
  

ggplot(counts, aes(x = "", y = n, fill = crm_cd_desc)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(x = NULL, y = NULL, fill = "Crime Type", 
       title = "Top 10 Crimes by Highest Number of Activities") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8)  
  )
  

# Visualize using ggplot2 - crime description and victim sex, then count occurrences
library(tidyverse)
library(lubridate)
library(dplyr)
Crime_Data$date_occ <- as.Date(Crime_Data$date_occ)
Crime_Data$date_rptd <- as.Date(Crime_Data$date_rptd)

# 2023 and 2024
filtered_data <- Crime_Data %>%
  filter(year(date_occ) %in% c(2023, 2024))

# Grouping  
crime_counts <- filtered_data %>%
  group_by(crm_cd_desc, vict_sex) %>%
  summarise(count = n(), .groups = 'drop') %>%  # Use .groups = 'drop' to drop grouping
  arrange(desc(count)) %>%
  top_n(10, wt = count)  # Select top 10 crimes by count
  
unique(Crime_Data$vict_sex)


# Top crimes in 2023-2024 by gender

library(ggplot2)
ggplot(crime_counts, aes(x = reorder(crm_cd_desc, count), y = count, fill = vict_sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Crime Description", y = "Count", title = "Top Crimes by Gender") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  
    panel.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),  
    legend.position = "top"  
  ) +
  scale_fill_manual(values = c("blue", "pink", "gray", "orange", "purple"),
                    labels = c("Male", "Female", "Other", "Unknown", "Missing"),
                    na.value = "gray",  
                    name = "Victim's Sex")

q()
# filtered data for men and women only
filtered_data <- crime_counts[crime_counts$vict_sex %in% c("M", "F"), ]

ggplot(filtered_data, aes(x = reorder(crm_cd_desc, count), y = count, fill = vict_sex)) +
  geom_bar(stat = "identity") +  # Stacked bar chart
  labs(x = "Crime Description", y = "Count", title = "Top Crimes by Gender (Male vs Female)") +
  scale_fill_manual(values = c("blue", "pink"),
                    labels = c("Male", "Female"),
                    name = "Victim's Sex") +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  
    panel.background = element_rect(fill = "white"),  
    panel.grid.major = element_blank(),  
    legend.position = "top"  
  )

# Filter data for plotting, excluding rows with NA 
plot_data <- Crime_Data %>%
  filter(!is.na(vict_age), !is.na(status_desc), year(date_occ) %in% 2023:2024)

# Creating a plot of crime counts by victim age and status description
ggplot(plot_data, aes(x = vict_age, y = ..count.., color = status_desc)) +
  geom_density() +
  labs(x = "Victim's Age", y = "Density", color = "Status Description",
       title = "Crime Density by Victim's Age and Status Description 2023-2024") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()


# Filter crime data for the years 2023-2024 and remove NA values in status_desc
filtered_data <- Crime_Data %>%
  filter(!is.na(status_desc), year(date_occ) %in% 2023:2024)

status_summary <- filtered_data %>%
  group_by(status_desc) %>%
  summarise(total_crimes = n()) %>%
  mutate(percent = total_crimes / sum(total_crimes) * 100)

ggplot(status_summary, aes(x = "", y = total_crimes, fill = status_desc)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percent), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = ) +
  labs(x = NULL, y = NULL, fill = "Status Description", title = "Crime Distribution by Status Description") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Crime Data in Los Angeles using a Leaflet and Shiny packages ( Drawing a 3D map )
  
  library(shiny)
  library(leaflet)
  
# Defining the boundaries for Los Angeles

  la_bounds <- list(
    west = -118.53,
    south = 33.70,
    east = -118.13,
    north = 34.34
  )
  
  # Creating 3D bar representation
  createCustomMarker <- function(height) {
    div(
      style = sprintf("width: 20px; height: %dpx; background-color: #e41a1c; border-radius: 5px;", height),
      ""
    )
  }
  
  # Defining UI
  ui <- fluidPage(
    
    titlePanel("Crime Data Map - Los Angeles"),
    

    h3("Crime Data in Los Angeles"),
    
    sidebarLayout(
      sidebarPanel(
        
        # Year range from 2020 to current year)\
        sliderInput("year", "Year:",
                    min = 2020, max = as.numeric(format(Sys.Date(), "%Y")), value = 2020, step = 1),
        
        # Crime description 
        selectInput("crime", "Crime Description:",
                    choices = sort(unique(Crime_Data$crm_cd_desc)))
      ),
      
    
      mainPanel(
        leafletOutput("map")
      )
    )
  )
  
  # Defining server logic
  server <- function(input, output) {
    
    # Filtering data based on user inputs and Los Angeles boundaries
    filteredData <- reactive({
      Crime_Data_subset <- Crime_Data[Crime_Data$crm_cd_desc == input$crime & 
                                        as.numeric(format(as.Date(Crime_Data$date_occ, "%Y-%m-%d"), "%Y")) == input$year &
                                        Crime_Data$lon >= la_bounds$west & Crime_Data$lon <= la_bounds$east &
                                        Crime_Data$lat >= la_bounds$south & Crime_Data$lat <= la_bounds$north, ]
      Crime_Data_subset
    })
    
    # Mapping
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = mean(c(la_bounds$west, la_bounds$east)), lat = mean(c(la_bounds$south, la_bounds$north)), zoom = 10) %>%
        addMarkers(data = filteredData(),
                   lng = ~lon, lat = ~lat,
                   icon = list(
                     iconUrl = NULL,
                     iconSize = c(20, 20),
                     iconAnchor = c(10, 10),
                     html = lapply(1, function(x) createCustomMarker(20))
                   ),
                   popup = ~paste("Crime Description:", crm_cd_desc, "<br>",
                                  "Date Occurred:", date_occ))
    })
  }
  
shinyApp(ui = ui, server = server) # used to run the server



