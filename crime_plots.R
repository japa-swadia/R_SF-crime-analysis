#CSE 563
#Japa Swadia

#San Francisco crime records analysis
#Plot of each crime by its Category against day of week, police district and year------

#Load necessary libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(readr)
library(lubridate)

# Read the training data
crime_data <- read.csv("C:/Users/japas_000/Documents/CSE 563/Data sets/kaggle/SF crime/train.csv", header = TRUE, nrows = 10000)

# Build a contingency table of all combinations of crime categories and days of the week
crimes_by_day <- table(crime_data$Category,crime_data$DayOfWeek)

# Reshape the table for plotting
crimes_by_day <- melt(crimes_by_day)
names(crimes_by_day) <- c("Category","DayOfWeek","Count")

# Make bar plots
g_crimeplot <- ggplot(crimes_by_day,aes(x=Category, y=Count,fill = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~DayOfWeek) +
  theme(legend.position = "none")

ggsave(g_crimeplot, file="C:/Users/japas_000/Documents/CSE 563/sfCrimes_by_day.png", width=20, height=8)


#----Plot by police district------------------------------------------------------

crimes_by_district <- table(crime_data$Category,crime_data$PdDistrict)

crimes_by_district <- melt(crimes_by_district)
names(crimes_by_district) <- c("Category","PdDistrict","Count")

g <- ggplot(crimes_by_district,aes(x=Category, y=Count,fill = Category)) + 
  geom_bar(stat = "Identity") + 
  coord_flip() +
  facet_grid(.~PdDistrict) +
  theme(legend.position = "none")

ggsave(g, file="C:/Users/japas_000/Documents/CSE 563/Crimes_by_district.png", width=20, height=8)

#-------Plot by year---------------------------------------

# Add a column for Year data
crime_data$Year <- year(ymd_hms(crime_data$Dates))

crimes_by_year <- table(crime_data$Category,crime_data$Year)

crimes_by_year <- melt(crimes_by_year)
names(crimes_by_year) <- c("Category","Year","Count")

g <- ggplot(crimes_by_year,aes(x=Category, y=Count,fill = Category)) + 
  geom_bar(stat = "Identity")

ggsave(g, file="C:/Users/japas_000/Documents/CSE 563/Crimes_by_year.png", width=20, height=8)
