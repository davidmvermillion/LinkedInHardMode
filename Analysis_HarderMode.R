# Code to analyze #LinkedInHarderMode results

library(tidyverse)

# Import
posts <- read.csv("LinkedInPostPerformancesDay98.csv", fileEncoding = "UTF-8-BOM")
profileviews <- read.csv("ProfileViews.csv", fileEncoding = "UTF-8-BOM")
connections <- read.csv("ConnectionCountbyDate.csv", fileEncoding = "UTF-8-BOM")

# Format
posts$Date <- as.Date(posts$Date)
profileviews$Weekof <- as.Date(profileviews$Weekof)
connections$Date <- as.Date(connections$Date)

plot(posts$Views)
