# Code to analyze #LinkedInHarderMode results

library(tidyverse)

# Import ----
posts <- read.csv("LinkedInPostPerformancesDay98.csv", fileEncoding = "UTF-8-BOM")
profileviews <- read.csv("ProfileViews.csv", fileEncoding = "UTF-8-BOM")
connections <- read.csv("ConnectionCountbyDate.csv", fileEncoding = "UTF-8-BOM")

# Format ----
posts$Date <- as.Date(posts$Date)
profileviews$Weekof <- as.Date(profileviews$Weekof)
connections$Date <- as.Date(connections$Date)

# ggplot Theme ----
theme_generic <- function(base_size = 12,
                          base_family = "",
                          base_line_size = base_size / 170,
                          base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size, 
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      axis.title.y = element_text(angle = 0, 
                                  vjust = 0, 
                                  hjust = 0.5, 
                                  size = 15, 
                                  color = "grey55"),
      axis.title.x = element_text(hjust = 0, 
                                  size = 15, 
                                  color = "grey55"),
      axis.text = element_text(size = 12, 
                               color = "grey60"),
      axis.line = element_line(color = "grey60"),
      axis.ticks = element_line(color = "grey60"),
      plot.title = element_text(hjust = 0.5, 
                                size = 40, 
                                color = "grey40"), 
      panel.grid = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, 
                                   size = 20, 
                                   color = "grey55"),
      
      complete = TRUE
    )
}

# Plan --------------------------------------------------------------------

# Trend
#   Duds
#   Top-Performers
#   Videos

# Find
#   Which days performed best
#   Number of categories
#   Best three categories

# Show
  # Performance over thirty days
    # Profile view history



plot(posts$Views)
