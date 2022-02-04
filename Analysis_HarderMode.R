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

posts$Engagement <- (posts$Reactions + posts$Comments)/posts$Views

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
#   Engagement

# Find
#   Which days performed best
#   Number of categories
#   Best three categories

# Show
  # Performance over thirty days
  # Profile view history

# Sort Data ---------------------------------------------------------------

duds <- posts %>% filter(Views < as.integer(quantile(posts$Views, .33))) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

topperformers <- posts %>% filter(Views > as.integer(quantile(posts$Views, .66))) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

greater1000 <- posts %>% filter(Views > 1000) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

top3 <- posts %>% filter(Views > 3500) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

video <- posts %>%  
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type == "Video") %>% 
  select(-Votes)

# Weekday comparisons ----

three <- posts %>% filter(WeekdayNumber == 3) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

four <- posts %>% filter(WeekdayNumber == 4) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

five <- posts %>% filter(WeekdayNumber == 5) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

three <- posts %>% filter(WeekdayNumber == 5) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

Sunday <- posts %>% filter(WeekdayNumber == 1) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  filter(Type != "Poll") %>% 
  select(-Votes)

Monday <- posts %>% filter(WeekdayNumber == 2) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

Tuesday <- posts %>% filter(WeekdayNumber == 3) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  filter(Type != "Poll") %>% 
  select(-Votes)

Wednesday <- posts %>% filter(WeekdayNumber == 4) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

Thursday <- posts %>% filter(WeekdayNumber == 5) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

Friday <- posts %>% filter(WeekdayNumber == 6) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

Saturday <- posts %>% filter(WeekdayNumber == 7) %>% 
  filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Video") %>% 
  select(-Votes)


# Categories --------------------------------------------------------------

catcount <- unique(posts$Category)
typecount <- unique(posts$Type)


# Visuals -----------------------------------------------------------------

posts %>% filter(Day > 0) %>% 
  ggplot(aes(x = Date, y = Reactions)) +
  geom_point(color = "grey45") +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Reactions per Post Show\nSteady Increase")


plot(posts$Views)
