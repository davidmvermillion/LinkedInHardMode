# Code to analyze #LinkedInHarderMode results

library(tidyverse)

# Import ----
posts <- read.csv("LinkedInPostPerformancesDay108.csv", fileEncoding = "UTF-8-BOM")
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
  select(-Votes) %>% 
  slice_head(n = 10)

greater1000 <- posts %>% filter(Views > 1000) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes)

top3 <- posts %>% filter(Views > 3500) %>% 
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type != "Video") %>% 
  select(-Votes) %>% 
  slice(1:3) # https://www.geeksforgeeks.org/select-top-n-highest-values-by-group-in-r/

video <- posts %>%  
  filter(Date >= "2022-01-03") %>% 
  filter(Date <= "2022-02-01") %>% 
  filter(Type == "Video") %>% 
  select(-Votes)

# Weekday comparisons ----

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

CDaysofTribute <- posts %>% 
  filter(Category == "12DaysofTribute")

CArtificial_Intelligence <- posts %>% 
  filter(Category == "Artificial_Intelligence")

CData <- posts %>% 
  filter(Category == "Data")

CHoliday <- posts %>% 
  filter(Category == "Holiday")

CLife <- posts %>% 
  filter(Category == "Life")

CNostalgia <- posts %>% 
  filter(Category == "Nostalgia")

COffice <- posts %>% 
  filter(Category == "Office")

CPhilosophy <- posts %>% 
  filter(Category == "Philosophy")

CPublishing <- posts %>% 
  filter(Category == "Publishing")

CSpace <- posts %>% 
  filter(Category == "Space")

CSpaceForce <- posts %>% 
  filter(Category == "SpaceForce")

CSpaceNews <- posts %>% 
  filter(Category == "SpaceNews")

CUpdate <- posts %>% 
  filter(Category == "Update")

CUSSFExplained <- posts %>% 
  filter(Category == "USSFExplained")


# Type --------------------------------------------------------------

TDocument <- posts %>%
  filter(Type == "Document")

TLink <- posts %>%
  filter(Type == "Link")

TPhoto_1 <- posts %>%
  filter(Type == "Photo_1")

TPhoto_2 <- posts %>%
  filter(Type == "Photo_2")

TPhoto_3 <- posts %>%
  filter(Type == "Photo_3")

TText <- posts %>%
  filter(Type == "Text")

# Still need to compare types and categories


# Visuals -----------------------------------------------------------------

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Reactions, color = Category)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Reactions per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Views, color = Category)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Views per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Comments, color = Category)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Comments per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Reactions, color = Type)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Reactions per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Views, color = Type)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Views per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Comments, color = Type)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E34234", size = 2) +
  theme_generic() +
  ggtitle("Comments per Post Show\nSteady Increase")




plot(posts$Views)
