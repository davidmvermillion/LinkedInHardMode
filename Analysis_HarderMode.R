# Code to analyze #LinkedInHarderMode results

library(tidyverse)
library(scales)
library(latex2exp)

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
      axis.title.y = element_text(angle = 0, vjust = 0, hjust = 0.5, size = 15,
                                  color = "grey55", margin =
                                    margin(t = 0, r = 7, b = 0, l = 0,
                                           unit = "pt")),
      axis.title.x = element_text(hjust = 0, size = 15, color = "grey55"),
      axis.text = element_text(size = 12, color = "grey60"),
      axis.line = element_line(color = "grey60"),
      axis.ticks = element_line(color = "grey60"),
      plot.title = element_text(hjust = 0.5, size = 40, color = "grey40",
                                margin =
                                  margin(t = 0, r = 0, b = 10, l = 0,
                                         unit = "pt")),
      panel.grid = element_blank(),
      plot.subtitle = element_text(hjust = 0.5, size = 20, color = "grey40",
                                   margin =
                                     margin(t = 10, r = 0, b = 0, l = 0,
                                            unit = "pt")),
      legend.title = element_text(size = 15, color = "grey40"),
      legend.text = element_text(size = 12, color = "grey30"),
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

Weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
              "Saturday")
weekcalc <- c(median(Sunday$Views), median(Monday$Views), median(Tuesday$Views),
               median(Wednesday$Views), median(Thursday$Views), median(Friday$Views),
               median(Saturday$Views))

Weeks <- as_tibble(cbind(Weekdays, weekcalc))
Weeks$weekcalc <- as.numeric(Weeks$weekcalc)

# Categories --------------------------------------------------------------

CDaysofTribute <- posts %>% 
  filter(Category == "12DaysofTribute")

CArtificial_Intelligence <- posts %>% 
  filter(Category == "Artificial_Intelligence")

CCelebration <- posts %>% 
  filter(Category == "Celebration")

CCoding <- posts %>% 
  filter(Category == "Coding")

CCyberSecurity <- posts %>% 
  filter(Category == "CyberSecurity")

CData <- posts %>% 
  filter(Category == "Data")

CHoliday <- posts %>% 
  filter(Category == "Holiday")

CLife <- posts %>% 
  filter(Category == "Life")

CMath <- posts %>% 
  filter(Category == "Math")

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

catlist <- (unique(posts$Category))

# More work needed to streamline this process. for loop better, but not sure how to implement
# This is for the entire data set
CategoriesCompared <- (c(median(CSpaceForce$Views), median(CCyberSecurity$Views),
                                  median(CCelebration$Views), median(CUpdate$Views), median(CSpaceNews$Views),
                        median(CData$Views), median(CUSSFExplained$Views), median(CNostalgia$Views), median(CPublishing$Views),
                        median(CHoliday$Views), median(COffice$Views), median(CSpace$Views),
                        median(CArtificial_Intelligence$Views), median(CDaysofTribute$Views), median(CPhilosophy$Views),
                        median(CLife$Views), median(CCoding$Views), median(CMath$Views)))

cc <- as_tibble(cbind(catlist, CategoriesCompared))
cc$CategoriesCompared <- as.numeric(cc$CategoriesCompared)
cc$catlist <- as.character(cc$catlist)

top3c <- cc %>% 
  arrange(desc(CategoriesCompared)) %>% 
  slice_head(n = 3)

top5c <- cc %>% 
  arrange(desc(CategoriesCompared)) %>% 
  slice_head(n = 5)

dud3c <- cc %>% 
  arrange(desc(CategoriesCompared)) %>% 
  slice_tail(n = 3)


# Type --------------------------------------------------------------

typelist <- (unique(posts$Type))

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

TPhoto_5 <- posts %>%
  filter(Type == "Photo_5")

TText <- posts %>%
  filter(Type == "Text")

TShare <- posts %>%
  filter(Type == "Share")

TPoll <- posts %>%
  filter(Type == "Poll")

TVideo <- posts %>%
  filter(Type == "Video")

TypeCompared <- c(
  median(TShare$Views), median(TPhoto_1$Views), median(TPhoto_5$Views), 
  median(TText$Views),  median(TPhoto_3$Views), median(TDocument$Views), 
  median(TLink$Views), median(TPoll$Views), median(TPhoto_2$Views), 
  median(TVideo$Views)
)

tt <- as_tibble(cbind(typelist, TypeCompared))
tt$TypeCompared <- as.numeric(tt$TypeCompared)
tt$typelist <- as.character(tt$typelist)

# Total top 3
top3t <- tt %>% 
  arrange(desc(TypeCompared)) %>% 
  slice_head(n = 3)

# Top 3 ignoring two wild ones
top3tnorm <- tt %>% 
  arrange(desc(TypeCompared)) %>% 
  filter(typelist != c("Photo_5", "Poll")) %>% 
  slice_head(n = 3)

dud3t <- tt %>% 
  arrange(desc(TypeCompared)) %>% 
  filter(typelist != "Video") %>% 
  slice_tail(n = 3)

# Still need to compare types and categories


# Visuals -----------------------------------------------------------------

# Comparisons in categories

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Reactions, color = Category)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Reactions per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Views, color = Category)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Views per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Comments, color = Category)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Comments per Post Show\nSteady Increase")

# Comparisons in Types

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Reactions, color = Type)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Reactions per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Views, color = Type)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Views per Post Show\nSteady Increase")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Comments, color = Type)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Comments per Post Show\nSteady Increase")


# Engagement
# Videos (top four in upper right quadrant) show high engagement
posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  # filter(Type != "Poll") %>% 
  ggplot(aes(x = Date, y = Engagement)) +
  geom_point(color = "#f4b3ae") +
  geom_point(data = video,
            aes(x = Date, y = Engagement),
            color = "#E34234", size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  theme_generic() +
  ggtitle("Video Engagement is Incredible",
          subtitle = "Compared to Other Content")

# Polls have horrible engagement relative to views
posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  ggplot(aes(x = Date, y = Engagement)) +
  geom_point(color = "#f4b3ae") +
  geom_point(data = TPoll,
             aes(x = Date, y = Engagement),
             color = "#E34234", size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  theme_generic() +
  ggtitle("Poll Engagement is Terrible",
          subtitle = "Compared to Other Content")

# Views, Reactions, and Comments

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Comments)) +
  geom_point(color = "#f4b3ae") +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Comments per Post Show\nSteady Increase",
          subtitle = "Ignoring Polls and Videos")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Views)) +
  geom_point(color = "#f4b3ae") +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Views per Post Show\nSteady Increase",
          subtitle = "Ignoring Polls and Videos")

posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>% 
  filter(Type != "Poll") %>% 
  filter(Type != "Video") %>% 
  ggplot(aes(x = Date, y = Reactions)) +
  geom_point(color = "#f4b3ae") +
  geom_smooth(method='lm', se=FALSE, color = "#E3347D", size = 2) +
  theme_generic() +
  ggtitle("Reactions per Post Show\nSteady Increase",
          subtitle = "Ignoring Polls and Videos")

# Profile Views During #LinkedInHarderMode Challenge
pchal <- profileviews %>% 
  filter(Weekof < "2022-02-03") %>% 
  filter(Weekof > "2022-01-03")

# Profile Views
profileviews %>% 
  filter(Weekof < "2022-02-03") %>% 
  ggplot(aes(x = Weekof, y = ProfileViews)) +
  geom_line(color = "grey40", size = 1, linetype = 2) +
  geom_point(data = pchal,
             aes(x = Weekof, y = ProfileViews), size = 4, color = "#E34234") +
  theme_generic() +
  ggtitle("Profile Views\nHighest During Challenge",
          subtitle = "Regular Posting Increased Profile Views") +
  labs(y = "Profile\nViews", x = element_blank())


# Histogram Comparisons ----

# Views
posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>%
  filter(Views < 5000) %>% 
  ggplot(
  aes(x = Views)
) +
  geom_histogram(bins = 15, color = "#e34234", fill = "#f4b3ae") +
  theme_generic() +
  labs(title = TeX("Posts Usually Receive ~ 1,000 Views"),
       x = "Views",
       y = "Count/\nBin")

# Comments
posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>%
  filter(Comments < 50) %>% 
  ggplot(
    aes(x = Comments)
  ) +
  geom_histogram(bins = 10, color = "#e34234", fill = "#f4b3ae") +
  theme_generic() +
  labs(title = TeX("Posts Usually Receive $\\approx$ 10 Comments"),
       x = "Comments",
       y = "Count/\nBin")

# Reactions
posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>%
  filter(Reactions < 50) %>% 
  ggplot(
    aes(x = Reactions)
  ) +
  geom_histogram(bins = 10, color = "#e34234", fill = "#f4b3ae") +
  theme_generic() +
  labs(title = TeX("Posts Usually Receive $\\approx$ 10 Reactions"),
       x = "Reactions",
       y = "Count/\nBin")

# Reactions
posts %>% filter(Day > 0) %>% 
  filter(Day <= 100) %>%
  # filter(Reactions < 50) %>% 
  ggplot(
    aes(x = Category, stat = "count")
  ) +
  geom_histogram(bins = 10, color = "#e34234", fill = "#f4b3ae") +
  theme_generic() +
  labs(title = TeX("Posts Usually Receive $\\approx$ 10 Reactions"),
       x = "Reactions",
       y = "Count/\nBin")


# Bar Graphs ----
# Compare median view counts on posts by category, type, and day

# Category
cc %>% 
  ggplot(
  aes(x = reorder(catlist, -CategoriesCompared), y = CategoriesCompared)
) +
  geom_bar(stat = 'identity', fill = "#f4b3ae") +
  coord_flip() +
  theme_generic() +
  ggtitle("Category Views\nNot Evenly Distributed") +
  labs(y = ("Median Views"),
       x = ("Post\nCategories")) +
  theme(plot.margin =
          margin(t = 10, r = 50, b = 10, l = 10,
                 unit = "pt"))

t2 <- tt %>% 
  filter(typelist == "Photo_1")
t3 <- tt %>% 
  filter(typelist == "Document")
typehighlight <- bind_rows(t2, t3)


# Type
tt %>% 
  ggplot(
    aes(x = reorder(typelist, -TypeCompared), y = TypeCompared)
  ) +
  geom_bar(stat = 'identity', fill = "#f4b3ae") +
  geom_bar(data = typehighlight,
             aes(x = reorder(typelist, -TypeCompared), y = TypeCompared),
             stat = "identity", fill = "#E34234") +
  coord_flip() +
  theme_generic() +
  ggtitle("Documents and Single Photos\nPerform Well") +
  labs(y = ("Median Views"),
       x = ("Post\nTypes")) +
  theme(plot.margin =
          margin(t = 10, r = 50, b = 10, l = 10,
                 unit = "pt"))

d2 <- Weeks %>% 
  filter(Weekdays == "Thursday")
d3 <- Weeks %>% 
  filter(Weekdays == "Sunday")
dayhighlight <- bind_rows(d2, d3)

# Days
Weeks %>% 
  ggplot(
    aes(x = reorder(Weekdays, -weekcalc), y = weekcalc)
  ) +
  geom_bar(stat = 'identity', fill = "#f4b3ae") +
  geom_bar(data = dayhighlight,
           aes(x = reorder(Weekdays, -weekcalc), y = weekcalc),
           stat = "identity", fill = "#E34234") +
  coord_flip() +
  theme_generic() +
  ggtitle("Sundays and Thursdays\nPerform Best") +
  labs(y = ("Median Views"),
       x = ("Post\nDays")) +
  theme(plot.margin =
          margin(t = 10, r = 50, b = 10, l = 10,
                 unit = "pt"))

plot(posts$Views)
