# Code to analyze each post's performances

library('tidyverse')

# Load data into Tibble
data <- read.csv("~/GitHub/LinkedInHardMode/LinkedInHardMode/LinkedInPostPerformancesDay25.csv")
# Correct to most recent file before running script

# Exploring weird read-in


# Exploring weird read-in -------------------------------------------------
summary(data)
head(data)
a <- data %>% select(contains("Day"))

# Corrected Day column ----
data <- rename(data, Day = ï..Day)
head(data)
plot(data)

# Create useful selections ----
CategoryCount <- count(data, Category)
TypeCount <- count(data, Type)
InteractionsCount <- count(data, Interactions)
CommentsCount <- count(data, Comments)
VIC <- data %>% select(Views, Interactions, Comments)

# Violin plot Views, Interactions, and Comments ----
vp_v <- ggplot(data, aes(Day, Views)) + geom_violin()
vp_v


# Bar Graph Comparison of Views, Interactions, and Comments ---------------



# Compare Categories and Post Types ---------------------------------------


