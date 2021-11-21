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
challengeonly <- filter(data, Day > 0)

# Violin plot Views, Interactions, and Comments ----
# Views
vp_v <- ggplot(data, aes(Day, Views)) + geom_violin()
vp_v
vp_vco <- ggplot(challengeonly, aes(Day, Views)) + geom_violin()
vp_vco <- vp_vco + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Interactions
vp_i <- ggplot(data, aes(Day, Interactions)) + geom_violin()
vp_i
vp_ico <- ggplot(challengeonly, aes(Day, Interactions)) + geom_violin()
vp_ico <- vp_ico + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Comments
vp_c <- ggplot(data, aes(Day, Comments)) + geom_violin()
vp_c
vp_cco <- ggplot(challengeonly, aes(Day, Interactions)) + geom_violin()
vp_cco <- vp_cco + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

# Put them together
library(gridExtra) # https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/
Violin_VICCO <- grid.arrange(vp_vco, vp_ico, vp_cco, nrow = 1)
Violin_VIC <- grid.arrange(vp_v, vp_i, vp_c, nrow = 1)
Violt <- grid.arrange(vp_vco, vp_ico, vp_cco, vp_v, vp_i, vp_c, nrow = 2)

# Bar Graph Comparison of Views, Interactions, and Comments ---------------



# Compare Categories and Post Types ---------------------------------------


