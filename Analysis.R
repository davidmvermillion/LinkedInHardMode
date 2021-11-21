# Code to analyze each post's performances

library('tidyverse')

# Load data into Tibble
data <- read.csv("~/GitHub/LinkedInHardMode/LinkedInPostPerformancesDay25.csv")
# Correct to most recent file before running script

# Exploring weird read-in


# Exploring weird read-in -------------------------------------------------
# CSV mutated Day header
summary(data)
head(data)

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
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra) # https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/
Violin_VICCO <- grid.arrange(vp_vco, vp_ico, vp_cco, nrow = 1)
Violin_VIC <- grid.arrange(vp_v, vp_i, vp_c, nrow = 1)
Violin <- grid.arrange(vp_vco, vp_ico, vp_cco, vp_v, vp_i, vp_c, nrow = 2)
# Once ready, create proper presentation template

# Bar Graph Comparison of Views, Interactions, and Comments ---------------

# Tidy the data
# https://stackoverflow.com/questions/70051541/how-do-i-adjust-my-tibble-to-get-a-grouped-bar-chart-in-ggplot2
data1 <- data %>% 
  pivot_longer(
    cols = c(Views, Interactions, Comments),
    names_to = "Section",
    values_to = "values"
  )

# Full Comparison
vic1 <-  ggplot(data1, aes(x = Day, y = values, Fill = Section)) +
  geom_bar(position = "dodge", stat = "identity")
vic1

# Challenge Only
co1 <- challengeonly %>% 
  pivot_longer(
    cols = c(Views, Interactions, Comments),
    names_to = "Section",
    values_to = "values"
  )

vic_co1 <- ggplot(co1, aes(x = Day, y = values, Fill = Section)) +
  geom_bar(position = "dodge", stat = "identity")
vic_co1

# Challenge only category
co_2 <- arrange(co1, values, Category)

cat_co <- ggplot(co_2, aes(x = Category, y = values, Fill = Section)) +
  geom_bar(position = "dodge", stat = "identity")
cat_co

# Combined
vicc <- grid.arrange(vic1, vic_co1, cat_co, nrow = 3)
vicc


# Columns -----------------------------------------------------------------


# Views
# Tidied this section, but not the next ones
bp_v <- ggplot(data1, aes(x = Day, y = values)) + geom_col()
bp_v
bp_vco <- ggplot(co1, aes(x = Day, y = values)) + geom_col()
bp_vco

# Interactions
bp_i <- ggplot(data, aes(x = Day, y = Interactions)) + geom_col()
bp_i
bp_ico <- ggplot(challengeonly, aes(x = Day, y = Interactions)) + geom_col()
bp_ico

# Comments
bp_c <- ggplot(data, aes(x = Day, y = Comments)) + geom_col()
bp_c
bp_cco <- ggplot(challengeonly, aes(x = Day, y = Comments)) + geom_col()
bp_cco

# Put them together
column_VICCO <- grid.arrange(bp_vco, bp_ico, bp_cco, nrow = 3)
column_VIC <- grid.arrange(bp_v, bp_i, bp_c, nrow = 1)
column <- grid.arrange(bp_vco, bp_ico, bp_cco, bp_v, bp_i, bp_c, nrow = 2)


# Compare Categories and Post Types ---------------------------------------

# Currently includes three days of posts before challenge

# Categories
cat2 <- CategoryCount
cat2$Category <- factor(cat2$Category, levels = cat2$Category[order(cat2$n, decreasing = FALSE)])
cat <- ggplot(cat2, aes(n, Category)) + geom_bar(stat = "identity")
cat

# Types
type2 <- TypeCount
type2$Type <- factor(type2$Type, levels = type2$Type[order(type2$n, decreasing = FALSE)])
type <- ggplot(type2, aes(n, Type)) + geom_bar(stat = "identity")
type

# Charts together
ct <- grid.arrange(cat, type, nrow = 2)
