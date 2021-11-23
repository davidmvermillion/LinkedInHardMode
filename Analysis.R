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
vp_vy <- data %>% #y
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ylab("View \nDistribution") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0, size = 20, color = "grey55"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_vy

vp_vt <- data %>% #title
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("View Distribution") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_vt

vp_vcoy <- challengeonly %>% #y
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ylab("View \nDistribution") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0, size = 20, color = "grey55"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_vcoy

vp_vcot <- challengeonly %>% #title
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("View Distribution") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_vcot

vp_vcott <- challengeonly %>% #title
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Views") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_vcott


# Interactions
vp_iy <- data %>% #y
  ggplot(aes(Day, Interactions)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ylab("Interaction \nDistribution") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0, size = 20, color = "grey55"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_iy

vp_it <- data %>% #title
  ggplot(aes(Day, Interactions)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Interaction Distribution") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_it

vp_icoy <- challengeonly %>% #y
  ggplot(aes(Day, Interactions)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ylab("Interaction \nDistribution") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0, size = 20, color = "grey55"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_icoy

vp_icot <- challengeonly %>% #title
  ggplot(aes(Day, Interactions)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Interaction Distribution") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_icot

vp_icott <- challengeonly %>% #title
  ggplot(aes(Day, Interactions)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Interactions") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_icott

# Comments
vp_cy <- data %>% #y
  ggplot(aes(Day, Comments)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ylab("Comment \nDistribution") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0, size = 20, color = "grey55"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_cy

vp_ct <- data %>% #title
  ggplot(aes(Day, Comments)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Comment Distribution") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_ct

vp_ccoy <- challengeonly %>% #y
  ggplot(aes(Day, Comments)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ylab("Comment \nDistribution") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0, size = 20, color = "grey55"),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_ccoy

vp_ccot <- challengeonly %>% #title
  ggplot(aes(Day, Comments)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Comment Distribution") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_ccot

vp_ccott <- challengeonly %>% #title
  ggplot(aes(Day, Comments)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("Comments") +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
vp_ccott

# Put them together
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra) # https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/
if(!require(grid)) install.packages("grid")
library(grid)

Violin_VICCOY <- grid.arrange(vp_vcoy, vp_icoy, vp_ccoy, nrow = 3)
Violin_VICCOT <- grid.arrange(vp_vcott, vp_icott, vp_ccott, nrow = 1, 
                              bottom = textGrob("Challenge Distributions",
                                      gp = gpar(fontsize = 20, col = "grey35")))
Violin_VIC <- grid.arrange(vp_vt, vp_it, vp_ct, nrow = 1)
Violin <- grid.arrange(vp_vcot, vp_icot, vp_ccot, vp_vt, vp_it, vp_ct, nrow = 2,
                       bottom = textGrob("Challenge Distributions \nOutlier Included on Bottom",
                                      gp = gpar(fontsize = 20, col = "grey35")))
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

# Challenge Only Wave Pattern
co1 <- challengeonly %>% 
  pivot_longer(
    cols = c(Views, Interactions, Comments),
    names_to = "Section",
    values_to = "values"
  )

vic_co1 <- ggplot(co1, aes(x = Day, y = values, Fill = Section)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  ggtitle("Views Dwarfed Likes and Comments", subtitle = "Views Showed a Wavelike Cyclical Pattern") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    plot.subtitle = element_text(hjust = 0.5, color = "grey55", size = 15),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
vic_co1

# Challenge only category
co_2 <- arrange(co1, values, Category) # Doesn't show on chart

# https://stackoverflow.com/questions/70051541/how-do-i-adjust-my-tibble-to-get-a-grouped-bar-chart-in-ggplot2/70051880?noredirect=1#comment123848638_70051880
level_order <- co_2 %>% 
  filter(Section == "Views") %>% 
  arrange(desc(values)) %>% 
  pull(Category) %>% 
  unique()

cat_co <- ggplot(co_2, aes(y = factor(Category, level = level_order), x = values, Fill = Section)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  ggtitle("Post Categories Ranked by Impact") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank()
    )
cat_co




# Combined
vicc <- grid.arrange(vic1, vic_co1, cat_co, nrow = 3)
vicc


# Columns -----------------------------------------------------------------


# Views
# Tidied this section, but not the next ones
bp_v <- ggplot(data1, aes(x = Day, y = values)) + geom_col()
bp_v
bp_vco <- ggplot(co1, aes(x = Day, y = values)) +
  geom_col() +
  theme_minimal() +
  ggtitle("View Counts") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
  )
bp_vco

# Interactions
bp_i <- ggplot(data, aes(x = Day, y = Interactions)) + geom_col()
bp_i
bp_ico <- ggplot(challengeonly, aes(x = Day, y = Interactions)) +
  geom_col() +
  theme_minimal() +
  ggtitle("Interaction Counts") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
  )
bp_ico

# Comments
bp_c <- ggplot(data, aes(x = Day, y = Comments)) + geom_col()
bp_c
bp_cco <- ggplot(challengeonly, aes(x = Day, y = Comments)) +
  geom_col() +
  theme_minimal() +
  ggtitle("Comment Counts") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
  )
bp_cco

# Put them together
column_VICCO <- grid.arrange(bp_vco, bp_ico, bp_cco, nrow = 1,
                             bottom = textGrob("Challenge Views, Interactions, and Comments had Cyclical Patterns",
                                               gp = gpar(fontsize = 20, col = "grey35")))
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
