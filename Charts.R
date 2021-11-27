# Pulling specific code needed from Analysis

library('tidyverse')

# Load data into Tibble
data <- read.csv("~/GitHub/LinkedInHardMode/LinkedInPostPerformancesDay30.csv")

# Corrected Day column ----
data <- rename(data, Day = ï..Day)

# Create useful selections ----
challengeonly <- filter(data, Day > 0)
CategoryCount2 <- count(challengeonly, Category)
TypeCount2 <- count(challengeonly, Type)
VIC <- data %>% select(Views, Interactions, Comments)

# Violin plot Views, Interactions, and Comments ----
# Views
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

Violin_VICCOT <- grid.arrange(vp_vcott, vp_icott, vp_ccott, nrow = 1, 
                              top = textGrob("Challenge Distributions",
                                             gp = gpar(fontsize = 30, col = "grey45")))

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
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  ggtitle("Outlier Post was 3x Bigger than Closest Challenger") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
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

# Compare Categories and Post Types ---------------------------------------

# Currently includes three days of posts before challenge

# Categories
# Challenge only
cat3 <- CategoryCount2
cat3$Category <- factor(cat3$Category, levels = cat3$Category[order(cat3$n, decreasing = FALSE)])
cat4 <- ggplot(cat3, aes(n, Category)) + geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("I Posted to 9 Subcategories") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
  )
cat4

# Types
# Challenge only
type3 <- TypeCount2
type3$Type <- factor(type3$Type, levels = type3$Type[order(type3$n, decreasing = FALSE)])
type4 <- ggplot(type3, aes(n, Type)) + geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("I Posted More Text than Photos") +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "grey55", size = 30),
    # plot.subtitle = element_text(hjust = 0.5, color = "grey55", size = 15),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
  )
type4
