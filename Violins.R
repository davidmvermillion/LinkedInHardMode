# Figure out optimal chart setup for violin charts
# Requires environment load from Analysis.R

vp_v <- data %>% 
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal() +
  ggtitle("View Counts") +
  #ylab("View \n Counts") +
  # geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  # Quantile doesn't work to overlay in this manner
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 30),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
        )
vp_v


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
        axis.text.x = element_blank())
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
        axis.text.x = element_blank())
vp_vt