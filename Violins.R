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
        axis.text.x = element_blank())
vp_v



# Not worked on yet
vp_vco <- ggplot(challengeonly, aes(Day, Views)) + geom_violin()
vp_vco <- vp_vco + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))