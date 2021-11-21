# Figure out optimal chart setup for violin charts
# Requires environment load from Analysis.R

vp_v <- data %>% 
  ggplot(aes(Day, Views)) +
  geom_violin(fill = "gray80") +
  theme_minimal()
vp_v
vp_vco <- ggplot(challengeonly, aes(Day, Views)) + geom_violin()
vp_vco <- vp_vco + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))