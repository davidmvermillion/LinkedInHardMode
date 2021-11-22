# Figure out optimal chart setup for bar charts
# Requires environment load from Analysis.R

bp_vco <- ggplot(co1, aes(x = Day, y = values)) +
  geom_col() +
  theme_minimal() +
  ggtitle("Interaction Variance per Challenge Day") +
  theme(
    axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "grey55", size = 20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        )
bp_vco

