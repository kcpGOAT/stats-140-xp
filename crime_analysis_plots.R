## PLOT 0

barplot(sort(period_count$n),
        names.arg = period_count$period_day[c(3, 1, 2)],
        density = 45, col = "blue",
        xlab = "Period of Day", ylab = "Occurrences", yaxt = "n")
axis(2, at = c(0, 200000, 400000), labels = c(0, "200K", "400K"))

## PLOT 1

ggplot(crime_season_results, aes(reorder_within(type_crime, n, season), n)) +
  geom_col(alpha = 0.5, aes(fill = season)) +
  scale_x_reordered() +
  facet_wrap(~ factor(season, levels = c("Winter", "Spring", "Summer", "Fall")), 
             scales = "free_x") +
  labs(x = "") +
  scale_fill_manual(values = c("red", "lightgreen", "orange", "lightblue")) +
  guides(fill = "none") +
  theme_minimal()

## PLOT 2

ggplot(crime_period_results, aes(reorder_within(type_crime, n, period_day), n)) +
  geom_col(alpha = 0.5, aes(fill = period_day)) +
  scale_x_reordered() +
  facet_wrap(~ factor(period_day, levels = c("Morning", "Afternoon", "Evening")), 
             scales = "free", nrow = 3) +
  labs(x = "") +
  scale_fill_manual(values = c("gray", "yellow", "orange")) +
  guides(fill = "none") +
  theme_minimal()

## PLOT 3

crime_status %>%
  slice_head(n = 5) %>%
  union_all(crime_status[22:26, ]) %>%
  mutate(top_five = prop_resolved > 0.50) %>%
  ggplot(aes(reorder(type_crime, -prop_resolved), prop_resolved)) +
    geom_col(alpha = 0.5, aes(fill = top_five)) +
    labs(x = "", y = "Proportion of investigations resolved") +
    scale_fill_manual(values = c("red", "blue")) +
    coord_flip() +
    guides(fill = "none") +
    theme_minimal()
    
