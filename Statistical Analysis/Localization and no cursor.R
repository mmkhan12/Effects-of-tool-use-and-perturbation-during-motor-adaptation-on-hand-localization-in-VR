


combined_dat <- bind_rows(localization, nocursor)

combine <- combined_dat %>%
  filter(type == 'rotated')%>%
  mutate(
    localization = ifelse(trial_type == "localization", deviation, NA),
    nocursor = ifelse(trial_type == "no cursor", deviation, NA)
  )

df <- combine %>%
  group_by(ppid, group) %>%
  summarise(
    localization = mean(localization, na.rm = TRUE),
    nocursor = mean(nocursor, na.rm = TRUE)
  ))


# Plot
custom_colors <- c("hand" = "palegreen2", "hand_pen" = "darkcyan", "pen" = "coral2")
custom_labels <- c("hand" = "Hand (Exp 1)", "hand_pen" = "Hand after pen (Exp 2)", "pen" = "Pen (Exp 2)")
custom_labels <- c("Hand (Exp 1)", "Pen (Exp 2)", "Hand after pen (Exp 2)")

ggplot(df, aes(x = nocursor, y = localization, color = group)) +
  geom_point() + # Scatterplot
  geom_smooth(method = "lm", se = FALSE) + # Add regression line
  labs(x = "Angular reach deviation during no cursor (°)", y = "Changes in localization (°)", color = NULL) + # Axis and legend labels
  scale_color_manual(values = custom_colors, labels = custom_labels)+
  theme_classic() +
  theme(
    legend.position = c(1.0, 1.05), # Adjust legend position
    legend.justification = c(1, 1) # Justify legend to top right
  )
 # annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, 
         #  label = paste("hand:", round(r_squared_values[1], 3), 
             #            "\nhand_pen:", round(r_squared_values[2], 3), 
              #           "\npen:", round(r_squared_values[3], 3)))

# Fit linear regression models for each group
lm_models <- lapply(unique(df$group), function(g) {
  lm_model <- lm(localization ~ nocursor, data = subset(df, group == g))
  return(lm_model)
})

# Get R-squared values for each group
r_squared_values <- sapply(lm_models, function(model) summary(model)$r.squared)
print(r_squared_values)


# Subset the data for each group
group1_data <- subset(df, group == "hand")
group2_data <- subset(df, group == "hand_pen")
group3_data <- subset(df, group == "pen")

# Perform correlation analysis for each group
cor_group1 <- cor.test(group1_data$localization, group1_data$nocursor)
cor_group2 <- cor.test(group2_data$localization, group2_data$nocursor)
cor_group3 <- cor.test(group3_data$localization, group3_data$nocursor)

# Print correlation coefficients and p-values
print("Group 1:")
print(cor_group1)
print("Group 2:")
print(cor_group2)
print("Group 3:")
print(cor_group3)


