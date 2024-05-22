data_toola <- "pen_aligned.csv"
Alocalization_pen <- fread(data_toola)%>%
  filter(type=="localization", block_num >8)
 
local_diff <- Alocalization_pen$localizing_angle - Alocalization_pen$arc_aquired_angle
Alocalization_pen$local_diff <- Alocalization_pen$localizing_angle - Alocalization_pen$arc_aquired_angle

Alocalization_pen <- Alocalization_pen %>%
    mutate(localization_type = case_when(
    block_num %in% c(9,13,24,28,39,43) ~ "aligned pen",
    block_num %in% c(11,15,26,30,41,45) ~ "aligned hand",
    TRUE ~ "other"
  ))

#Median
Alocalization_sum <- Alocalization_pen %>%
  group_by(ppid,localization_type) %>%
  summarise(local_median = median(local_diff, na.rm = TRUE),
            local_mean = mean(local_diff, na.rm = TRUE),
            local_sd = sd(local_diff, na.rm = TRUE))

#Identify and remove outliers
filtered_alignedpen <- Alocalization_pen %>%
  left_join(Alocalization_sum, by = c("ppid", "localization_type")) %>%
  mutate(
    is_outlier = local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)


# Identify outliers from the original Localization dataframe
outliers <- Alocalization_pen %>%
  left_join(Alocalization_sum, by = c("ppid", "localization_type")) %>%
  filter(local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd) %>%
  select(-local_median, -local_mean, -local_sd)






#plot
Apen_localization <- filtered_alignedpen %>% 
  ggplot(aes(x=localization_type, y=local_median, fill=localization_type))+
  geom_boxplot(data=filtered_alignedpen %>%
                 filter(localization_type=='aligned hand'),
               width=0.4, alpha=0.3)+
  geom_boxplot(data=filtered_alignedpen %>%
                 filter(localization_type=='aligned pen'),
               width=0.4, alpha=0.3)+
  geom_point(aes(fill=localization_type),size=2,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.8)+
  theme_classic()+
  scale_color_manual(values = c("aligned hand"="#009999", "aligned pen"="#009999"))+
  scale_fill_manual(values = c("aligned hand"="#009999", "aligned pen"="#009999"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 12), 
    axis.title.y = element_text(size = 12)
  ) +
  xlab("")+
  ylab("End-effector localization (°)")+
  scale_y_continuous(breaks = seq(-15, 50, by = 15), limits = c(-15, 25))+
  scale_x_discrete(labels=c('Aligned Hand', 'Aligned Pen'))+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) +
  geom_hline(
    yintercept = c(0, 0), linewidth = 0.4,
    colour = "#999999", linetype = "solid"
  ) 


#######################################################################
#Rotated Pen Localization
data_penr <- "pen_rotated.csv"
Rlocalization_pen <- fread(data_penr)%>%
  filter(type=="localization")

local_diff <- Rlocalization_pen$localizing_angle - Rlocalization_pen$arc_aquired_angle
Rlocalization_pen$local_diff <- Rlocalization_pen$localizing_angle - Rlocalization_pen$arc_aquired_angle

Rlocalization_pen <- Rlocalization_pen %>%
  mutate(localization_type = case_when(
    block_num %in% c(3,7,18,22,33,37) ~ "rotated pen",
    block_num %in% c(5,9,20,24,35,39) ~ "rotated hand",
    block_num %in% c(48) ~ "rotated pen hand",
    TRUE ~ "other"
  ))

#Median
Rlocalization_summary <- Rlocalization_pen %>%
  group_by(ppid,localization_type) %>%
  summarise(local_median = median(local_diff, na.rm = TRUE),
            local_mean = mean(local_diff, na.rm = TRUE),
            local_sd = sd(local_diff, na.rm = TRUE))

#Identify and remove outliers
filtered_rotatedpen <- Rlocalization_pen %>%
  left_join(Rlocalization_summary, by = c("ppid", "localization_type")) %>%
  mutate(
    is_outlier = local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

# Identify outliers from the original Localization dataframe
outliers <- Rlocalization_pen %>%
  left_join(Rlocalization_summary, by = c("ppid", "localization_type")) %>%
  filter(local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd) %>%
  select(-local_median, -local_mean, -local_sd)



#Comparison stats
#paired t-test
rotated_hand <- Rlocalization_summary %>%
  filter(localization_type == "rotated hand") %>%
  pull(local_median)

rotated_pen <- Rlocalization_summary %>%
  filter(localization_type == "rotated pen") %>%
  pull(local_median)

aligned_hand <- Alocalization_sum %>%
  filter(localization_type == "aligned hand") %>%
  pull(local_median)

aligned_pen <- Alocalization_sum %>%
  filter(localization_type == "aligned pen") %>%
  pull(local_median)

#t-test for hand localization in pen trials
diffs_hand <- rotated_hand - aligned_hand
t.test(diffs_hand)

#t-test for pen localization in pen trials
diffs_pen <- rotated_pen - aligned_pen
t.test(diffs_pen)

#plot
Rpen_localization <- filtered_rotatedpen %>% 
  ggplot(aes(x=localization_type, y=local_median, fill=localization_type))+
  geom_boxplot(data=Rlocalization_summary %>%
                 filter(localization_type=='rotated hand'),
               width=0.4, alpha=0.4)+
  geom_boxplot(data=Rlocalization_summary %>%
                 filter(localization_type=='rotated pen'),
               width=0.4, alpha=0.4)+
  geom_boxplot(data=Rlocalization_summary %>%
                 filter(localization_type=='rotated pen hand'),
               width=0.4, alpha=0.4)+
  geom_point(aes(fill=localization_type),size=2,shape=21, alpha=0.5)+
  geom_line(data = Rlocalization_summary %>%
              filter(localization_type %in% c("rotated hand", "rotated pen")),
            aes(group = ppid, color = 'gray', alpha=0.8))+
  theme_classic()+
  scale_color_manual(values = c("rotated hand"="#00CCCC", "rotated pen"="#339999", "rotated pen hand"="#3399CC"))+
  scale_fill_manual(values = c("rotated hand"="#00CCCC", "rotated pen"="#339999","rotated pen hand"="#3399CC"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 12), 
    axis.title.y = element_text(size = 12)
  ) +
  xlab("")+
  ylab("End-effector localization (°)")+
  ylim(-15,70)+
  scale_y_continuous(breaks = seq(-15, 70, by = 15))+
 # scale_x_discrete(labels=c('Hand', 'Pen', 'Hand After'))+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) +
  geom_hline(
    yintercept = c(0, 0), linewidth = 0.4,
    colour = "#999999", linetype = "solid"
  ) 


#COMBINE ALL LOCALIZATION PLOTS
#library(gridExtra)

#combined_plot <- grid.arrange(hand_localization, Apen_localization, Rpen_localization, ncol = 3)

combined_dataset <- rbind(filtered_alignedpen, filtered_rotatedpen)
order_of_bars <- c("aligned pen", "rotated pen", "aligned hand", "rotated hand", "rotated pen hand")
combined_dataset$category <- factor(combined_dataset$localization_type, levels = order_of_bars)

bar_colors <- c("skyblue1","turquoise","skyblue1","turquoise","turquoise4")
Combine_localization <- combined_dataset %>% 
  ggplot(aes(x=category, y=local_median, fill=localization_type))+
  geom_boxplot(width=0.4)+
  geom_point(aes(fill=category),size=1.5,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.4)+
  scale_fill_manual(values = alpha(bar_colors, alpha=0.9))+
  theme_classic()+
  geom_hline(
    yintercept = c(0), linewidth = 0.4,
    colour = "#999999", linetype = "solid"
  ) +
  geom_hline(
    yintercept = c(15), linewidth = 0.4,
    colour = "#999999", linetype = "dotted"
  ) +
  xlab("")+
  ylab("End-effector localization (°)")+
  geom_vline(xintercept = c(2.5), color = "light gray", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = c(4.5), color = "light gray", linetype = "solid", size = 0.5) +
  scale_x_discrete(labels=c('Aligned', 'Rotated', 'Aligned','Rotated', 'Rotated-pen'))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 11), 
    axis.title.y = element_text(size = 11)
  ) +
  # theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())+
  scale_y_continuous(
    limits = c(-20, 50),
    breaks = c(0, -15, 15, 30, 45),
    labels = c(0, -15, 15, 30, 45)
  )+
  theme(text = element_text(size = 11))+
  theme(axis.text.x = element_text(color = "black"))+
  theme(axis.text.y = element_text(color = "black"))

print(Combine_localization)



#------------------------------------------------------
 new_df <- combined_dataset %>%
  mutate(hand_angle = localizing_angle - arc_aquired_angle)

summary_stats <- new_df %>%
  group_by(ppid, localization_type) %>%
  summarize(mean_hand_angle = mean(hand_angle),
            median_hand_angle = median(hand_angle),
            sd_hand_angle = sd(hand_angle))

# Create boxplot with mean and standard deviation bars
ggplot(summary_stats, aes(x = localization_type, y = mean_hand_angle, fill = localization_type)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Hand Angles Across Participants and Localization Types",
       x = "Participant",
       y = "Mean Hand Angle") +
  theme_classic()



# Create boxplot with mean, standard deviation bars, and individual data points
ggplot(summary_stats, aes(x = localization_type, y = mean_hand_angle, fill = localization_type)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.2), color = "black", alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Hand Angles Across Participants and Localization Types",
       x = "Localization Type",
       y = "Hand Angle") +
  theme_classic()
