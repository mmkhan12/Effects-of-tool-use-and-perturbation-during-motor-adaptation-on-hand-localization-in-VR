#Load omnibus trial_results file
hand_data <- "omnibus_hand.csv"
localization <- fread(hand_data)%>%
  filter(type=="localization")%>%
  filter(!(trial_num >= 19 & trial_num <= 26))

Localization <- localization %>%
  mutate(localization_type = case_when(
    trial_num < 191 ~ "aligned hand",
    trial_num > 194 ~ "rotated hand"))
local_arc_acqr <- Localization$localizing_angle - Localization$arc_aquired_angle
Localization$local_diff <- local_arc_acqr

#Median
localization_summary <- Localization %>%
  group_by(ppid,localization_type) %>%
  summarise(local_median = median(local_diff, na.rm = TRUE),
            local_mean = mean(local_diff, na.rm = TRUE),
            local_sd = sd(local_diff, na.rm = TRUE))

#Identify and remove outlier trials
filtered_data <- Localization %>%
  left_join(localization_summary, by = c("ppid", "localization_type")) %>%
  mutate(
    is_outlier = local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

# Identify outliers from the original Localization dataframe
outliers <- Localization %>%
  left_join(localization_summary, by = c("ppid", "localization_type")) %>%
  filter(local_diff > local_mean + 3 * local_sd | local_diff < local_mean - 3 * local_sd) %>%
  select(-local_median, -local_mean, -local_sd)


#Comparison stats
#paired t-test
aligned_group <- localization_summary %>%
  filter(localization_type == "aligned hand") %>%
  pull(local_median)

rotated_group <- localization_summary %>%
  filter(localization_type == "rotated hand") %>%
  pull(local_median)

diffs <- rotated_group - aligned_group
t.test(diffs)

filtered_data %>%
  group_by(localization_type) %>%
  summarise(num_points = n())

#plot
hand_localization <- filtered_data %>% 
  ggplot(aes(x=localization_type, y=local_median, fill=localization_type))+
  geom_boxplot(width=0.4, alpha = 0.6)+
  geom_point(aes(fill=localization_type),size=2,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.4)+
  theme_classic()+
  scale_color_manual(values = c("aligned hand"="skyblue1", "rotated hand"="skyblue1"))+
  scale_fill_manual(values = c("aligned hand"="skyblue1", "rotated hand"="skyblue1"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 11), 
    axis.title.y = element_text(size = 11)
  ) +
  xlab("")+
  ylab("End-effector localization (°)")+
  geom_hline(
    yintercept = c(0), linewidth = 0.4,
    colour = "#999999", linetype = "solid"
  ) +
  geom_hline(
    yintercept = c(15), linewidth = 0.4,
    colour = "#999999", linetype = "dotted"
  ) +
  scale_x_discrete(labels = c("aligned hand" = "Aligned", "rotated hand" = "Rotated"))+
  scale_y_continuous(
    limits = c(-20, 50),
    breaks = c(0, -15, 15, 30, 45),
    labels = c(0, -15, 15, 30, 45)
  )+
  theme(text = element_text(size = 11))+
  theme(axis.text.x = element_text(color = "black"))+
  theme(axis.text.y = element_text(color = "black"))
#theme(plot.margin = unit(c(0, 10, 0, 0), "cm"))
#scale_y_continuous(breaks = seq(-15, 30, by = 15), limits = c(-15, 30))+
#theme(axis.ticks.x = element_blank())+
#theme(axis.text.x = element_blank())
print(hand_localization)

