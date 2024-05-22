data_toola <- "pen_aligned.csv"
A_NoCursor_pen <- fread(data_toola)%>%
  filter(type=="nocursor", block_num >8)

A_NoCursor_pen <- A_NoCursor_pen %>%
  mutate(nocursor_type = case_when(
    block_num %in% c(21,36,51) ~ "aligned pen",
    block_num %in% c(18,33,48) ~ "aligned hand",
    TRUE ~ "other"),
  nocursor_diff = case_when(
    nocursor_type == "aligned pen" ~ pen_final_angle - target_angle,
    nocursor_type == "aligned hand" ~ hand_final_angle - target_angle))

#Summary statistics
A_NoCursor_summary <- A_NoCursor_pen %>%
  group_by(ppid,nocursor_type) %>%
  summarise(nocursor_median = median(nocursor_diff, na.rm = TRUE),
            nocursor_mean = mean(nocursor_diff, na.rm = TRUE),
            nocursor_sd = sd(nocursor_diff, na.rm = TRUE))

#Identify and remove outliers
filtered_npen <- A_NoCursor_pen %>%
  left_join(A_NoCursor_summary, by = c("ppid", "nocursor_type")) %>%
  mutate(
    is_outlier = nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)
# Identify outliers from the original Localization dataframe
outliers_npen <- A_NoCursor_pen %>%
  left_join(A_NoCursor_summary, by = c("ppid", "nocursor_type")) %>%
  filter(nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd) %>%
  select(-nocursor_median, -nocursor_mean, -nocursor_sd)



#plot
A_Pen_NoCursor <- filtered_npen %>% 
  ggplot(aes(x=nocursor_type, y=nocursor_median, fill=nocursor_type))+
  geom_boxplot()+
  geom_point(aes(fill=nocursor_type),size=2,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.8)+
  theme_classic()+
  scale_color_manual(values = c("aligned hand"="#009966", "aligned pen"="#009966"))+
  scale_fill_manual(values = c("aligned hand"="#009966", "aligned pen"="#009966"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 12), 
    axis.title.y = element_text(size = 12)
  ) +
  xlab("")+
  ylab("End-effector localization (°)")+
  #ylim(-15,70)+
  #scale_y_continuous(breaks = seq(-15, 70, by = 15))+
  #scale_x_discrete(labels=c('Aligned Hand', 'Aligned Pen'))+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) +
  geom_hline(
    yintercept = c(0, 0), linewidth = 0.4,
    colour = "#999999", linetype = "solid"
  ) 


#######################################################################
#Rotated Pen No Cursor
data_penr <- "pen_rotated.csv"
Rnocursor_pen <- fread(data_penr)%>%
  filter(type=="nocursor")

Rnocursor_pen <- Rnocursor_pen %>%
  mutate(nocursor_type = case_when(
    block_num %in% c(15,30,45) ~ "rotated pen",
    block_num %in% c(12,27,42) ~ "rotated hand",
    block_num %in% c(50) ~ "rotated hand-pen"),
    nocursor_diff = case_when(
      nocursor_type == "rotated pen" ~ (pen_final_angle - target_angle)*(-1),
      nocursor_type == "rotated hand" ~ (hand_final_angle - target_angle)*(-1),
      nocursor_type == "rotated hand-pen" ~ (hand_final_angle - target_angle)*(-1)))

#Median
Rnocursor_summary <- Rnocursor_pen %>%
  group_by(ppid,nocursor_type) %>%
  summarise(nocursor_median = median(nocursor_diff),
            nocursor_mean = mean(nocursor_diff),
            nocursor_sd = sd(nocursor_diff))

#Identify and remove outliers
filtered_nrpen <- Rnocursor_pen %>%
  left_join(Rnocursor_summary, by = c("ppid", "nocursor_type")) %>%
  mutate(
    is_outlier = nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

# Identify outliers from the original Localization dataframe
outliers_nrpen <- Rnocursor_pen %>%
  left_join(Rnocursor_summary, by = c("ppid", "nocursor_type")) %>%
  filter(nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd) %>%
  select(-nocursor_median, -nocursor_mean, -nocursor_sd)




#plot
Rpen_nocursor <- filtered_nrpen %>% 
  ggplot(aes(x=nocursor_type, y=nocursor_median, fill=nocursor_type))+
  geom_boxplot()+
  geom_point(aes(fill=nocursor_type),size=2,shape=21, alpha=0.5)+
  geom_line(data = nocursor_median %>%
              filter(nocursor_type %in% c("rotated hand", "rotated pen")),
              aes(group = ppid, color = 'gray', alpha=0.8))+
  theme_classic()+
  scale_color_manual(values = c("rotated hand"="#33CC99", "rotated pen"="#009966", "rotated hand-pen"="#006633"))+
  scale_fill_manual(values = c("rotated hand"="#33CC99", "rotated pen"="#009966","rotated hand-pen"="#006633"))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 12), 
    axis.title.y = element_text(size = 12)
  ) +
  xlab("")+
  ylab("Median no cursor deviation (°)")+
  #ylim(-15,70)+
  #scale_y_continuous(breaks = seq(-15, 70, by = 15))+
  # scale_x_discrete(labels=c('Hand', 'Pen', 'Hand After'))+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) +
  geom_hline(
    yintercept = c(0, 0), linewidth = 0.4,
    colour = "#999999", linetype = "solid"
  ) 


#COMBINE ALL NO CURSOR PLOTS
#library(gridExtra)

#combined_plot <- grid.arrange(hand_localization, Apen_localization, Rpen_localization, ncol = 3)

combined_dataset <- rbind(filtered_npen, filtered_nrpen)
order_of_bars <- c("aligned pen", "rotated pen", "aligned hand", "rotated hand", "rotated hand-pen")
combined_dataset$category <- factor(combined_dataset$nocursor_type, levels = order_of_bars)

bar_colors <- c("plum2","plum3","plum2","plum4","plum3")
Combine_localization <- combined_dataset %>% 
  ggplot(aes(x=category, y=nocursor_median, fill=nocursor_type))+
  geom_boxplot(width = 0.4)+
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
  ylab("Median no cursor deviation (°)")+
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

##Combine hand and pen no cursor plots on one svg file
install.packages("patchwork")
# Load the patchwork package
library(patchwork)

# Set the width ratio for the plots
width_ratio <- c(0.6, 1.4)  # Adjust as needed

# Combine the plots horizontally
combined_plot <- hand_nocursor + Combine_localization + plot_layout(widths = width_ratio)

print(combined_plot)




#--------------------------------------------------------
#reach aftereffects



pen_aftereffects <- combined_dataset %>%
  group_by(nocursor_type, experiment)%>%
  summarise(nocursor_median = median(nocursor_diff, na.rm = TRUE),
            nocursor_mean = mean(nocursor_diff, na.rm = TRUE),
            nocursor_sd = sd(nocursor_diff, na.rm = TRUE)) 

reach_aftereffects <- data.frame(
  experiment = c("hand", "pen", "hand2", "hand-pen"),
  aftereffect = c(10.59087, 13.7760994, 10.4526636, 10.4526636 )
)
print(reach_aftereffects)


ggplot(reach_aftereffects, aes(x = experiment, y = aftereffect)) +
  geom_bar(stat = "identity", fill = "gray", color = "black") +
  labs(x = "Experiment", y = "Aftereffects")




