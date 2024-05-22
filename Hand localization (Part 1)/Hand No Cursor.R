library(ggpubr)
#install.packages("ggpubr")
#Load omnibus trial_results file
hand_data <- "omnibus_hand.csv"

#Filter df for only nocursor trials and exclude practice block
data <- fread(hand_data) %>%
 filter(type=="nocursor", block_num >7)
#Create subtype to differentiate between no cursor trials before and after training
NoCursor <- data %>%
  mutate(nocursor_type = case_when(
    trial_num <= 191 ~ "aligned hand",
    trial_num >= 343 ~ "rotated hand"))
#Calculate the angular reach deviation by subtracting endpoint and target angle
nocursor_diff <- (NoCursor$hand_final_angle - NoCursor$target_angle)*(-1)
NoCursor$nocursor_diff <- nocursor_diff

#Summary statistics
nocursor_summary <- NoCursor %>%
  group_by(ppid,nocursor_type) %>%
  summarise(nocursor_median = median(nocursor_diff, na.rm = TRUE),
            nocursor_mean = mean(nocursor_diff, na.rm = TRUE),
            nocursor_sd = sd(nocursor_diff, na.rm = TRUE)) 
#Identify and remove outliers
filtered_nhand <- NoCursor %>%
  left_join(nocursor_summary, by = c("ppid", "nocursor_type")) %>%
  mutate(
    is_outlier = nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)
# Identify outliers from the original Localization dataframe
outliers_nhand <- NoCursor %>%
  left_join(nocursor_summary, by = c("ppid", "nocursor_type")) %>%
  filter(nocursor_diff > nocursor_mean + 3 * nocursor_sd | nocursor_diff < nocursor_mean - 3 * nocursor_sd) %>%
  select(-nocursor_median, -nocursor_mean, -nocursor_sd)

#Comparison stats
#paired t-test
naligned_group <- nocursor_summary %>%
  filter(nocursor_type == "aligned hand") %>%
  pull(nocursor_mean)

nrotated_group <- nocursor_summary %>%
  filter(nocursor_type == "rotated hand") %>%
  pull(nocursor_mean)

ndiffs <- nrotated_group - naligned_group
t.test(ndiffs)

#plot
hand_nocursor <- filtered_nhand %>% 
  ggplot(aes(x=nocursor_type, y=nocursor_median, fill=nocursor_type))+
  geom_boxplot(width=0.4)+
  geom_point(aes(fill=nocursor_type),size=2,shape=21, alpha=0.5)+
  geom_line(aes(group=ppid), color='gray', alpha=0.4)+
  theme_classic()+
  scale_color_manual(values = c("aligned hand"="plum2", "rotated hand"="plum2", alpha = 0.9))+
  scale_fill_manual(values = c("aligned hand"="plum2", "rotated hand"="plum2", alpha = 0.9))+
  theme(
    legend.position="none",
    axis.text = element_text(size = 11), 
    axis.title.y = element_text(size = 11)
  ) +
  xlab("")+
  ylab("Median no cursor deviation (°)")+
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
print(hand_nocursor)


#---------------------------------------------------------------------

#Reach aftereffects

aftereffects <- NoCursor %>%
  group_by(nocursor_type, experiment)%>%
  summarise(nocursor_median = median(nocursor_diff, na.rm = TRUE),
            nocursor_mean = mean(nocursor_diff, na.rm = TRUE),
            nocursor_sd = sd(nocursor_diff, na.rm = TRUE)) 

aftereffects <- aftereffects %>%
  group_by(experiment)%>%
  summarize(aftereffect = mean(nocursor_mean[nocursor_type == "rotated hand"]) - mean(nocursor_mean[nocursor_type == "aligned hand"]))
  
  
ggplot(aftereffects, aes(x = experiment, y = aftereffect)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(x = "Experiment", y = "Aftereffects")

####Reach aftereffects individual data points
new_aftereffects <- NoCursor %>%
  group_by(experiment, ppid)%>%
  summarize(aftereffect = mean(nocursor_diff[nocursor_type == "rotated hand"]) - mean(nocursor_diff[nocursor_type == "aligned hand"]))

ggplot(new_aftereffects, aes(x = experiment, y = aftereffect)) +
  geom_point() +
  labs(x = "", y = "Aftereffects")
