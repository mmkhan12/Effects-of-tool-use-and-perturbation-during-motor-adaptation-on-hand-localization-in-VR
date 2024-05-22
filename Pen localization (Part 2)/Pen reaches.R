#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

###Part 2 Pen Reaches
#Aligned
data_toola <- "pen_aligned.csv"
pen_data <- fread(data_toola)

#ppid from int to chr
hand_data$ppid <- as.character(hand_data$ppid)

#Filter last 3 blocks of aligned pen reaches (deviation = pen_3cm_out_angle - target angle )
aligned_pen <- pen_data %>%
  filter(type=='aligned', block_num >43)%>%
  group_by(trial_num)%>%
  summarise(aligned_mean=mean(deviation, na.rm = TRUE),
            reaches_median=median(deviation, na.rm = TRUE),
            sd_err = sd(deviation, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = reaches_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = reaches_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))

# Create a new variable with consecutive trial numbers
aligned_pen$sequential_trials <- 1:(nrow(aligned_pen))

# Plot the data continuously without gaps using the new sequential trial numbers
aligned_penr <- aligned_pen %>%
  ggplot(aes(x = sequential_trials, y = aligned_median,color = factor(ppid)))+  
  theme_classic()+
  #theme(legend.position = "none")+
  labs(
    x = "Trial Number",
    y = "Median angular deviation (°)",
    title="Aligned Pen Reaches"
  )

aligned_penr <- aligned_penr+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) +
  geom_line(na.rm = TRUE)

# set font size to 12
aligned_penr <- aligned_penr +
  theme(text = element_text(size = 11))
# add confidence intervals and data points
aligned_penr <- aligned_penr + geom_ribbon(aes(ymin = lower_ci,
                                               ymax = upper_ci),
                                           fill = "#660033", alpha=0.5, color=NA)+ 
  geom_line(color = "#660033")+ geom_point(color = "#660033", size=1)+
  scale_y_continuous(breaks = seq(-15, 70, by = 15), limits = c(-15, 65))

#COMBINE BOTH ALIGNED PLOTS FROM PART 1 AND 2 OF THE EXPERIMENT
#Find number of participants to confirm all data is included
aligned_hand %>% 
  distinct(ppid) %>% 
  count()

aligned_pen %>% 
  distinct(ppid) %>% 
  count()

#COMBINE
aligned_plot <- bind_rows(aligned_hand %>% mutate(source = "hand"),
                      aligned_pen %>% mutate(source = "pen"))
aligned_plot <- ggplot(aligned_plot, aes(x = sequential_trials, y = reaches_median, color = source))+
  geom_point()+
  geom_line()+
  #ggtitle("Aligned Pen and Hand Reaches") +
  xlab("Trial Number") +
  ylab("Median angular deviation (°)") +
  theme_classic()

aligned_plot <- aligned_plot+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) 

aligned_plot <- aligned_plot +
  theme(text = element_text(size = 12))
# add confidence intervals and data points

# Modify the combined plot to have different geom_ribbon for each dataset
aligned_plot <- aligned_plot +
  geom_ribbon(data = aligned_hand, aes(ymin = lower_ci, ymax = upper_ci),
              fill = "#CC0066", alpha = 0.4, color = NA) +
  geom_ribbon(data = aligned_pen, aes(ymin = lower_ci, ymax = upper_ci),
              fill = "#660033", alpha = 0.4, color = NA) +
  geom_line(data = aligned_hand, aes(color = "hand")) +  
  geom_line(data = aligned_pen, aes(color = "pen")) +   
  geom_point(data = aligned_hand, aes(color = "hand"), size = 1) + 
  geom_point(data = aligned_pen, aes(color = "pen"), size = 1) +   
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65))+
  scale_color_manual(values = c("pen" = "#660033", "hand" = "#CC0066"))+
  labs(fill = NULL, color = NULL)+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  theme(legend.position = "none")+
  theme(panel.background = element_rect(fill = "#F0F0F0"))
  
# Print or display the modified combined plot
print(aligned_plot)





##################################################################################
#Pen Rotated reaches first 100 trials
data_toolhand <- "pen_rotated.csv"
pen_train <- fread(data_toolhand)

#ROTATED filter first 100 trials
rotated_pen <- pen_train %>%
  filter(type=='rotated', block_num < 3) %>%
#rotated_diff <- rotated_pen$pen_3cm_out_angle - rotated_pen$target_angle
#rotated_pen$rotated_diff <- rotated_pen$pen_3cm_out_angle - rotated_pen$target_angle
  group_by(trial_num)%>%
  summarise(rotated_mean=mean(deviation,na.rm = TRUE),
            reaches_median=median(deviation,na.rm = TRUE),
            sd_err = sd(deviation, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = reaches_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = reaches_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))

# Re-number the trial numbers starting from 1
rotated_pen$sequential_trials <- 1:(nrow(rotated_pen))

rotated_pen$sequential_trials <- rotated_pen$sequential_trials + 40


rotated_penreach <- ggplot(rotated_pen, aes(x = trial_num, y = rotated_diff, color = factor(ppid))) +
  geom_point()+ 
  geom_line()+
  theme_classic()+
  #theme(legend.position = "none")+
  labs(
    x = "Trial Number",
    y = "Angular deviation (°)",
    title="Exp 2: Rotated Pen Reaches"
  )+
  scale_y_continuous(breaks = seq(-80, 80, by = 20), limits = c(-80, 80))+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  )

print(rotated_penreach)





#ROTATED PLOTS FOR POSTER
rotated_pen <- pen_train %>%
  filter(type=='rotated', block_num < 3)

rotated_diff <- rotated_pen$pen_3cm_out_angle - rotated_pen$target_angle
rotated_pen$rotated_diff <- rotated_pen$pen_3cm_out_angle - rotated_pen$target_angle

rotated_pen <- rotated_pen %>%
  group_by(trial_num)%>%
  summarise(rotated_mean=mean(rotated_diff),
            rotated_median=median(rotated_diff),
            sd_err = sd(rotated_diff, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = rotated_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = rotated_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))



# Re-number the trial numbers starting from 1
rotated_pen$sequential_trials <- 1:(nrow(rotated_pen))

# Plot the data with re-numbered trials starting from 1 and custom breaks/labels
rotated_penreach <- ggplot(rotated_pen, aes(x = sequential_trials, y = rotated_median)) +
  geom_point()+ 
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "Trial Number",
    y = "Median angular deviation (°)"
  )


rotated_penreach <- rotated_penreach+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) 

# set font size to 11
rotated_penreach <- rotated_penreach +
  theme(text = element_text(size = 11))
# add confidence intervals and data points
rotated_penreach <- rotated_penreach + geom_ribbon(aes(ymin = lower_ci,
                                                   ymax = upper_ci),
                                               fill = "#660033", alpha=0.5, color=NA)+ 
  geom_line(color = "#660033")+ geom_point(color = "#660033", size=1)+
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65))



#COMBINE ROTATED HAND AND ROTATED PEN REACHES
rotated_plot <- bind_rows(rotated_hand %>% mutate(source = "hand"),
                          rotated_pen %>% mutate(source = "pen"))
rotated_plot <- ggplot(rotated_plot, aes(x = sequential_trials, y = rotated_median, color = source))+
  #ggtitle("Aligned Pen and Hand Reaches") +
  xlab("Trial Number") +
  ylab("Median angular deviation (°)") +
  theme_classic()

rotated_plot <- rotated_plot+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) 

rotated_plot <- rotated_plot +
  theme(text = element_text(size = 11))
# add confidence intervals and data points

# Modify the combined plot to have different geom_ribbon for each dataset
rotated_plot <- rotated_plot +
  geom_ribbon(data = rotated_hand, aes(ymin = lower_ci, ymax = upper_ci),
              fill = "#CC0066", alpha = 0.4, color = NA) +
  geom_ribbon(data = rotated_pen, aes(ymin = lower_ci, ymax = upper_ci),
              fill = "#660033", alpha = 0.4, color = NA) +
  geom_line(data = rotated_hand, aes(color = "hand")) +  
  geom_line(data = rotated_pen, aes(color = "pen")) +   
  geom_point(data = rotated_hand, aes(color = "hand"), size = 1) + 
  geom_point(data = rotated_pen, aes(color = "pen"), size = 1) +   
  scale_y_continuous(breaks = seq(-15, 70, by = 15), limits = c(-20, 70))+
  scale_color_manual(values = c("pen" = "#660033", "hand" = "#CC0066"))+
  labs(fill = NULL, color = NULL)+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(legend.position = "none")
  

# Print or display the modified combined plot
print(rotated_plot)



##################################################################################
#ROTATED HAND REACHES AFTER TRAINING WITH PEN
data_toolhand <- "pen_rotated.csv"
pen_hand <- fread(data_toolhand)

rotated <- pen_hand %>%
  filter(type=='rotated', block_num > 46)

#rotated_diff <- rotated$hand_3cm_out_angle - rotated$target_angle
rotated$deviation <- rotated$hand_3cm_out_angle - rotated$target_angle
rotated <- rotated %>%
  #filter(abs(rotated_diff) < 90)
  group_by(trial_num)%>%
  summarise(rotated_mean=mean(deviation, na.rm = TRUE),
            reaches_median=median(deviation, na.rm = TRUE),
            sd_err = sd(deviation, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = reaches_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = reaches_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))


# Re-number the trial numbers starting from 1
rotated$sequential_trials <- 1:(nrow(rotated))

rotated$sequential_trials <- rotated$sequential_trials + 150

# Plot the data with re-numbered trials starting from 1 and custom breaks/labels
rotated_hand <- ggplot(rotated, aes(x = sequential_trials, y = rotated_median)) +
  geom_point()+ 
  theme_classic()+
  theme(legend.position = "none")+
  labs(
    x = "Trial Number",
    y = "Median angular deviation (°)"
  )

rotated_hand <- rotated_hand+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) 

# set font size to 11
rotated_hand <- rotated_hand +
  theme(text = element_text(size = 11))
# add confidence intervals and data points
rotated_hand <- rotated_hand + geom_ribbon(aes(ymin = lower_ci,
                                                       ymax = upper_ci),
                                                   fill = "#CC0066", alpha=0.5, color=NA)+ 
  geom_line(color = "#CC0066")+ geom_point(color = "#CC0066", size=1)+
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65))+
  theme(panel.background = element_rect(fill = "#F0F0F0"))+
  theme(axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())





###################################################
#Combine all reaches plots
# Combine aligned and rotated datasets into one for ribbons
combined_ribbons <- bind_rows(
  mutate(aligned_hand, source = "hand"),
  mutate(aligned_pen, source = "pen"),
  mutate(rotated_hand, source = "hand"),
  mutate(rotated_pen, source = "pen"),
  mutate(rotated, source = "hand")
)

# Create the plot
plots <- ggplot(combined_ribbons, aes(x = sequential_trials, y = reaches_median, color = source, fill = source)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.4, color = NA) +
  scale_color_manual(values = c("hand" = "#CC0066", "pen" = "#660033")) +
  scale_fill_manual(values = c("hand" = "#CC0066", "pen" = "#660033")) +
  labs(x = "Trial Number", y = "Median angular deviation (°)", color = NULL, fill = NULL) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#F0F0F0")) +
  geom_hline(yintercept = c(0, 30), linewidth = 0.4, colour = "#999999", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65))
  #theme(axis.text.x = element_blank())+
  #theme(axis.ticks.x = element_blank())+
  #theme(legend.position = "none")+

# Print the plot
print(plots)