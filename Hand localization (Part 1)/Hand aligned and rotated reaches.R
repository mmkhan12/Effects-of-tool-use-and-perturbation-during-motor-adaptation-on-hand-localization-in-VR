#Load packages
library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr) 
library(magrittr)

#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

#Load omnibus trial_results file
data_hand <- "omnibus_hand.csv"
hand_data <- fread(data_hand)

#ppid from int to chr
hand_data$ppid <- as.character(hand_data$ppid)

###ALIGNED SESSION
#Filter last 3 blocks of aligned hand reaches
aligned_hand <- hand_data %>%
  filter(type=='aligned', block_num > 19)%>%
  group_by(trial_num)%>%
  summarise(aligned_mean=mean(deviation, na.rm = TRUE),
            reaches_median=median(deviation, na.rm = TRUE),
            sd_err = sd(deviation, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = reaches_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = reaches_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))

  
  aligned_hand$sequential_trials <- 1:(nrow(aligned_hand))
  
  
###ROTATED session
rotated_hand <- hand_data %>%
  filter(type=='rotated', block_num < 30)%>%
  group_by(trial_num)%>%
  summarise(rotated_mean=mean(deviation, na.rm = TRUE),
            reaches_median=median(deviation, na.rm = TRUE),
            sd_err = sd(deviation, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
            mutate(lower_ci = reaches_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
                   upper_ci = reaches_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))

# Re-number the trial numbers starting from 1
rotated_hand$sequential_trials <- 1:(nrow(rotated_hand))
#move the rotated plot further along the x axis so it doesn't overlap with aligned sessions when combining
rotated_hand$sequential_trials <- rotated_hand$sequential_trials + 40

#Combine both dfs to make one plot with aligned and rotated hand reaches
combined <- ggplot() +
  geom_line(data=aligned_hand, aes(x=trial_num, y=reaches_median), color="pink")+
  geom_line(data=rotated_hand, aes(x=trial_num, y=reaches_median), color="pink")+
  labs(x = "Trial number", y = "Median angular deviation (°)") +
  theme_classic()+
  geom_hline(
    yintercept = c(0, 30), linewidth = 0.4,
    colour = "#999999", linetype = "dashed"
  ) +
  scale_y_continuous(breaks = seq(-80, 80, by = 20), limits = c(-80, 80))

combined <- combined +
  geom_ribbon(data = aligned_hand, aes(x = trial_num, ymin = lower_ci, ymax = upper_ci), fill = "#CC0066", alpha = 0.5, color = NA) + 
  geom_ribbon(data = rotated_hand, aes(x = trial_num, ymin = lower_ci, ymax = upper_ci), fill = "#CC0066", alpha = 0.5, color = NA) + 
  geom_line(color = "#CC0066") +
  geom_point(color = "#CC0066", size = 1) +
  scale_y_continuous(breaks = seq(-15, 65, by = 15), limits = c(-15, 65)) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank())
print(combined)
