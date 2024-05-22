#Load packages
library(dplyr)

#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

#Load omnibus trial_results file
hand_data <- read.csv("omnibus_raw.csv")

#Delete columns you want to get rid of
hand_data <- hand_data[, !(names(hand_data) %in% c("session_num", "trial_num_in_block", "start_time", "end_time",
                                               "step_timestamp", "hand", "home_x", "home_y", "home_z", "target_size_m",
                                               "rotation_size", "cursor_size_m", "arc_radius_or_target_distance_m", "hand_pos_x",
                                               "hand_pos_y", "hand_pos_z", "hand_pos_time_stamp", "cursor_pos_x", "cursor_pos_y", "cursor_pos_z",
                                               "cursor_pos_time_stamp", "hand_3cm_out_time", "cursor_3cm_out_time", "arc_aquired_time", "localizing_time",
                                               "Indicator_position_x", "Indicator_position_y", "Indicator_position_z", "Indicator_position_time_stamp", "X"))]

# Add a new column "deviation" with the specified calculations
hand_data <- hand_data %>%
  mutate(deviation = ifelse(type == "aligned", hand_3cm_out_angle - target_angle,
                            cursor_3cm_out_angle - target_angle))%>%
  filter(abs(deviation) < 90)


library(openxlsx)

write.csv(hand_data, "omnibus_hand.csv")
