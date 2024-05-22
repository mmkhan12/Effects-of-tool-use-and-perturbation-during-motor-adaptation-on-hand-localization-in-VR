#Load packages
library(data.table)
library(dplyr)

#EXPERIMENT 1
#Load omnibus trial_results file
data_hand <- "omnibus_hand.csv"
hand_data <- fread(data_hand)

#Filter for last 12 trials (3 sets) for each type
hand <- hand_data %>%
  filter((type == 'aligned' & trial_num > 162) | (type == 'rotated' & trial_num > 387))
#Check to make sure that 12 trials for each are being displayed
unique_trial_numbers <- unique(hand$trial_num)
print(unique_trial_numbers)

#Perform ANOVA to compare the baseline hand from rotated
hand <- hand %>%
  mutate(diff = ifelse(type == "aligned", hand_3cm_out_angle - target_angle, hand_3cm_out_angle - (target_angle - 30)))%>%
  group_by(trial_num, type)%>%
  summarise(mean=mean(diff, na.rm = TRUE),
            median=median(diff, na.rm = TRUE))

anova_result <- aov(mean ~ type, data=hand)
print(anova_result)

# Step 3: Post-hoc analysis
# Install and load the 'lsmeans' package for post-hoc tests
install.packages("lsmeans")
library(lsmeans)

# Conduct post-hoc tests to compare baseline vs. rotated groups
posthoc_result <- lsmeans(anova_result, pairwise ~ type, adjust = "tukey")

# Print the post-hoc test results
print(posthoc_result)

######EXPERIMENT 2
#Aligned
data_toola <- "omnibus_aligned.csv"
pen_aligned <- fread(data_toola)
pen_aligned$ppid <- factor(pen_aligned$ppid)
#Rotated
data_toolhand <- "omnibus_rotated.csv"
pen_rotated <- fread(data_toolhand)
pen_rotated$ppid <- factor(pen_rotated$ppid)

#Filter for last 12 trials (3 sets) for each type
# Filter 'pen_aligned' for trial_num > 358
filtered_aligned <- pen_aligned %>%
  filter(type=='aligned',trial_num > 358)

# Filter 'pen_rotated' for trial_num > 359 and trial_num < 381
filtered_rotated <- pen_rotated %>%
  filter(type=='rotated',trial_num > 359 & trial_num < 381)

# Combine the filtered data frames
filtered_data <- bind_rows(filtered_aligned, filtered_rotated)

#Peform ANOVA to compare the baseline hand from baseline rotated
pen <- filtered_data %>%
  mutate(diff = pen_3cm_out_angle - target_angle)%>%
  group_by(trial_num, type)%>%
  summarise(mean=mean(diff, na.rm = TRUE),
            median=median(diff, na.rm = TRUE))

anova_result <- aov(mean ~ type, data=pen)
print(anova_result)

# Conduct post-hoc tests to compare baseline vs. rotated groups
posthoc_result <- lsmeans(anova_result, pairwise ~ type, adjust = "tukey")
print(posthoc_result)

#Part 3: Compare first 12 rotated pen with first 12 rotated hand after training with pen
rotated_pen <- pen_rotated %>%
  filter(type=='rotated', trial_num >1 & trial_num < 14)
rotated_hand <- pen_rotated %>%
  filter(type=='rotated', trial_num >426 & trial_num < 439)
rotated_pen$type <- 'rotated_pen'
rotated_hand$type <- 'rotated_hand'
filtered <- bind_rows(rotated_pen, rotated_hand)
pen_hand <- filtered %>%
  mutate(diff = ifelse(pen_present, pen_3cm_out_angle - target_angle, hand_3cm_out_angle - target_angle)) %>%
  group_by(trial_num, type) %>%
  summarise(mean = mean(diff, na.rm = TRUE))

anova_result <- aov(mean ~ type, data=pen_hand)
print(anova_result)

posthoc_result <- lsmeans(anova_result, pairwise ~ type, adjust = "tukey")
print(posthoc_result)

#-------------------------------------------------
#Include in r markdown
  
library(tidyr)
#One way ANOVA for rotated hand (Exp 1), rotated pen(Exp2), rotated hand after training with pen (Exp2)
#Exp1 rotated hand df for 3 trial sets (first, second, last)
handrot1 <- hand_data %>%
  filter(type == 'rotated' & between(trial_num, 194, 293) & !(between(trial_num, 202, 289))) %>%
  mutate(ppid = as.character(as.integer(ppid)))%>%
  mutate(trial_set = case_when(
    between(trial_num, 194, 197) ~ "1",
    between(trial_num, 198, 201) ~ "2",
    between(trial_num, 290, 293) ~ "3",
    TRUE ~ NA_character_
  ))

#Exp2 rotated pen
data_toolhand <- "pen_rotated.csv"
pen_rot <- fread(data_toolhand)
pen_rotated <- pen_rot %>%
  filter(between(trial_num, 2, 101) & !(between(trial_num, 10, 97))) %>%
  #mutate(ppid = as.character(as.integer(ppid)))%>%
  mutate(trial_set = case_when(
    between(trial_num, 2, 5) ~ "1",
    between(trial_num, 6, 9) ~ "2",
    between(trial_num, 98, 101) ~ "3",
    TRUE ~ NA_character_
  ))

pen_rotated <- pen_rotated %>%
  mutate(ppid = as.character(ppid))


#Exp2 rotated hand after training with pen
data_tool <- "pen_rotated.csv"
hand_trainpen <- fread(data_tool)
rotated_hand <- hand_trainpen %>%
  filter(type=='rotated'& between(trial_num, 391, 438) & !(between(trial_num, 399, 434))) %>%
  mutate(trial_set = case_when(
    between(trial_num, 391, 394) ~ "1",
    between(trial_num, 395, 398) ~ "2",
    between(trial_num, 435, 438) ~ "3",
    TRUE ~ NA_character_
  ))
handrot1$group <- 'hand'
pen_rotated$group <- 'pen'
rotated_hand$group <- 'hand_pen'
#Bind df rows
filtered_data <- bind_rows(handrot1, pen_rotated, rotated_hand)

sum_stats <- filtered_data %>%
  mutate(diff = ifelse(!is.na(cursor_3cm_out_angle), cursor_3cm_out_angle - target_angle)) %>%
  bind_rows(
    pen_rotated %>%  # Calculate diff for pen_rotated dataframe
      mutate(diff = ifelse(pen_present, pen_3cm_out_angle - target_angle)),
    rotated_hand %>%  # Calculate diff for rotated_hand dataframe
      mutate(diff = hand_3cm_out_angle - target_angle))%>%
  filter(abs(diff) < 90)%>%
  group_by(ppid, group, trial_set) %>%
  summarise(median = median(diff, na.rm = TRUE))

anova_result <- aov(median ~ trial_set * group + Error(ppid/(trial_set * group)), data = sum_stats)

# Print the ANOVA summary
summary(anova_result)

#post-hoc test
posthoc_result <- lsmeans(model, pairwise ~ type, adjust = "tukey")
print(posthoc_result)

install.packages("BayesFactor")

library(BayesFactor)

bf <- ttestBF(median ~ trial_set * group, data = data.frame(sum_stats), progress = FALSE)
print(bf)
