#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

#EXPERIMENT 1 FOR HAND NOCURSOR
data_hand <- "omnibus_hand.csv"
nocursor <- fread(data_hand) %>%
  filter(type=="nocursor", block_num >7)

#filter the hand no cursors and create new cursor type and type
hand_nocur <- nocursor %>%
  mutate(type = case_when(
    trial_num <= 191 ~ "aligned",
    trial_num >= 343 ~ "rotated"))%>%
  mutate(ppid = as.character(as.integer(ppid))) %>%
  mutate(group = "hand") %>%
  mutate(deviation = hand_final_angle - target_angle)

#Summary statistics
hand_summary <- hand_nocur %>%
  group_by(ppid, type, group) %>%
  summarise(median = median(deviation, na.rm = TRUE),
            mean = mean(deviation, na.rm = TRUE),
            sd = sd(deviation, na.rm = TRUE))

#Identify and remove outliers
hand <- hand_nocur %>%
  left_join(hand_summary, by = c("ppid","type", "group")) %>%
  mutate(
    is_outlier = deviation > mean + 3 * sd | deviation < mean - 3 * sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

##################PEN ROTATED NO CURSOR
# Read the CSV files
pen_aligned <- read.csv("pen_aligned.csv")
pen_rotated <- read.csv("pen_rotated.csv")
# Filter and add a "type" variable based on block numbers
aligned <- pen_aligned %>%
  filter(block_num %in% c(21, 36, 51)) %>%
  mutate(type = "aligned") %>%
  mutate(group = "pen")%>%
  mutate(deviation = pen_final_angle - target_angle)

rotated <- pen_rotated %>%
  filter(block_num %in% c(15, 30, 45)) %>%
  mutate(type = "rotated") %>%
  mutate(group = "pen")%>%
  mutate(deviation = pen_final_angle - target_angle)
#combine both dfs
pen_comb <- bind_rows(aligned, rotated)

#Summary statistics
pen_summary <- pen_comb %>%
  group_by(ppid, type, group) %>%
  summarise(median = median(deviation, na.rm = TRUE),
            mean = mean(deviation, na.rm = TRUE),
            sd = sd(deviation, na.rm = TRUE))

#Identify and remove outliers
pen <- pen_comb %>%
  left_join(pen_summary, by = c("ppid","type", "group")) %>%
  mutate(
    is_outlier = deviation > mean + 3 * sd | deviation < mean - 3 * sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

#Hand after training with pen
aligned_hp <- pen_aligned %>%
  filter(block_num %in% c(18,33,48))%>%
  mutate(type = "aligned")%>%
  mutate(group = "hand_pen")%>%
  mutate(deviation = hand_final_angle - target_angle)

rotated_hp <- pen_rotated %>%
  filter(block_num %in% c(50))%>%
  mutate(type = "rotated")%>%
  mutate(group = "hand_pen")%>%
  mutate(deviation = hand_final_angle - target_angle)
#combine both dfs
handpen <- bind_rows(aligned_hp, rotated_hp)

#Summary statistics
handpen_summary <- handpen %>%
  group_by(ppid, type, group) %>%
  summarise(median = median(deviation, na.rm = TRUE),
            mean = mean(deviation, na.rm = TRUE),
            sd = sd(deviation, na.rm = TRUE))

#Identify and remove outliers
hand_pen <- handpen %>%
  left_join(handpen_summary, by = c("ppid", "type", "group")) %>%
  mutate(
    is_outlier = deviation > mean + 3 * sd | deviation < mean - 3 * sd
  ) %>%
  filter(!is_outlier) %>%
  select(-is_outlier)

nocursor <- bind_rows(hand,pen,hand_pen)

nocursor <- nocursor %>%
  mutate(trial_type = "no cursor")

#Perform 2x3 ANOVA
anova_result <- aov(deviation ~ type * group + Error(ppid/(type * group)), data = nocursor)

# Summarize the ANOVA results
summary(anova_result)
