#Set working directory
setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

#EXPERIMENT 1 FOR HAND LOCALIZATION
data_hand <- "omnibus_hand.csv"
local <- fread(data_hand) %>%
  filter(type=="localization", block_num >8)

#filter the hand localizations and create new type
hand_local <- local %>%
  mutate(type = case_when(
    trial_num < 191 ~ "aligned",
    trial_num > 194 ~ "rotated"))%>%
  mutate(ppid = as.character(as.integer(ppid))) %>%
  mutate(group = "hand") %>%
  mutate(deviation = localizing_angle - arc_aquired_angle)

#Summary statistics
hand_summary <- hand_local %>%
  group_by(ppid, type, group) %>%
  summarise(median = median(deviation, na.rm = TRUE),
            mean = mean(deviation, na.rm = TRUE),
            sd = sd(deviation, na.rm = TRUE))

#Identify and remove outliers
hand <- hand_local %>%
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
  filter(block_num %in% c(9,13,24,28,39,43)) %>%
  mutate(type = "aligned") %>%
  mutate(group = "pen")%>%
  mutate(deviation = localizing_angle - arc_aquired_angle)

rotated <- pen_rotated %>%
  filter(block_num %in% c(3,7,18,22,33,37)) %>%
  mutate(type = "rotated") %>%
  mutate(group = "pen")%>%
  mutate(deviation = localizing_angle - arc_aquired_angle)
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
  filter(block_num %in% c(11,15,26,30,41,45))%>%
  mutate(type = "aligned")%>%
  mutate(group = "hand_pen")%>%
  mutate(deviation = localizing_angle - arc_aquired_angle)

rotated_hp <- pen_rotated %>%
  filter(block_num %in% c(48))%>%
  mutate(type = "rotated")%>%
  mutate(group = "hand_pen")%>%
  mutate(deviation = localizing_angle - arc_aquired_angle)
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

localization <- bind_rows(hand,pen,hand_pen)

localization <- localization %>%
  mutate(trial_type = "localization")

#Perform 2x3 ANOVA
anova_result <- aov(deviation ~ type * group + Error(ppid/(type * group)), data = localization)

# Summarize the ANOVA results
summary(anova_result)
