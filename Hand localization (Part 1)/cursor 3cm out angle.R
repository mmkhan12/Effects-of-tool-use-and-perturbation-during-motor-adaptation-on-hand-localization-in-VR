setwd("C:/Users/Hp 14/Documents/expand_unity_exp_framework_data-main/data/processedpen/omnibus")
getwd()
dir()

cursor <- read.csv("comb_cursor.csv")


# Function to calculate the distance between two points
distance <- function(x1, z1, x2, z2) {
  return(sqrt((x2 - x1)^2 + (z2 - z1)^2))
}

# Group the data by trial number
grouped_reaches <- cursor %>%
 #filter(type=="rotated")%>%
  group_by(ppid) %>%
  mutate(dist_to_path = distance(home_x, home_z, cursor_pos_x, cursor_pos_z))

# Filter for rows where the distance to path is approximately 3cm
filtered_reaches <- grouped_reaches %>%
  filter(dist_to_path >= 0.027 & dist_to_path <= 0.0305)

# Get the first row for each trial that meets the condition
result_data <- filtered_reaches %>%
  group_by(ppid,trial_num) %>%
  slice_min(order_by = abs(dist_to_path - 0.03), n = 1)

dx <- result_data$cursor_pos_x - result_data$home_x
dz <- result_data$cursor_pos_z - result_data$home_z
# Calculate the angle in radians using atan2 function
angle_final_radians <- atan2(dz, dx)
# Convert the angle from radians to degrees
result_data$cursor_angle_3cm <- angle_final_radians * 180 / pi

# Calculate the deviation
result_data$deviation <- result_data$cursor_angle_3cm - result_data$target_angle

plot <- result_data %>%
  group_by(trial_num)%>%
  summarise(rotated_mean=mean(deviation, na.rm = TRUE),
            reaches_median=median(deviation, na.rm = TRUE),
            sd_err = sd(deviation, na.rm = TRUE),
            n=n(),.groups = 'drop') %>%
  mutate(lower_ci = reaches_median - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
         upper_ci = reaches_median + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))

ggplot(plot, aes(x=trial_num, y=reaches_median)) +
  geom_point()+
  geom_line()

