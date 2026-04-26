#-------------------------------------------------------------------------
# Lucy Toelcke
# April 7, 2026
# ECNS_460: Data Analysis
# Abortion Analysis Final Project -- Creating visuals describing the data
#-------------------------------------------------------------------------

# import necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# Import data
data = read_csv("clean_abortion_data.csv")

# Set theme
theme_update(
  plot.title = element_text(face = "bold"),
  legend.position = "bottom"
)

#-------------------------------------------------------------------------
# Graph set 1: Effect of abortion bans on number of children********

# Graph 1.1: Effect of Abortion Bans on Number of Children
# Create treatment group and indicator
data = data |>
  mutate(
    treated = ifelse(`Abortion Ban` == 1, 1, 0),
    post = ifelse(YEAR >= 2022, 1, 0)
  )

# Collapse to mean number of kids by year and whether there was treatment
plot_data = data |>
  group_by(YEAR, treated) |>
  summarise(mean_kids = mean(NCHILD, na.rm = TRUE), .groups = "drop")

# Plot data
plot1 = ggplot(plot_data, aes(x = YEAR, y = mean_kids, color = factor(`treated`))) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray40") +
  labs(
    title = "Effect of Abortion Bans on Number of Children",
    subtitle = "Difference-in-Differences: Ban vs Non-Ban States",
    x = "Year",
    y = "Average Number of Children",
    color = "Abortion Ban State"
  ) +
  scale_color_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

ggsave("plot1.png", plot = plot1)

# Graph 1.2: Check pre-trends in Fertility (before 2022)
pretrend <- data %>%
  filter(YEAR < 2022) %>%
  group_by(YEAR, treated) %>%
  summarise(mean_kids = mean(NCHILD, na.rm = TRUE), .groups = "drop")

ggplot(pretrend, aes(x = YEAR, y = mean_kids, color = factor(`treated`))) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  labs(
    title = "Pre-Trends in Fertility (Before 2022)",
    x = "Year",
    y = "Average Number of Children"
  ) +
  scale_color_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

# Graph 1.3: Probability of having children
data = data |>
  mutate(has_child = ifelse(NCHILD > 0, 1, 0))

ggplot(data, aes(x = factor(`Abortion Ban`), y = has_child)) +
  stat_summary(fun = mean, geom = "bar", fill = "#4C72B0", width = 0.6) +
  labs(
    title = "Probability of Having Children",
    x = "Abortion Ban State",
    y = "Share with Children"
  )

# Final graph of fertilty before and after 2022
data |>
  mutate(post = ifelse(YEAR >= 2022, "Post-2022", "Pre-2022")) %>%
  group_by(YEAR, `Abortion Ban`, post) %>%
  summarise(mean_kids = mean(NCHILD, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = YEAR, y = mean_kids, color = factor(`Abortion Ban`))) +
  geom_line(size = 1.2) +
  facet_wrap(~post) +
  labs(
    title = "Fertility Before and After 2022",
    color = "Ban State"
  ) +
  scale_color_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Graph 2: Average Age Over Time: Ban vs Non-Ban States
age_plot = data |>
  group_by(YEAR, `Abortion Ban`) |>
  summarise(mean_age = mean(AGE, na.rm = TRUE), .groups = "drop")

ggplot(age_plot, aes(x = YEAR, y = mean_age, color = factor(`Abortion Ban`))) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray40") +
  labs(
    title = "Average Age Over Time: Ban vs Non-Ban States",
    x = "Year",
    y = "Mean Age",
    color = "Ban State"
  ) +
  scale_color_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

#-------------------------------------------------------------------------
# Graph set 3: Education 
# Graph 3.1: Education Composition by State Policy
data = data |>
  mutate(educ_cat = case_when(
    EDUCD < 72 ~ "Less than HS",
    EDUCD >= 72 & EDUCD < 110 ~ "High School",
    EDUCD >= 110 & EDUCD < 123 ~ "Some College",
    EDUCD >= 123 ~ "Bachelor+",
    TRUE ~ NA_character_
  ))

ggplot(data, aes(x = educ_cat, fill = factor(`treated`))) +
  geom_bar(position = "fill") +
  labs(
    title = "Education Composition by State Policy",
    x = "Education Level",
    y = "Proportion",
    fill = "Ban State"
  ) +
  scale_fill_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

# Graph 3.2: Education Levels: Ban vs Non-Ban States
ggplot(data, aes(x = educ_cat, fill = factor(`Abortion Ban`))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Education Levels: Ban vs Non-Ban States",
    x = "Education Level",
    y = "Count",
    fill = "Ban State"
  ) +
  scale_fill_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

# Graph 3.3: Education Trends Over Time by Policy State
edu_time = data |>
  group_by(YEAR, `Abortion Ban`, educ_cat) |>
  summarise(n = n(), .groups = "drop")

ggplot(edu_time, aes(x = YEAR, y = n, color = factor(`Abortion Ban`))) +
  geom_line() +
  facet_wrap(~educ_cat) +
  labs(
    title = "Education Trends Over Time by Policy State",
    x = "Year",
    y = "Count"
  ) +
  scale_color_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Graph set 4: Income
log_income = log(data$INCWAGE)
# Graph 4.0: Income distribution
ggplot(data, aes(x = log_income)) +
  geom_histogram(bins = 40, fill = "#4C72B0", color = "white") +
  labs(
    title = "Distribution of Log Income",
    x = "Log(Income + 1)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

# Graph 4.1: Income vs Age
data = data |>
  mutate(
    log_income = log(INCWAGE + 1),
    age_bin = cut(AGE, breaks = c(12, 14, 16, 18, 19), include.lowest = TRUE)
  )

age_income = data |>
  group_by(AGE) |>
  summarise(mean_income = mean(log_income, na.rm = TRUE))

ggplot(age_income, aes(x = AGE, y = mean_income)) +
  geom_line(size = 1.4, color = "#2C7FB8") +
  geom_point(size = 2) +
  labs(
    title = "Income Life-Cycle Pattern (Ages 12–19)",
    x = "Age",
    y = "Log Income"
  ) +
  theme_minimal(base_size = 14)

# Graph 4.2: Income by Education Level
ggplot(data, aes(x = factor(EDUC), y = INCWAGE)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Income by Education Level",
    x = "Education",
    y = "Wage Income"
  )

# Graph 4.3: Income Distribution by Race
ggplot(data, aes(x = factor(RACE), y = INCWAGE)) +
  geom_boxplot(fill = "#64B5CD", alpha = 0.6) +
  labs(
    title = "Income Distribution by Race",
    x = "Race",
    y = "Wage Income"
  )

# Graph 4.4: Average Income by State********
state_income = data |>
  group_by(STATEFIP, statefip1) |>
  summarise(mean_income = mean(INCWAGE, na.rm = TRUE))

plot2 = ggplot(state_income, aes(x = reorder(statefip1, mean_income), y = mean_income)) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Average Teen Income by State",
    x = "State",
    y = "Mean Wage Income"
  ) +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 9)
  )

ggsave("plot2.png", plot = plot2)

# Graph 4.5: Income Over Time: Ban vs Non-Ban States
inc_plot = data |>
  group_by(YEAR, `Abortion Ban`) |>
  summarise(mean_income = mean(INCWAGE, na.rm = TRUE), .groups = "drop")

ggplot(inc_plot, aes(x = YEAR, y = mean_income, color = factor(`Abortion Ban`))) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "gray40") +
  labs(
    title = "Income Over Time: Ban vs Non-Ban States",
    x = "Year",
    y = "Average Wage Income",
    color = "Ban State"
  ) +
  scale_color_manual(values = c("0" = "#2C7BB6", "1" = "#D7191C"))

# Graph 4.6: Income by Number of Children
child_income = data |>
  group_by(NCHILD) |>
  summarise(mean_income = mean(log_income, na.rm = TRUE))

ggplot(child_income, aes(x = NCHILD, y = mean_income)) +
  geom_line(size = 1.3, color = "#C44E52") +
  geom_point(size = 2) +
  labs(
    title = "Income by Number of Children",
    x = "Number of Children",
    y = "Log Income"
  ) +
  theme_minimal(base_size = 14)
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Graph 5: Average number of children to teens in each state*******

state_kids = data |>
  group_by(STATEFIP, statefip1) |>
  summarise(
    avg_children = mean(NCHILD, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
  
plot3 = ggplot(state_kids, aes(x = reorder(statefip1, avg_children), y = avg_children, fill = avg_children)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#56B1F7", high = "#CA0020") +
  labs(
    title = "Teen Fertility by State",
    x = "State",
    y = "Average Number of Children"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 6))

ggsave("plot3.png", plot = plot3)

#-------------------------------------------------------------------------
# Graph 6: State map of abortion bans*********

# Get U.S. state shapefile
states_map = states(cb = TRUE)

state_policy = data |>
  group_by(STATEFIP) |>
  summarise(
    ban = max(`Abortion Ban`, na.rm = TRUE),
    .groups = "drop"
  )

# Fix FIPS format
state_policy = state_policy |>
  mutate(STATEFP = sprintf("%02d", STATEFIP))

# merge with map
map_data = states_map |>
  left_join(state_policy, by = "STATEFP")

map_data = map_data |>
  filter(!STATEFP %in% c("02", "15"))

map_data = map_data |>
  dplyr::filter(!NAME %in% c(
    "United States",
    "Guam",
    "American Samoa",
    "District of Columbia",
    "Puerto Rico",
    "United States Virgin Islands",
    "Commonwealth of the Northern Mariana Islands"
  ))

# plot choropleth
plot4 = ggplot(map_data) +
  geom_sf(aes(fill = factor(ban)), color = "white", linewidth = 0.2) +
  
  scale_fill_manual(
    values = c("0" = "#4C72B0", "1" = "#C44E52"),
    labels = c("No Ban", "Ban/hostile state"),
    name = "Abortion Policy"
  ) +
  
  coord_sf(expand = FALSE) +
  
  labs(
    title = "Abortion Policy Across U.S. States",
    subtitle = "Post 2022",
    caption = "Source: IPUMS / state policy data"
  ) +
  
  theme_void(base_size = 12) +
  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
  )

ggsave("plot4.png", plot = plot4)

#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# Graph 7: race and number of children******

# Make a race
data = data |>
  mutate(race_label = case_when(
    RACE == 1 ~ "White",
    RACE == 2 ~ "Black",
    RACE == 3 ~ "American Indian",
    RACE == 4 ~ "Chinese",
    RACE == 5 ~ "Japanese",
    RACE == 6 ~ "Asian/Pacific Islander",
    RACE == 7 ~ "Other",
    RACE == 8 ~ "Multiracial",
    TRUE ~ NA_character_
  ))

race_children = data |>
  filter(!is.na(race_label)) |>
  group_by(race_label) |>
  summarise(avg_children = mean(NCHILD, na.rm = TRUE))

plot5 = ggplot(race_children, aes(x = reorder(race_label, avg_children), y = avg_children)) +
  geom_col(fill = "#4C72B0") +
  coord_flip() +
  labs(
    title = "Average Number of Children by Race",
    x = "Race",
    y = "Average Number of Children"
  ) +
  theme_minimal()

ggsave("plot5.png", plot = plot5)

#-------------------------------------------------------------------------
