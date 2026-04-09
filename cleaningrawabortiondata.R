#----------------------------------------------------------------------
# Lucy Toelcke
# April 7, 2026
# ECNS_440: Data Analysis
# Abortion Analysis Final Project -- Cleaning and merging the data set
#----------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)

# Set working directory
# Import individual by year data
data = read_csv("abortionraw2.csv.gz")
abortionsbystate = read_excel("abortionbansbystate.xlsx")

# Merge data set to abortion bans by state data set (m:1 merge)
merged_data <- left_join(data, abortionsbystate, by = c("STATEFIP" = "statefip"))
# Check merge
sum(is.na(merged_data$`Abortion Ban`))

# Explore the data
n_distinct(data$YEAR)
n_distinct(data$SAMPLE)

# Clean data
cleaned_data = merged_data |>
  filter(SEX == 2, AGE >= 12, AGE <= 19) # Keep only females and only ages 12-19

# Check for missing values
colSums(is.na(cleaned_data))

# Quick summary
summary(cleaned_data)

# Save clean data set
write_csv(cleaned_data, "clean_abortion_data.csv")

