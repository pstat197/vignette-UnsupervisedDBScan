# Preprocessing data
# setwd("/Users/aarti/Downloads/Capstone/197a/Vignette/vignette-UnsupervisedDBScan")

library(readr)
library(readxl)
library(visdat)
library(dplyr)
library(caret)

personality <- read.csv("data/raw/personality_synthetic_dataset.csv")
head(personality)

# Which variables have missing data, if any
vis_miss(personality) # No missing data yay! 

# Converting the response variable as factoring
personality$personality_type <- as.factor(personality$personality_type)

# Summary Statistics
summary(personality) 

# Checking for Duplicates
sum(duplicated(personality)) # No duplicates! 