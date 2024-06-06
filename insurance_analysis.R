# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data
data <- read.csv("dataset_HW1_insurance.csv")

# Check for missing data
summary(data)

# Data imputation
# Numerical data: Replacing NA with median
data$age <- ifelse(is.na(data$age), median(data$age, na.rm = TRUE), data$age)
data$bmi <- ifelse(is.na(data$bmi), median(data$bmi, na.rm = TRUE), data$bmi)
data$children <- ifelse(is.na(data$children), median(data$children, na.rm = TRUE), data$children)

# Categorical data: Replacing NA with mode
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

data$sex <- ifelse(is.na(data$sex), getmode(data$sex), data$sex)
data$smoker <- ifelse(is.na(data$smoker), getmode(data$smoker), data$smoker)
data$region <- ifelse(is.na(data$region), getmode(data$region), data$region)

# Check for outliers with boxplots
boxplot(data$age, main="Age")
boxplot(data$bmi, main="BMI")

# Exploratory Data Analysis (EDA)
ggplot(data, aes(x = age)) + geom_histogram(binwidth = 1, fill = "blue", color = "black")
ggplot(data, aes(x = bmi)) + geom_histogram(binwidth = 1, fill = "red", color = "black")
# Continue for other variables and use different types of plots

# Linear regression model 
model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = data)
summary(model)

# Model Assumptions Validation
# Checking for normality of residuals
hist(residuals(model))

# Checking for homoscedasticity
plot(model)

