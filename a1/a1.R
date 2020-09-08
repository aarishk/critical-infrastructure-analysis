# CMPT 318 (Fall 2018)
# Group Assignment 1
#
# Authors:
# Aarish Kapila
# Che Jung (Kent) Lee
# Karan Sharma
# Razvan Andrei Cretu
# Yernur Nursultanov

library("DescTools") # For Gmean() and Mode()

fileName <- 'Dataset1.txt'
df <- read.table(fileName, header = TRUE, sep = ",")

feature_A <- df$Global_active_power
feature_B <- df$Global_reactive_power
feature_C <- df$Voltage
feature_D <- df$Global_intensity

# 1. Compute the arithmetic and the geometric mean, the median, the mode and the
#    standard deviation for features A and B respectively.
print_stats <- function(x) {
    cat(sprintf("\tArithmetic mean\t= %.02f\n", mean(x, na.rm = TRUE)))
    cat(sprintf("\tGeometric mean\t= %.02f\n", Gmean(x, na.rm = TRUE)))
    cat(sprintf("\tMedian\t\t= %.02f\n", median(x, na.rm = TRUE)))
    cat(sprintf("\tMode\t\t= %.02f\n", Mode(x, na.rm = TRUE)))
    cat(sprintf("\tStandard dev.\t= %.02f\n", sd(x, na.rm = TRUE)))
}

cat("\nFeature A statistics:\n")
print_stats(feature_A)

cat("\nFeature B statistics:\n")
print_stats(feature_B)

# 2. Compute the correlation between each of the four features A, B, C and D
#    using Pearson’s correlation coefficient
print_cor <- function(x, y) {
  cat(sprintf("\tCorrelation\t= %.02f\n", cor(x, y, use="complete.obs", method="pearson")))
}

cat("\nFeature A vs Feature B:\n")
print_cor(feature_A, feature_B)

cat("\nFeature A vs Feature C:\n")
print_cor(feature_A, feature_C)

cat("\nFeature A vs Feature D:\n")
print_cor(feature_A, feature_D)

cat("\nFeature B vs Feature C:\n")
print_cor(feature_B, feature_C)

cat("\nFeature B vs Feature D:\n")
print_cor(feature_B, feature_D)

cat("\nFeature C vs Feature D:\n")
print_cor(feature_C, feature_D)

# 3. For features A and B compute the min and max values on weekdays and weekend
#    days respectively.

# Converting the format
date <- as.POSIXlt(df$Date, format = "%d/%m/%Y")

# Creating a column
df$day <- weekdays(date)

# Separating the days based on weekday and weekend
weekday <- df[df$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),]
weekend <- df[df$day %in% c("Saturday","Sunday"),]

print_min_max <- function(weekdays, weekends) {
    cat(sprintf("\tMin value on weekdays = %.02f\n", min(weekdays, na.rm = TRUE)))
    cat(sprintf("\tMax value on weekdays = %.02f\n", max(weekdays, na.rm = TRUE)))
    cat(sprintf("\tMin value on weekends = %.02f\n", min(weekends, na.rm = TRUE)))
    cat(sprintf("\tMax value on weekends = %.02f\n", max(weekends, na.rm = TRUE)))
}

cat("\nFeature A:\n")
print_min_max(weekday$Global_active_power, weekend$Global_active_power)

cat("\nFeature B:\n")
print_min_max(weekday$Global_reactive_power, weekend$Global_reactive_power)