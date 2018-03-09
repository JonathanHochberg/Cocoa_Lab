# Script to make box-plots
# DVs: mean accuracy, mean reaction time, mean movement time (correct trials)
# IV: reward value
# Author: JH 1/20/2018

# Load packages
library(data.table)

# set and get working directory 
setwd("~/Box Sync/DSP_scanner/output_files")
wd = getwd()

# read in .csv files using fread()
dt = fread("correct_mean_MT_data.csv")
dt2 = fread("all_mean_data.csv")

# makes side-by-side box plot for mean_RT (using all_mean_data.csv)
boxplot(dt2$mean_RT~dt2$reward, 
        main="Boxplot of Mean RT by Reward Level", ylab="Mean Response time", 
        xlab="Reward Level",names=c("5","10","30"))

# makes side-by-side box plot for mean_acc (using all_mean_data.csv)
boxplot(dt2$mean_acc~dt2$reward, 
        main="Boxplot of Mean acc by Reward Level", ylab="Mean Accuracy", 
        xlab="Reward Level",names=c("5","10","30"))

# makes side-by-side box plot for mean_MT (using correct_mean_MT_data.csv)
boxplot(dt$mean_MT~dt$reward, 
        main="Boxplot of Mean MT by Reward Level", 
        ylab="Mean Movement Time", xlab="Reward Level",names=c("5","10","30"))
