# Makes Plots of Means with within-subject SE bars
# DVs: reaction time, movement time, accuracy
# IV: Reward (5, 10, 30)

# Load packages
library(data.table)
library(tidyr) # for pipe
library(dplyr) # for pipe
library(plotrix) # for std.error()
library(ggplot2) # for great plots

# set and get working directory 
#setwd("~/Box Sync/DSP_scanner/output_files")
wd = getwd()
  
# read in .csv files using fread()
MT_data = fread("correct_mean_MT_data.csv")
RTacc_data = fread("all_mean_data.csv")

# Pipe to transform MT_data
  # Ultimately, adds a column of normalized mean movement times
  # SE of these normalized MTs should be used for error bars, given repeated-measures design
new_MT_data = MT_data %>%
  # Add subject means 
  # i.e., mean MT across reward values (for each subject)
  group_by(subject) %>%
  mutate(sub_mean_MT = mean(mean_MT)) %>%
  # Add grand mean
  # i.e., mean MT across all subjects and reward values
  ungroup() %>%
  mutate(grand_mean_MT = mean(mean_MT)) %>%
  # Add normalized mean_MTs
  # mean_MT - subject mean + grand mean
  group_by(subject,reward, sequence) %>%
  mutate(norm_mean_MT = mean_MT - sub_mean_MT + grand_mean_MT)

# Pipe to transform mean_acc and mean_RT
# Ultimately, adds a column of normalized mean accuracies and mean RTs
# SE of these normalized RTaccs should be used for error bars, given repeated-measures design
new_RTacc_data = RTacc_data %>%
  # Add subject means 
  # i.e., mean RTacc across reward values (for each subject)
  group_by(subject) %>%
  mutate(reward, sub_mean_acc = mean(mean_acc), 
                 sub_mean_RT = mean(mean_RT)) %>%
  # Add grand mean
  # i.e., mean RTacc across all subjects and reward values
  ungroup() %>%
  mutate(grand_mean_acc = mean(mean_acc), 
         grand_mean_RT = mean(mean_RT)) %>%
  # Add normalized mean_RTaccs
  # mean_RTacc - subject mean + grand mean
  group_by(subject,reward, sequence) %>%
  mutate(norm_mean_acc = mean_acc - sub_mean_acc + grand_mean_acc, 
         norm_mean_RT = mean_RT - sub_mean_RT + grand_mean_RT)

#create a new data summary for MT
summary_MT_data = new_MT_data %>%
  group_by(reward, sequence) %>%
  summarise(group_mean_MT = mean(mean_MT), group_norm_SE_MT = std.error(norm_mean_MT), n = n())

#create a new data summary for RT and Acc
summary_RTacc_data = new_RTacc_data %>%
  group_by(reward, sequence) %>%
  summarise(group_mean_acc = mean(mean_acc), group_norm_SE_acc = std.error(norm_mean_acc), 
            group_mean_RT = mean(mean_RT), group_norm_SE_RT = std.error(norm_mean_RT), n = n())

# PLOTTING PARAMETERS
dodge = 0
ebar_width = 0.13
ebar_type = 5 # 5 = dashed
save_units = "in"
save_height = 5
save_width = 5

# plot of mean MT with normalized SE bars
# factor() makes x-axis categorical
ggplot(data=summary_MT_data,aes(x=factor(reward),y=group_mean_MT, group = sequence)) + 
  geom_point(aes(colour=sequence)) + # add points
  # add error bars
  geom_errorbar(aes(colour=sequence, ymin=group_mean_MT-group_norm_SE_MT,
                    ymax=group_mean_MT+group_norm_SE_MT), 
                    width=ebar_width) +
  # add x-axis label
  scale_x_discrete("Reward Value ($)") +
  # add y-axis label and increase number of ticks
  scale_y_continuous("Mean Movement Time (ms)", breaks = scales::pretty_breaks(n = 10)) +
  # add nice theme
  theme_minimal()
ggsave(filename="group_mean_mt_by_reward.png",device="png",
       height=5,width=5,units="in")


# plot of mean accuracy with normalized SE bars
# factor() makes x-axis categorical
ggplot(data=summary_RTacc_data,aes(x=factor(reward),y=group_mean_acc,
                                   colour=sequence,group=sequence)) + 
  geom_point() + # add points
  geom_line() +
  # add error bars
  geom_errorbar(aes(colour=sequence, ymin=group_mean_acc-group_norm_SE_acc,
                    ymax=group_mean_acc+group_norm_SE_acc), width=ebar_width) +
  # add x-axis label
  scale_x_discrete("Reward Value ($)") +
  # add y-axis label and increase number of ticks
  scale_y_continuous("Mean Accuracy", breaks = scales::pretty_breaks(n = 5)) +
  # add nice theme
  theme_minimal()
ggsave(filename="group_mean_acc_by_reward.png",device="png",
       height=5,width=5,units="in")


# plot of mean reaction time with normalized SE bars
# factor() makes x-axis categorical
ggplot(data=summary_RTacc_data,aes(x=factor(reward),y=group_mean_RT, group = sequence)) + 
  geom_point(aes(colour=sequence)) + # add points
  # add error bars
  geom_errorbar(aes(colour=sequence, ymin=group_mean_RT-group_norm_SE_RT,
                    ymax=group_mean_RT+group_norm_SE_RT), width=ebar_width) +
  # add x-axis label
  scale_x_discrete("Reward Value ($)") +
  # add y-axis label and increase number of ticks
  scale_y_continuous("Mean Reaction Time (ms)", breaks = scales::pretty_breaks(n = 5)) +
  # add nice theme
  theme_minimal() 
ggsave(filename="group_mean_rt_by_reward.png",device="png",
       height=5,width=5,units="in")