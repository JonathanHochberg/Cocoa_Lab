# file for plotting group means and (all) indivual values of MT 

# Load packages
library(data.table)
library(tidyr) # for pipe
library(dplyr) # for pipe
library(plotrix) # for std.error()
library(ggplot2) # for great plots


all_sub_train_data = fread("all_training_output.csv")

all_train_data_correct = all_sub_train_data %>% filter(accuracy==1 & sequence!='random')

# Plot of all subjects' movement times by trial, grouped by subject
all_train_data_correct = all_train_data_correct %>% 
  group_by(subject,sequence) %>%
  mutate(count = 1:length(sequence))

# sequence A
# plot of linear models of all subjects' movement times by trial
A_train_data = all_train_data_correct %>% filter(sequence=='A')
ggplot(data=A_train_data, 
       aes(x=count,y=movement_time,
           colour=factor(subject),group=factor(subject))) + 
  #geom_point(aes(alpha=0.01)) + # add points 
  geom_smooth(method="lm",se=FALSE) +
  # add x-axis label
  scale_x_continuous("Trial", breaks = scales::pretty_breaks(n = 5)) +
  # add y-axis label and increase number of ticks
  scale_y_continuous("Movement time (ms)", breaks = scales::pretty_breaks(n = 5)) +
  # add nice theme
  theme_minimal()
ggsave(filename="all_sub_mt_by_trial_A.png",device="png",
       height=6,width=6,units="in")


# sequence B
# plot of linear models of all subjects' movement times by trial
B_train_data = all_train_data_correct %>% filter(sequence=='B')
ggplot(data=B_train_data, 
       aes(x=count,y=movement_time,
           colour=factor(subject),group=factor(subject))) + 
  #geom_point(aes(alpha=0.01)) + # add points 
  geom_smooth(method="lm",se=FALSE) +
  # add x-axis label
  scale_x_continuous("Trial", breaks = scales::pretty_breaks(n = 5)) +
  # add y-axis label and increase number of ticks
  scale_y_continuous("Movement time (ms)", breaks = scales::pretty_breaks(n = 5)) +
  # add nice theme
  theme_minimal()
ggsave(filename="all_sub_mt_by_trial_B.png",device="png",
       height=6,width=6,units="in")

# Plot of all subjects' movement times by trial, w/ group level trend
ggplot(data=all_train_data_correct, 
       aes(x=count,y=movement_time,group=sequence,colour=sequence)) + 
  #geom_point(aes(alpha=0.01)) + # add points 
  geom_smooth(method="lm") +
  # add x-axis label
  scale_x_continuous("Trial", breaks = scales::pretty_breaks(n = 5)) +
  # add y-axis label and increase number of ticks
  scale_y_continuous("Movement time (ms)", breaks = scales::pretty_breaks(n = 5)) +
  # add nice theme
  theme_minimal()
ggsave(filename="group_mt_by_trial.png",device="png",
       height=5,width=5,units="in")
