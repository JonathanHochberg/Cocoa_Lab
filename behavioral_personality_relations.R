# File for computing learning rate and reward enhancement 
# Also, computes correlations between the above variables, 
# and between some questionaire metric(s) and reward enhancement

# read in all training data 
# Load packages
library(data.table)
library(tidyr) # for pipe
library(dplyr) # for pipe
library(plotrix) # for std.error()
library(ggplot2) # for great plots

# function to get slope and intercept of line of best fit, to be used within dplyr pipe
lin_fit <- function(data) {
  the_fit <- lm(movement_time ~ count, data)
  setNames(data.frame(t(coef(the_fit))), c("intercept", "slope"))
}

# set and get working directory 
#setwd("~/Box Sync/DSP_scanner/output_files")
wd = getwd()

# COMPUTE LEARNING RATE OF MT IN TRAINING
# DEFINED AS SLOPE OF LINE OF BEST FIT OF MEAN MOVEMENT TIME BY BLOCK
# GIVES UNIQUE LR VALUE FOR EACH SUBJECT AND SEQUENCE

# Get training data
train_data = fread(sprintf("all_training_output.csv"))
# look only at correct trials and sequences A and B (i.e., ignore incorrect trials and random trials)
MT_correct_train = train_data[ which(train_data$accuracy == 1 & train_data$sequence !=  "random") , ]
# get the mean MT for each subject, block, and sequence (returns a long-form data tbl)
MT_summary_train = MT_correct_train %>%
  group_by(block,sequence,subject) %>%
  summarise(mean_MT = mean(movement_time, na.rm = TRUE))
#  compute learning rates for each subject and sequence
learning_rates = MT_correct_train %>%
  group_by(subject,sequence) %>%
  mutate(count=1:length(sequence)) %>%
  select(subject,sequence,count,movement_time) %>%
  do(lin_fit(.)) %>%
  filter(!(subject %in% c(103, 107, 112, 123, 125))) #subject > 102 & subject != 113 & subject < 119
write.csv(learning_rates,"learning_rates.csv")
# COMPUTE REWARD ENHACNEMENT OF MT IN TEST
# DEFINED AS DIFFERENCE BETWEEN MEAN MT FOR 5$ AND 30$
# GIVES UNIQUE RE VALUE FOR EACH SUBJECT AND SEQUENCE

# Get reward data
  # HINT: use fread("all_reward_output.csv")
test_data = fread("all_reward_output.csv")
  
# get the accuracy for each subject, reward value, and sequence
  # HINT: use a dplyr pipe with group_by() and summarise()
summary_test_tbl = test_data %>%
  group_by(subject, reward, sequence) %>%
  summarise(mean_acc = mean(accuracy), n=n()) 
  
# convert the tbl returned above to a data.frame (so that it plays nicely with reshape())
  # HINT: use as.data.frame()
summary_test_df = as.data.frame(summary_test_tbl)
  
# convert the long-form data.frame to wide-form (unique column for each reward value)
  # HINT: use reshape() with idvar = c("sequence","subject"), timevar = "reward", and direction = "wide"
summary_test_wide = reshape(summary_test_df, idvar = c("sequence","subject"), timevar = "reward", direction = "wide" )

# compute reward enhancements for each subject and sequence
  # HINT1: use group_by() and summarise()
  # HINT2: within summarise(), add new variable reward_enhancement that equals mean_MT.30 - mean_MT.10
reward_enhancements = summary_test_wide %>%
  group_by(subject, sequence) %>%
  summarise(reward_enhancement = mean_acc.30 - mean_acc.10) %>%
  filter(!(subject %in% c(101,102,103, 107, 125))) #subject > 102 & subject != 113 & subject < 119
write.csv(reward_enhancements,"reward_enhancements.csv")
# Questionnaire data 
QS = fread("questionnaire_all_sub_scores.csv") %>%   filter(!(subject %in% c(103, 107, 112, 123, 125)))
BRR = QS$bas_reward_responsiveness

#merge LR and RE data for scatterplot
LR_RE_QS_data_merged <- merge(learning_rates, reward_enhancements, by=c("subject","sequence"))
LR_RE_QS_data_merged$BRR = rep(BRR, each=2)
  
#define LR and RE data
A_learning_rates = learning_rates %>% filter(sequence=="A") %>% select(subject,sequence,slope)
B_learning_rates = learning_rates %>% filter(sequence=="B") %>% select(subject,sequence,slope)
# look for sequence effect on learning rate
t.test(A_learning_rates$slope,B_learning_rates$slope,paire=TRUE)
A_reward_enhancements = reward_enhancements %>% filter(sequence=="A") 
B_reward_enhancements = reward_enhancements %>% filter(sequence=="B")

# COMPUTE CORRELATIONS BETWEEN REWARD ENHANCEMENTS AND LEARNING RATES
# ONE CORRELATION FOR EACH SEQUENCE
  # HINT: use cor.test()
a_LR_RE_cor = cor.test(A_learning_rates$slope, A_reward_enhancements$reward_enhancement)
b_LR_RE_cor = cor.test(B_learning_rates$slope, B_reward_enhancements$reward_enhancement)
  
# COMPUTE CORRELATIONS BETWEEN LEARNING RATES AND SCORES ON A QUESTIONAIRE (one for each sequence)
  # HINT: fread("questionnaire_all_sub_scores.csv") and pick a variable that you suspect would be related to learning rate
#rationale for bas_reward_responsiveness: 
a_LR_QS_cor = cor.test(A_learning_rates$slope, BRR)
b_LR_QS_cor = cor.test(B_learning_rates$slope, BRR)

# COMPUTE CORERLATIONS BETWEEN REWARD ENHACNEMENTS AND SCORES ON A QUESTIONAIRE (one for each sequence)
  # HINT: fread("questionnaire_all_sub_scores.csv") and pick a variable that you suspect would be related to reward enhancement
a_RE_QS_cor = cor.test(A_reward_enhancements$reward_enhancement, BRR)
b_RE_QS_cor = cor.test(B_reward_enhancements$reward_enhancement, BRR)

#correlation for other questionnaires
cor(QS)

#scatterplot for LR and RE correlations
LR_RE_data_merged <- merge(learning_rates, reward_enhancements, by=c("subject","sequence"))
ggplot(data=LR_RE_data_merged, aes(x=slope, y=reward_enhancement, group=sequence)) + 
  geom_point(aes(colour=sequence)) +
  geom_smooth(aes(colour=sequence),method="lm") +
  scale_x_continuous("learning rates (ms)") +
  scale_y_continuous("reward enhancements") +
  ggtitle(sprintf("learning rates by reward enhancements")) +
  theme_minimal()

#scatterplot for LR and QS correlations
ggplot(data=LR_RE_QS_data_merged, aes(x=BRR, y=slope, group=sequence)) + 
  geom_point(aes(colour=sequence)) +
  geom_smooth(aes(colour=sequence),method="lm") + 
  scale_y_continuous("learning rates (change in MT)") +
  scale_x_discrete("bas reward responsiveness") +
  ggtitle(sprintf("learning rates by BRR")) +
  theme_minimal()

#scatterplot for RE and QS correlations
ggplot(data=LR_RE_QS_data_merged, aes(x=BRR, y=reward_enhancement,group=sequence)) + 
  geom_point(aes(colour=sequence)) +
  geom_smooth(aes(colour=sequence),method="lm") + 
  scale_y_continuous("Reward Effect (change in accuracy)") +
  scale_x_continuous("bas reward responsiveness") +
  ggtitle(sprintf("reward enhancements by BRR")) +
  theme_minimal()

# density plot of learning rates
ggplot(learning_rates, aes(x=slope, fill=sequence)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous("Learning Rates") +
  theme_minimal()

# density plot of reward enhancements
ggplot(reward_enhancements, aes(x=reward_enhancements, fill=sequence)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous("Reward Enhancements") +
  theme_minimal()
