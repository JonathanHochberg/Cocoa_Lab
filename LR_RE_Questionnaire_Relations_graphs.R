#find which subjects are missing from training / reward / questionnaire

# read in all training data 
# Load packages
library(data.table)
library(tidyr) # for pipe
library(dplyr) # for pipe
library(plotrix) # for std.error()
library(ggplot2) # for great plots
library(Rmisc) # for multiplot


setwd("~/Box Sync/LeeLab/Experiments/data/DSP_scanner/output_files")
getwd()

#get questionnaire data
QS = fread("questionnaire_all_sub_scores.csv") %>%   
    filter(subject > 103 & subject != 113 & subject < 119 & subject != 107 & subject != 112) %>%
    #remove V1 column
    select(-V1)
  
#only get certain subjects from LR data
learning_rates = fread("learning_rates.csv")
LR = learning_rates %>% filter(subject > 103 & subject != 113 & subject < 119)
#LR for each sequence
LR_A = LR  %>% filter(sequence=="A")
LR_B = LR  %>% filter(sequence=="B")

#only get certain subjects from RE data
reward_enhancements = fread("reward_enhancements.csv")
RE = reward_enhancements %>% filter(subject > 103 & subject != 113 & subject != 112 & subject < 119)
#LR for each sequence
RE_A = RE  %>% filter(sequence=="A")
RE_B = RE  %>% filter(sequence=="B")

#reshape data for A sequence
A_melted <- melt(QS, id.vars ='subject')
#add LR column
A_melted$LR = LR_A$slope
#add RE column
A_melted$RE = RE_A$reward_enhancement
#rename columns
A_melted <- dplyr::rename(A_melted, score = value, questionnaire = variable) 
A_melted$sequence = rep("A",length(A_melted$RE))

#reshape data for B sequence
B_melted <- melt(QS, id.vars ='subject')
#add LR column
B_melted$LR = LR_B$slope
#add RE column
B_melted$RE = RE_B$reward_enhancement
#rename columns
B_melted = dplyr::rename(B_melted, score = value, questionnaire = variable) 
B_melted$sequence = rep("B",length(B_melted$RE))

all_melted = rbind(A_melted,B_melted)

# plot of A-sequence RE by bis11 scores
ggplot(A_melted%>%filter(questionnaire=='bis11'),aes(x=RE,y=score)) +
  geom_point() +
  geom_smooth(method="lm")

# TO DO:
# - make 2 plots for each questionnaire score
#   - separate plots for LR and RE
#   - A and B can be on same plot, just use different group/colour
# e.g., for 5 QS, you would make 10 plots
# hint: you'll need to redo the data tbbles above so that they aren't separate for A and B

 
# 
# #plot of LR by A sequence scores 
# p1 <- ggplot(A_melted, aes(x=score, y=LR, group = questionnaire, 
#                           color = questionnaire)) +
#   geom_point() +
#   geom_line() +
#   ggtitle("plot of LR by A sequence scores ") +
#   theme_minimal()
# 
# 
# #plot of RE by A sequence scores 
# p3 <- ggplot(A_melted, aes(x=score, y=RE, group = questionnaire, 
#                            color = questionnaire)) +
#   geom_point() +
#   geom_line() +
#   ggtitle("plot of RE by A sequence scores") +
#   theme_minimal()
# 
# #plot of LR by B sequence scores 
# p2 <- ggplot(B_melted, aes(x=score, y=LR, group = questionnaire, 
#                      color = questionnaire)) +
#   geom_point() +
#   geom_line() +
#   ggtitle("plot of LR by B sequence scores") +
#   theme_minimal()
# 
# #plot of RE by A sequence scores 
# p4 <- ggplot(B_melted, aes(x=score, y=RE, group = questionnaire, 
#                            color = questionnaire)) +
#   geom_point() +
#   geom_line() +
#   ggtitle("plot of RE by A sequence scores") +
#   theme_minimal()
# 
# #multiplot of LR by score and RE by score for sequences A and B
# #png(filename = ".png", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE)
# multiplot(p1,p2,p3,p4, cols=2)
#dev.off()
