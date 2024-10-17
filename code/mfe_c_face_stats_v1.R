# This script will run stats on mini_mfe data.
# Author: Kianoosh Hosseini at NDCLab @FIU (https://Kianoosh.info; https://NDClab.com)
# Last Update: 2024-08-15 (YYYY-MM-DD)

library(tidyverse)
library(dplyr)
library(stringr)
library(psycho)
library(car)
library(lme4)
library(ggplot2)
library(emmeans)
library(report)
library(sjPlot)
library(effsize)


#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Google Drive/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-face-dataset"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "derivatives", "psychopy", "stat_output", sep ="/", collapse = NULL) # input data directory

main_df <-  read.csv(file = paste(processed_file_input, "processed_data_mfe_c_face_Proj_v1.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))


# Check the values in every column in main_df and remove the outliers based on +- 3SD.
# Write a function that removes the outliers from an array
remove_outliers <- function(x) {
  mean_x <- mean(as.numeric(x), na.rm = TRUE)
  sd_x <- sd(as.numeric(x), na.rm = TRUE)
  for (xx in 1:length(x)){
    if (!is.na(x[xx])){
      if (x[xx] < (mean_x - 3*sd_x) | x[xx] > (mean_x + 3*sd_x)){
        x[xx] <- NA
      }
    }
  }
  return(x)
}
# apply this outlier removing function to all the columns in the dataframe except for participant ID column.
new_main_df <- main_df
new_main_df[-c(1, ncol(new_main_df))] <- apply(main_df[-c(1, ncol(main_df))], 2, remove_outliers)
main_df <- new_main_df

mean(main_df$epepq15_scrdTotal_s1_r1_e1, na.rm = TRUE) #
sd(main_df$epepq15_scrdTotal_s1_r1_e1, na.rm = TRUE) #
median(main_df$epepq15_scrdTotal_s1_r1_e1, na.rm = TRUE) #


mean(main_df$post_error_acc, na.rm = TRUE) #
sd(main_df$post_error_acc, na.rm = TRUE) #


mean(main_df$post_error_rt, na.rm = TRUE) #
sd(main_df$post_error_rt, na.rm = TRUE) #


# flanker task stats
# Accuracy
mean(main_df$congAcc, na.rm = TRUE)
sd(main_df$congAcc, na.rm = TRUE)
# normality test
shapiro.test(main_df$congAcc)


mean(main_df$incongAcc, na.rm = TRUE)
sd(main_df$incongAcc, na.rm = TRUE)

shapiro.test(main_df$incongAcc)
median(main_df$incongAcc, na.rm = TRUE)
IQR(main_df$incongAcc, na.rm = TRUE)
median(main_df$congAcc, na.rm = TRUE)
IQR(main_df$congAcc, na.rm = TRUE)

# as they are not normal, we perform non-parametric Wilcoxon test instead of t-test
wil_acc <- wilcox.test(main_df$congAcc, main_df$incongAcc, alternative = 'greater', paired = TRUE, na.action = na.omit)
Z_acc <- qnorm(wil_acc$p.value/2) # z-score
r_acc <- abs(Z_acc)/sqrt(32) # r (effect size) However, I reported Cohen's d in the paper. # formulas are from https://stats.stackexchange.com/questions/330129/how-to-get-the-z-score-in-wilcox-test-in-r#:~:text=How%20can%20i%20get%20the,for%20wilcox%20test%20in%20R%3F&text=The%20R%20code%20never%20stores,to%20the%20equivalent%20z%2Dscore.
cohen.d(main_df$congAcc, main_df$incongAcc, paired=TRUE)


# RT (unit in seconds)
mean(main_df$congCorr_meanRT, na.rm = TRUE)
sd(main_df$congCorr_meanRT, na.rm = TRUE)
shapiro.test(main_df$congCorr_meanRT)
median(main_df$congCorr_meanRT, na.rm = TRUE)
IQR(main_df$congCorr_meanRT, na.rm = TRUE)

mean(main_df$incongCorr_meanRT, na.rm = TRUE)
sd(main_df$incongCorr_meanRT, na.rm = TRUE)
shapiro.test(main_df$incongCorr_meanRT)
median(main_df$incongCorr_meanRT, na.rm = TRUE)
IQR(main_df$incongCorr_meanRT, na.rm = TRUE)

mean(main_df$congErr_meanRT, na.rm = TRUE)
sd(main_df$congErr_meanRT, na.rm = TRUE)
shapiro.test(main_df$congErr_meanRT)


mean(main_df$incongErr_meanRT, na.rm = TRUE)
sd(main_df$incongErr_meanRT, na.rm = TRUE)
shapiro.test(main_df$incongErr_meanRT)


wil_RT <- wilcox.test(main_df$congCorr_meanRT, main_df$incongCorr_meanRT, alternative = 'less', paired = TRUE, na.action = na.omit)
Z_RT <- qnorm(wil_RT$p.value/2)
r_RT <- abs(Z_RT)/sqrt(32)
cohen.d(main_df$congCorr_meanRT, main_df$incongCorr_meanRT,paired=TRUE)
report(wilcox.test(main_df$congCorr_meanRT, main_df$incongCorr_meanRT, alternative = 'less', paired = TRUE, na.action = na.omit))
##################################################
# Surprise memory task in mfe_c_face task
mean(main_df$overall_hitRate, na.rm = TRUE)
sd(main_df$overall_hitRate, na.rm = TRUE)

mean(main_df$error_hitRate, na.rm = TRUE)
sd(main_df$error_hitRate, na.rm = TRUE)
shapiro.test(main_df$error_hitRate) #

mean(main_df$correct_hitRate, na.rm = TRUE) #
sd(main_df$correct_hitRate, na.rm = TRUE) #
shapiro.test(main_df$correct_hitRate) #

mean(main_df$post_error_hitRate, na.rm = TRUE)
sd(main_df$post_error_hitRate, na.rm = TRUE)
shapiro.test(main_df$post_error_hitRate) #

mean(main_df$post_correct_hitRate, na.rm = TRUE) #
sd(main_df$post_correct_hitRate, na.rm = TRUE) #
shapiro.test(main_df$post_correct_hitRate) #


t.test(main_df$correct_hitRate, main_df$error_hitRate, alternative = 'less', paired = TRUE, na.action = na.omit) #
t.test(main_df$correct_hitRate, main_df$error_hitRate, paired = TRUE, na.action = na.omit) #

t.test(main_df$post_correct_hitRate, main_df$post_error_hitRate, paired = TRUE, na.action = na.omit)

report(t.test(main_df$correct_hitRate, main_df$error_hitRate, alternative = 'less', paired = TRUE, na.action = na.omit))
cohen.d(main_df$correct_hitRate, main_df$error_hitRate, paired=TRUE)

cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$overall_hitRate, method = 'pearson', na.action = na.omit)
cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$flankEff_meanACC, method = 'pearson', na.action = na.omit)
cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$incongAcc, method = 'pearson', na.action = na.omit)


##################################################
# Hit Rate correlation with SCAARED social

lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$hitRate_error_minus_correct, method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=error_hitRate)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=correct_hitRate)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$hitRate_post_error_minus_correct, method = 'pearson', na.action = na.omit)


# scaared total
lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdTotal_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdTotal_s1_r1_e1, main_df$hitRate_error_minus_correct, method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdTotal_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED total anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

ggplot(main_df, aes(x=scaared_b_scrdTotal_s1_r1_e1, y=error_hitRate)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED total anxiety score", y = "Error Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

ggplot(main_df, aes(x=scaared_b_scrdTotal_s1_r1_e1, y=correct_hitRate)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED total anxiety score", y = "Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


# Hit Rate correlation with SCAARED GA

lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdGA_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdGA_s1_r1_e1, main_df$hitRate_error_minus_correct, method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdGA_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED General anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


cor.test(main_df$scaared_b_scrdGA_s1_r1_e1, main_df$hitRate_post_error_minus_correct, method = 'pearson', na.action = na.omit)



current_pep_HR_reg <- lm(hitRate_error_minus_correct ~ epepq15_scrdTotal_s1_r1_e1*scaared_b_scrdSoc_s1_r1_e1 , data = main_df)
summary(current_pep_HR_reg)
# plot
plot_model(current_pep_HR_reg, type = "eff", terms = c("epepq15_scrdTotal_s1_r1_e1","scaared_b_scrdSoc_s1_r1_e1"))




current_pep_HR_reg <- lm(hitRate_error_minus_correct ~ epepq15_scrdTotal_s1_r1_e1*scaared_b_scrdGA_s1_r1_e1 , data = main_df)
summary(current_pep_HR_reg)
# plot


########################## POST
post_pep_HR_reg <- lm(hitRate_post_error_minus_correct ~ epepq15_scrdTotal_s1_r1_e1*scaared_b_scrdSoc_s1_r1_e1 , data = main_df)
summary(post_pep_HR_reg)
# plot
plot_model(post_pep_HR_reg, type = "eff", terms = c("epepq15_scrdTotal_s1_r1_e1","scaared_b_scrdSoc_s1_r1_e1"))


post_pep_HR_reg <- lm(hitRate_post_error_minus_correct ~ epepq15_scrdTotal_s1_r1_e1*scaared_b_scrdGA_s1_r1_e1 , data = main_df)
summary(post_pep_HR_reg)
# plot