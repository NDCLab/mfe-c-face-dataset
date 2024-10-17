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


# Loading mfe_c_face data

proje_wd <- "/Users/kihossei/Google Drive/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-face-dataset"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "derivatives", "psychopy", "stat_output", sep ="/", collapse = NULL) # input data directory

face_df <-  read.csv(file = paste(processed_file_input, "processed_data_mfe_c_face_Proj_v1.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))

# Keep the columns that we will need
selected_columns <- face_df[, c("participant_id", "flankEff_meanACC", "incongAcc", "error_hitRate", "correct_hitRate", "overall_hitRate", "hitRate_error_minus_correct", "post_error_hitRate", "post_correct_hitRate", "hitRate_post_error_minus_correct", "scaared_b_scrdSoc_s1_r1_e1", "scaared_b_scrdGA_s1_r1_e1", "scaared_b_scrdTotal_s1_r1_e1", "bfne_b_scrdTotal_s1_r1_e1", "epepq15_scrdTotal_s1_r1_e1", "phq8_scrdTotal_s1_r1_e1", "stai5_scrdS_s1_r1_e1", "stai5_scrdS_s1_r1_e2", "stai5_scrdS_diff")]
face_df <- selected_columns

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
new_face_df <- face_df
new_face_df[-c(1, ncol(new_face_df))] <- apply(face_df[-c(1, ncol(face_df))], 2, remove_outliers)
face_df <- new_face_df

face_df$group <- 'face' # adding a column that specifies the group (will be needed below)

#Working directory should be the Psychopy experiment directory.

proje_wd <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-object-dataset"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "derivatives", "psychopy", "stat_output", sep ="/", collapse = NULL) # input data directory

object_df <-  read.csv(file = paste(processed_file_input, "processed_data_mfe_c_object_Proj_v1.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))
# Keep the columns that we will need
selected_columns <- object_df[, c("participant_id", "flankEff_meanACC", "incongAcc", "error_hitRate", "correct_hitRate", "overall_hitRate", "hitRate_error_minus_correct", "post_error_hitRate", "post_correct_hitRate", "hitRate_post_error_minus_correct", "scaared_b_scrdSoc_s1_r1_e1", "scaared_b_scrdGA_s1_r1_e1", "scaared_b_scrdTotal_s1_r1_e1", "bfne_b_scrdTotal_s1_r1_e1", "epepq15_scrdTotal_s1_r1_e1", "phq8_scrdTotal_s1_r1_e1", "stai5_scrdS_s1_r1_e1", "stai5_scrdS_s1_r1_e2", "stai5_scrdS_diff")]
object_df <- selected_columns

# apply this outlier removing function to all the columns in the dataframe except for participant ID column.
new_object_df <- object_df
new_object_df[-c(1, ncol(new_object_df))] <- apply(object_df[-c(1, ncol(object_df))], 2, remove_outliers)
object_df <- new_object_df

object_df$group <- 'object' # adding a column that specifies the group (will be needed below)

# binding these two dataframes two create a single main_df
main_df <- rbind(face_df, object_df)



# Regression
main_df$group <- as.factor(main_df$group)

fit <- lm(hitRate_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1*group, data = main_df)
summary(fit)
# Plotting
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_error_minus_correct, group = group)) + geom_point(size = 4) + geom_smooth(method="lm", aes(color = group)) +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

# Simple slopes
library(reghelper)
simple_slopes(fit)
############ POST
fit2 <- lm(hitRate_post_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1*group, data = main_df)
summary(fit2)
# Plotting
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_post_error_minus_correct, group = group)) + geom_point(size = 4) + geom_smooth(method="lm", aes(color = group)) +
  labs(x = "SCAARED social anxiety score", y = "Post Error vs. Post Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))

# Simple slopes
simple_slopes(fit2)

fit3 <- lm(hitRate_post_error_minus_correct ~ group, data = main_df)
summary(fit3)
ggplot(main_df, aes(x = group, y = hitRate_post_error_minus_correct)) +
  geom_boxplot() +
  theme_minimal()


fit4 <- lm(hitRate_error_minus_correct ~ group, data = main_df)
summary(fit4)
ggplot(main_df, aes(x = group, y = hitRate_error_minus_correct)) +
  geom_boxplot() +
  theme_minimal()

fit4 <- lm(overall_hitRate ~ group, data = main_df)
summary(fit4)
ggplot(main_df, aes(x = group, y = overall_hitRate)) +
  geom_boxplot() +
  theme_minimal()






















#### SAMPLE CODE BELOW:

t.test(main_df$correct_hitRate, main_df$error_hitRate, alternative = 'less', paired = TRUE, na.action = na.omit) #
t.test(main_df$correct_hitRate, main_df$error_hitRate, paired = TRUE, na.action = na.omit) #

report(t.test(main_df$correct_hitRate, main_df$error_hitRate, alternative = 'less', paired = TRUE, na.action = na.omit))
cohen.d(main_df$correct_hitRate, main_df$error_hitRate, paired=TRUE)

##################################################
# Hit Rate correlation with SCAARED social

lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdSoc_s1_r1_e1, main_df$hitRate_error_minus_correct, alternative = 'greater', method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))



# Hit Rate correlation with SCAARED GA

lm_for_cor_fit_line <- lm(hitRate_error_minus_correct ~ scaared_b_scrdGA_s1_r1_e1, main_df)
summary(lm_for_cor_fit_line)
cor.test(main_df$scaared_b_scrdGA_s1_r1_e1, main_df$hitRate_error_minus_correct, alternative = 'greater', method = 'pearson', na.action = na.omit)
ggplot(main_df, aes(x=scaared_b_scrdGA_s1_r1_e1, y=hitRate_error_minus_correct)) + geom_point(size = 4) + geom_smooth(method="lm") +
  labs(x = "SCAARED General anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))





current_pep_HR_reg <- lm(hitRate_error_minus_correct ~ epepq15_scrdTotal_s1_r1_e1*scaared_b_scrdSoc_s1_r1_e1 , data = main_df)
summary(current_pep_HR_reg)
# plot
plot_model(current_pep_HR_reg, type = "eff", terms = c("epepq15_scrdTotal_s1_r1_e1","scaared_b_scrdSoc_s1_r1_e1"))




current_pep_HR_reg <- lm(hitRate_error_minus_correct ~ epepq15_scrdTotal_s1_r1_e1*scaared_b_scrdGA_s1_r1_e1 , data = main_df)
summary(current_pep_HR_reg)
# plot