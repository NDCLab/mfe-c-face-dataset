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

proje_wd <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-face-dataset"
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

proje_wd <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-object-dataset"
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



############################################ COMPUTE AGE FOR ALL PARTICIPANTS ##########################################

##### to compute age based on the month and year of birth.
### Loading RedCap questionnaire data for mfe-c-face
redcapDat_face <- read.csv(file = "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv")
### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object <- read.csv(file = "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv")

# Keep only the data collection date column
redcapDat_face_dataCollection_date <- redcapDat_face %>% dplyr::select(all_of(c("record_id", "demo_c_s1_r1_e1_timestamp")))
redcapDat_object_dataCollection_date <- redcapDat_object %>% dplyr::select(all_of(c("record_id", "demo_c_s1_r1_e1_timestamp")))

### Loading RedCap questionnaire data for mfe-c-face (this dataframe has only months and years of births)
redcapDat_face_mob_yob <- read.csv(file = "/Users/kihossei/Desktop/mfe-c-face_mob-yob_2024-12-13.csv")

# adding the date of data collection to this dataframe
redcapDat_face_mob_yob <- redcapDat_face_mob_yob %>%
  left_join(redcapDat_face_dataCollection_date, by = "record_id")

### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object_mob_yob <- read.csv(file = "/Users/kihossei/Desktop/mfe-c-object_mob-yob_2024-12-13.csv")

# adding the date of data collection to this dataframe
redcapDat_object_mob_yob <- redcapDat_object_mob_yob %>%
  left_join(redcapDat_object_dataCollection_date, by = "record_id")

redcapDat <- rbind(redcapDat_face_mob_yob, redcapDat_object_mob_yob)

# remove participants that did not participate although started the redcap
redcapDat_filtered <- redcapDat[redcapDat$record_id != "260009", ]
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260073", ] # fully collected data but not included data analysis as this person was beyond the 70 participants we aimed initially
redcapDat <- redcapDat_filtered


# Convert birth od date months and years (mob and yob) columns to a single Date object
library(lubridate)
redcapDat <- redcapDat %>%
  mutate(
    dob = make_date(demo_c_yob_s1_r1_e1, demo_c_mob_s1_r1_e1, 1) # Assume 1st of the month for DOB
  )
# Convert the datetime column to a POSIXct object
redcapDat$dataCollection_datetime <- ymd_hms(redcapDat$demo_c_s1_r1_e1_timestamp)

# Function to calculate age from birth year to data colllection date
redcapDat$age <- as.period(interval(start=redcapDat$dob, end=redcapDat$demo_c_s1_r1_e1_timestamp), unit = "years")

# Function to convert age string to decimal years
convert_age_to_decimal <- function(age_str) {
  # Extract years and months using regular expressions
  years <- as.numeric(gsub("y.*", "", age_str))
  months <- as.numeric(gsub(".* ([0-9]+)m.*", "\\1", age_str))

  # Calculate decimal years
  decimal_years <- years + (months / 12)

  return(decimal_years)
}
# Compute age in decimal
for(i in 1:nrow(redcapDat)){
  redcapDat$decimal_age[i] <- convert_age_to_decimal(redcapDat$age[i])
}

round(mean(redcapDat$decimal_age),2)
round(sd(redcapDat$decimal_age), 2)
############################################ END OF COMPUTING AGE FOR ALL PARTICIPANTS #################################

############################################ COMPUTE cronbach.alpha for SCAARED ########################################
# Compute cronbach.alpha for SCAARED
# Define file paths for your CSV files
redcap_mfe_c_face_scaaredSoc <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv"
redcap_mfe_c_object_scaaredSoc <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv"

# Define the columns to keep
selected_columns <- c("record_id",
                      "scaared_b_i3_s1_r1_e1",
                      "scaared_b_i10_s1_r1_e1",
                      "scaared_b_i27_s1_r1_e1",
                      "scaared_b_i34_s1_r1_e1",
                      "scaared_b_i41_s1_r1_e1",
                      "scaared_b_i42_s1_r1_e1",
                      "scaared_b_i43_s1_r1_e1")

# Function to load and select columns from a CSV file
load_and_select <- function(file_path, columns) {
  tryCatch({
    df <- read_csv(file_path)

    # Check if all specified columns exist in the dataframe
    if (!all(columns %in% names(df))) {
      missing_cols <- setdiff(columns, names(df))
      stop(paste("The following columns are missing in file:", file_path, paste(missing_cols, collapse = ", ")))
    }

    df_selected <- df %>% dplyr::select(all_of(columns))
    return(df_selected)
  }, error = function(e) {
    message(paste("Error processing file:", file_path))
    message(e)
    return(NULL) # Return NULL in case of an error
  })
}

# Load and select columns from both files
df1_selected <- load_and_select(redcap_mfe_c_face_scaaredSoc, selected_columns)
df2_selected <- load_and_select(redcap_mfe_c_object_scaaredSoc, selected_columns)

# Check if both dataframes were loaded successfully before binding
if (!is.null(df1_selected) && !is.null(df2_selected)) {
  # Bind the dataframes using bind_rows (from dplyr)
  combined_df <- bind_rows(df1_selected, df2_selected)
} else {
  message("One or both files could not be loaded. Please check the file paths and column names.")
}

combined_df <- filter(combined_df, record_id != 260009 & record_id != 260060 ) # these two participants had not completed the experiment. So, they had empty surveys.

new_combined_df <- combined_df[, -1]

install.packages("ltm")
library(ltm) # for cronbach.alpha
cronbach.alpha(combined_df, standardized = TRUE, na.rm = TRUE)

cronbach.alpha(new_combined_df, standardized = TRUE, na.rm = TRUE)
cronbach.alpha(new_combined_df, standardized = FALSE, na.rm = TRUE) #computes raw alpha value (raw is the value that we report in the paper.

############################################ END of COMPUTING cronbach.alpha for SCAARED ###############################

