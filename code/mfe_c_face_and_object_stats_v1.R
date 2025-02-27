# This script will run stats on mini_mfe data.
# Author: Kianoosh Hosseini at NDCLab @FIU (https://Kianoosh.info; https://NDClab.com)
# Last Update: 2025-01-21 (YYYY-MM-DD)

library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(psycho)
library(car) # for functions like Anova(); Anova() is different from anova()
library(lme4)
library(ggplot2)
library(emmeans)
library(report)
library(sjPlot)
library(effsize)
library(effectsize)

# Loading mfe_c_face data

proje_wd <- "/Users/kihossei/Library/CloudStorage/GoogleDrive-hosseinikianoosh@gmail.com/My Drive/My Digital Life/Professional/Github_Repos/mfe-c-face-dataset"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "derivatives", "psychopy", "stat_output", sep ="/", collapse = NULL) # input data directory

face_df <-  read.csv(file = paste(processed_file_input, "processed_data_mfe_c_face_Proj_v1.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))

# Keep the columns that we will need
selected_columns <- face_df[, c("participant_id", "flankEff_meanACC", "congAcc", "incongAcc", "num_of_correct_cong_trials", "num_of_error_cong_trials", "num_of_correct_incong_trials", "num_of_error_incong_trials", "congCorr_meanRT", "incongCorr_meanRT", "error_hitRate", "correct_hitRate", "overall_hitRate", "hitRate_error_minus_correct", "scaared_b_scrdSoc_s1_r1_e1", "scaared_b_scrdGA_s1_r1_e1", "scaared_b_scrdTotal_s1_r1_e1", "bfne_b_scrdTotal_s1_r1_e1", "epepq15_scrdTotal_s1_r1_e1", "phq8_scrdTotal_s1_r1_e1", "stai5_scrdS_s1_r1_e1", "stai5_scrdS_s1_r1_e2", "stai5_scrdS_diff")]
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
selected_columns <- object_df[, c("participant_id", "flankEff_meanACC", "congAcc", "incongAcc", "num_of_correct_cong_trials", "num_of_error_cong_trials", "num_of_correct_incong_trials", "num_of_error_incong_trials", "congCorr_meanRT", "incongCorr_meanRT", "error_hitRate", "correct_hitRate", "overall_hitRate", "hitRate_error_minus_correct", "scaared_b_scrdSoc_s1_r1_e1", "scaared_b_scrdGA_s1_r1_e1", "scaared_b_scrdTotal_s1_r1_e1", "bfne_b_scrdTotal_s1_r1_e1", "epepq15_scrdTotal_s1_r1_e1", "phq8_scrdTotal_s1_r1_e1", "stai5_scrdS_s1_r1_e1", "stai5_scrdS_s1_r1_e2", "stai5_scrdS_diff")]
object_df <- selected_columns

# apply this outlier removing function to all the columns in the dataframe except for participant ID column.
new_object_df <- object_df
new_object_df[-c(1, ncol(new_object_df))] <- apply(object_df[-c(1, ncol(object_df))], 2, remove_outliers)
object_df <- new_object_df

object_df$group <- 'object' # adding a column that specifies the group (will be needed below)

# binding these two dataframes two create a single main_df
main_df <- rbind(face_df, object_df)


###################################### accuracy ~ congruency + group MODEL #############################################
# Reshape to Long Format
long_main_df1 <- main_df %>%
  pivot_longer(
    cols = c(congAcc, incongAcc),  # Columns to pivot
    names_to = "flanker_congruency",       # Name of the new column for variable names
    values_to = "accuracy",        # Name of the new column for values
    names_pattern = "(.*)Acc" # Extract subject name from column name
  )


# Converting categorical variables (group and flanker accuracy) to factors
long_main_df1$group <- as.factor(long_main_df1$group)
long_main_df1$flanker_congruency <- as.factor(long_main_df1$flanker_congruency)


#
# Load the lmerTest package
library(lmerTest)

# I ran the following mixed effects model. However, the model violated normality of residuals and homoscedasticity
# assumptions according to visual inspections.

# Because of this I am gonna run GLMM with a beta distribbution because my outcome (accuracy) is positive and between 0 and 1 and is a proportion.

############## not used for the paper because of assumption violations ##########
# Fit the mixed-effects model with random intercepts
model1 <- lmer(accuracy ~ flanker_congruency * group + (1 | participant_id), data = long_main_df1)

# Display the model summary
summary(model1)
# confidence intervals
confint(model1)

# check_heteroscedasticity() using performance library
library(performance)
check_heteroscedasticity(model1)

qqnorm(resid(model1))
qqline(resid(model1))

hist(resid(model1))
hist(long_main_df1$accuracy)
# Get the fitted values
fitted_values <- fitted(model1)

# Get the residuals
residuals <- residuals(model1)

# Create the plot
plot(fitted_values, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Transform my dependent variable (accuracy) to better meet normality assumptions:
# Log transformation (if no zeros)
long_main_df1$log_accuracy <- log(long_main_df1$accuracy)
model_log <- lmer(log_accuracy ~ flanker_congruency * group + (1 | participant_id), data = long_main_df1)

qqnorm(resid(model_log))
qqline(resid(model_log))

# Square root transformation
long_main_df1$sqrt_accuracy <- sqrt(long_main_df1$accuracy)
model_sqrt <- lmer(sqrt_accuracy ~ flanker_congruency * group + (1 | participant_id), data = long_main_df1)

qqnorm(resid(model_sqrt))
qqline(resid(model_sqrt))

# Logit transformation (if accuracy is proportion between 0-1)
# as accuracy can have values of for some of the subjects. I perform some adjustments.
long_main_df1$adjusted_accuracy <- ifelse(long_main_df1$accuracy == 1.0, 0.999999,
                                 ifelse(long_main_df1$accuracy == 0.0, 0.000001,
                                        long_main_df1$accuracy)) # for beta distribution, the values cannot be 0s and 1s. So, I convert them to avoid error.

long_main_df1$logit_accuracy <- car::logit(long_main_df1$adjusted_accuracy)
model_logit <- lmer(logit_accuracy ~ flanker_congruency * group + (1 | participant_id), data = long_main_df1)

qqnorm(resid(model_sqrt))
qqline(resid(model_sqrt))

## Option 1: Running GLMM with a binomial distribution (As I know the number of trials that have been used to compute accuracy for incongruent and
# congruent trials, I can use this method. The number of trials is the half of flanker trials which 384/2 = 192).
# Binomial requires count data:
# # (successes + failures), not pre-computed proportions. The model needs to know:
# # # - Number of trials (n)
# # # - Number of successes (k)

# In the GLMM, I need to use a matrix of successes and failures as the output variable.
# I can use accuracy rate and then specify weights in glmer which is total number of trials used to compute the acc rate. However,
# this can cause a problem when it multiplies the number of trials with acc rate the value will not be integer.
# So, I am doing it manually to avoid this issue.

long_main_df1$total_trials <- 192
long_main_df1$successes <- round(long_main_df1$accuracy * long_main_df1$total_trials)

# GLMM with binomial family and weights
# instead of accuracy rate and then a
model_glmm1 <- glmer(
  cbind(successes, 192 - successes) ~ flanker_congruency * group + (1 | participant_id),
  family = binomial,
  data = long_main_df1
)
summary(model_glmm1)

qqnorm(resid(model_glmm1))
qqline(resid(model_glmm1))

qqnorm(ranef(model_glmm1)$participant_id[,1])
qqline(ranef(model_glmm1)$participant_id[,1])

check_overdispersion(model_glmm1)
crPlots(model_glmm1)

all_fit_results <- allFit(model_glmm1)
summary(all_fit_results)

# an example of the code with weights specified below:
# model_glmer_weights <- glmer(accuracy ~ flanker_congruency * group + (1 | participant_id), data = long_main_df1, family = binomial, weights = rep(192, nrow(long_main_df1))) # 192 trials for all rows
# Note: weights argument tells glmer that each observation (each accuracy rate) is based on 192 trials.


## Option 2: Running GLMM with a beta distribution #####################
# This option is used:
# - assuming only the rates are available without the counts.
# - Using a binomial model with rates alone (without counts) can lead to incorrect variance estimation.
# - Beta regression handles the continuous proportion data more naturally and doesn't require knowledge of the number of trials.

# glmmTMB package supports beta ditribution. So, I use this library.
library(glmmTMB)
long_main_df1$accuracy <- ifelse(long_main_df1$accuracy == 1.0, 0.999999,
                                 ifelse(long_main_df1$accuracy == 0.0, 0.000001,
                                        long_main_df1$accuracy)) # for beta distribution, the values cannot be 0s and 1s. So, I convert them to avoid error.

model1 <- glmmTMB(
  accuracy ~ flanker_congruency * group + (1 | participant_id),
  data = long_main_df1,
  family = beta_family(link = "logit")
)

# Display the model summary
summary(model1)

#estimates are in log odds. So. I need to convert them to odds to have an easier report and interpretation.
# Convert log odds to odds using exp(log_odds)
exp(-1.98652)
# Convert odds to proportion
odds <- 0.137172
proportion <- odds / (1 + odds)

# The shorter way to convert log odds to proportion is:
#congruency
#beta
round(plogis(-1.98652467),3) # (-) 0.121

# confidence intervals
round(confint(model1),3)

# To Convert a logit back to a proportion
# to get the estimated beta value in proportion and its confidence interval values

#congruency
#beta
round(plogis(-1.98652467),3) # - 0.121

# Get the table with standardized coefficients for GLMM
tab_model(
  model1,
  show.std = "refit",       # Standardize using the refit method
  show.se = TRUE,          # Show standard errors
  show.stat = TRUE,         # Show z-statistic
  show.p = TRUE,           # Show p-values
  show.ci = 0.95,          # Show 95% confidence intervals (default)
  transform = NULL,   # Important for Beta regression: Show coefficients on the logit scale
  dv.labels = "Beta Regression (logit link)", # Label for the dependent variable and model type
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

# Calculate partial eta squared
aov <- Anova(model1, type=2, test="F")
eta_squared(aov, alternative="two.sided")


ggplot(long_main_df1, aes(x = flanker_congruency, y = accuracy)) +
  geom_boxplot() +
  labs(title = "Accuracy by Category",
       x = "Categorical Predictor",
       y = "Accuracy")

# Power analysis for this model using mixedpower library
# load library
library(mixedpower)
# INFORMATION ABOUT MODEL USED FOR SIMULATION
mp_model <- model1 # which model do we want to simulate power for?
mp_data <- long_main_df1 # data used to fit the model
mp_fixed_effects <- c("flanker_congruency", "group") # all fixed effects specified in FLPmodel
mp_simvar <- "participant_id" # which random effect do we want to vary in the simulation?

# SIMULATION PARAMETERS
mp_steps <- c(60, 80, 100, 120) # which sample sizes do we want to look at?
critical_value <- 1.96 # which t/z value do we want to use to test for significance?
n_sim <- 100 # how many single simulations should be used to estimate power?

# RUN SIMULATION
power_FLP <- mixedpower(model = mp_model, data = mp_data,
                        fixed_effects = mp_fixed_effects,
                        simvar = mp_simvar, steps = mp_steps,
                        critical_value = critical_value, n_sim = n_sim)


library(simr)
# -------------------------------------------- #
clean_data <- na.omit(long_main_df1)
mp_model <- lmer(accuracy ~ flanker_congruency * group + (1 | participant_id), data = clean_data)
# POWER ANALYSIS WITH SIMR
powerC <- powerCurve(fit = mp_model, test = fixed(mp_fixed_effects), along = "participant_id",
                     breaks = c(60, 80, 100, 120))
# let's have a look at the results
print(powerC)
########################################################################################################################




###################################### RT ~ congruency + group MODEL #############################################

# Reshape to Long Format
long_main_df2 <- main_df %>%
  pivot_longer(
    cols = c(congCorr_meanRT, incongCorr_meanRT),  # Columns to pivot
    names_to = "flanker_congruency",       # Name of the new column for variable names
    values_to = "RT",        # Name of the new column for values
    names_pattern = "(.*)Corr_meanRT" # Extract subject name from column name
  )


# Converting categorical variables (group and flanker accuracy) to factors
long_main_df2$group <- as.factor(long_main_df2$group)
long_main_df2$flanker_congruency <- as.factor(long_main_df2$flanker_congruency)

#
# Load the lmerTest package
library(lmerTest)

# Fit the mixed-effects model with random intercepts
model2 <- lmer(RT ~ flanker_congruency * group + (1 | participant_id), data = long_main_df2)

# Display the model summary
summary(model2)
# confidence intervals
round(confint(model2),3)

# Get the table with standardized coefficients for LMM
tab_model(
  model2,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

# Calculate partial eta squared
aov2 <- Anova(model2, type=2, test="F")
eta_squared(aov2, alternative="two.sided")

# Standardize the model using different methods
library(sjstats)
std_refit <- standardize(model2, method = "refit")


# check_heteroscedasticity() using performance library
library(performance)
check_heteroscedasticity(model2)

qqnorm(resid(model2))
qqline(resid(model2))

hist(resid(model2))
hist(long_main_df2$RT)
# Get the fitted values
fitted_values <- fitted(model2)

# Get the residuals
residuals <- residuals(model2)

# Create the plot
plot(fitted_values, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Visual inspections show that this model is fine in terms of homosscedasticity and normality of residuals.

########################################################################################################################

######################################## hit rate ~ accuracy + group MODEL #############################################
# Reshape to Long Format
long_main_df3 <- main_df %>%
  pivot_longer(
    cols = c(error_hitRate, correct_hitRate),  # Columns to pivot
    names_to = "flanker_accuracy",       # Name of the new column for variable names
    values_to = "hitRate",        # Name of the new column for values
    names_pattern = "(.*)_hitRate" # Extract subject name from column name
  )


# Converting categorical variables (group and flanker accuracy) to factors
long_main_df3$group <- as.factor(long_main_df3$group)
long_main_df3$flanker_accuracy <- as.factor(long_main_df3$flanker_accuracy)

#
# Load the lmerTest package
library(lmerTest)

# Fit the mixed-effects model with random intercepts
model3 <- lmer(hitRate ~ flanker_accuracy * group + (1 | participant_id), data = long_main_df3)

# Display the model summary
summary(model3)
# confidence intervals
round(confint(model3),3)

# Get the table with standardized coefficients for LMM
tab_model(
  model3,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

# Calculate partial eta squared
aov3 <- Anova(model3, type=2, test="F")
eta_squared(aov3, alternative="two.sided")

# Standardize the model using different methods
library(sjstats)

standardize(model3, method = "refit")

# Visual inspections show that this model is fine in terms of homosscedasticity and normality of residuals.
library(simr)
power <- powerSim(model3,nsim = 200)
power <- powerSim(model3, test = fixed("flanker_accuracy:group"), nsim = 200)
########################################################################################################################


######################################## Memory Bias for error events ~ scaared SA*group MODEL #############################################

# Regression
main_df$group <- as.factor(main_df$group)

# The following model satisfies the linear regression normality, homoscedasticity, linearity, and independence of errors assumptions.

fit <- lm(hitRate_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1*group, data = main_df)
summary(fit)
confint(fit, level=0.95)

tab_model(
  fit,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)
# Plotting
ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=hitRate_error_minus_correct, group = group)) + geom_point(size = 4) + geom_smooth(method="lm", aes(color = group)) +
  labs(x = "SCAARED social anxiety score", y = "Error vs. Correct Hit rate") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text = element_text(size = 15)) + theme(text = element_text(size = 18))


# 1. Create Standardized Variables
main_df$scaared_b_scrdSoc_s1_r1_e1_std <- scale(main_df$scaared_b_scrdSoc_s1_r1_e1)
main_df$hitRate_error_minus_correct_std <- scale(main_df$hitRate_error_minus_correct)

fit2 <- lm(hitRate_error_minus_correct_std ~ scaared_b_scrdSoc_s1_r1_e1_std*group, data = main_df)
summary(fit2)

# Simple slopes

library(reghelper)
simple_slopes(fit)
simple_slopes(fit2)
emtrends(fit, ~ group, var="scaared_b_scrdSoc_s1_r1_e1") #gives confidence intervals
emtrends(fit2, ~ group, var="scaared_b_scrdSoc_s1_r1_e1_std") #gives confidence intervals

library(sjstats)

standardize(fit, method = "refit")
########## assumptions check for the regression model above

# 1. Linearity
# Create diagnostic dataframe
diagnostics <- data.frame(
  fitted = fitted(fit),
  residuals = residuals(fit),
  standardized = rstandard(fit)
)

# Residual vs Fitted plot for linearity and homoscedasticity
ggplot(diagnostics, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")

# 2. Independence of Errors
# Durbin-Watson test
library(car)
durbinWatsonTest(fit)

# 3. Homoscedasticity
check_heteroscedasticity(fit)

# 4. Normality of Errors
# Histogram of residuals
hist(resid(fit))

# Q-Q plot of residuals
plot(fit, which = 2)  # Normal Q-Q

# Shapiro-Wilk test for normality
shapiro.test(resid(fit))

# Inspections show that this model satisfies the assumptions.
######### END of assumptions check






############################################ COMPUTE AGE FOR ALL PARTICIPANTS Before exclusion #########################

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

############################################ COMPUTE AGE FOR ALL PARTICIPANTS After exclusion ##########################
# Select only the 'ID' and 'decimal_age' columns from dataframe redcapDat
redcapDat_subset <- redcapDat[, c("record_id", "decimal_age")]

# Rename the column 'record_id' to 'participant_id' in dataframe 'redcapDat_subset'
redcapDat_subset <- redcapDat_subset %>% rename(participant_id = record_id)

# Merge the subset of redcapDat_subset with dataframe main_df by the 'participant_id' column
main_df <- merge(main_df, redcapDat_subset, by = "participant_id", all.x = TRUE)

# Compute age mean and SD for the face group
face_temp_df <- filter(main_df, group == "face" )

round(mean(face_temp_df$decimal_age),2)
round(sd(face_temp_df$decimal_age), 2)

# Compute age mean and SD for the object group
object_temp_df <- filter(main_df, group == "object" )

round(mean(object_temp_df$decimal_age),2)
round(sd(object_temp_df$decimal_age), 2)
############################################ END OF COMPUTING AGE For the remaining PARTICIPANTS #######################

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
combined_df <- filter(combined_df, record_id != 260073 ) # This participant was collected after reaching 140 sample size. So, we are not analyzing its data.


# remove the ID column
new_combined_df <- combined_df[, -1]

install.packages("ltm")
library(ltm) # for cronbach.alpha

cronbach.alpha(new_combined_df, standardized = FALSE, na.rm = TRUE) #computes raw alpha value (raw is the value that we report in the paper.

############################################ END of COMPUTING cronbach.alpha for SCAARED ###############################


############################################ COMPUTING the stats of the number of flanker trials removed based on RT ###

# For face
proje_wd <- "/Users/kihossei/Google Drive/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-face-dataset"
flanker_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
flanker_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(flanker_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_face_flankerDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    flanker_proc_dat_files_list <- c(flanker_proc_dat_files_list, temp_list)
  }
}

# Create an empty dataframe to store the results
rt_df <- data.frame(participant_id = character(),
                      num_fast_rts = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each csv file
for (file in flanker_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(flanker_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.15 seconds
  num_fast_rts <- sum(tempDat$current_trial_rt < 0.15, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                       num_fast_rts = num_fast_rts))
}

# Adding the object data
#
proje_wd <- "/Users/kihossei/Google Drive/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-object-dataset"
flanker_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
flanker_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(flanker_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_object_flankerDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    flanker_proc_dat_files_list <- c(flanker_proc_dat_files_list, temp_list)
  }
}

# Loop through each csv file
for (file in flanker_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(flanker_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.15 seconds
  num_fast_rts <- sum(tempDat$current_trial_rt < 0.15, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                   num_fast_rts = num_fast_rts))
}

round(mean(rt_df$num_fast_rts), 2)
round(sd(rt_df$num_fast_rts), 2)

########################################################################################################################



############################################ COMPUTING the stats of the number of surprsie trials removed based on RT ###

# For face
proje_wd <- "/Users/kihossei/Google Drive/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-face-dataset"
surprsie_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
surprsie_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(surprsie_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_face_surpriseDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    surprsie_proc_dat_files_list <- c(surprsie_proc_dat_files_list, temp_list)
  }
}

# Create an empty dataframe to store the results
rt_df <- data.frame(participant_id = character(),
                    num_fast_rts = numeric(),
                    stringsAsFactors = FALSE)

# Loop through each csv file
for (file in surprsie_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(surprsie_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.20 seconds
  num_fast_rts <- sum(tempDat$memory_surp_rt < 0.20, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                   num_fast_rts = num_fast_rts))
}

# Adding the object data
#
proje_wd <- "/Users/kihossei/Google Drive/My Drive/My Digital Life/Professional/GitHub_Repos/mfe-c-object-dataset"
surprsie_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
surprsie_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(surprsie_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_object_surpriseDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    surprsie_proc_dat_files_list <- c(surprsie_proc_dat_files_list, temp_list)
  }
}

# Loop through each csv file
for (file in surprsie_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(surprsie_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.20 seconds
  num_fast_rts <- sum(tempDat$memory_surp_rt < 0.20, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                   num_fast_rts = num_fast_rts))
}

round(mean(rt_df$num_fast_rts), 2)
round(sd(rt_df$num_fast_rts), 2)

########################################################################################################################


