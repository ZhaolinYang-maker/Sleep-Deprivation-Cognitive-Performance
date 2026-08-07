# Secondary Data Analysis Project1: Studying the Relationship between sleep deprivation and emotional regualtion and relationship between psychological states and cognitive performance

# Read the file from the local computer 
data <- read.csv("/Users/zhaolinyang/Desktop/Indpendent Project/Project 1st/Dataset.csv")

install.packages("qqplotr")
library(qqplotr)
install.packages("psych")
library(psych)

#=====================================================================================================================
# MODULE1: Dataset Overview and Descriptive Statistics
#=====================================================================================================================

# STEP1: Brief overall summary for the whole data
dim(data) # Quick check for the number of sample size (N = 60) and number of variables (p = 14)
names(data) # Labeling variable names. Especially useful for quick check for if there is any misspelling in code
summary(data) # Generate a brief descriptive summary of all variables

#Checking missing values in the whole data
colSums(is.na(data)) #Confirmation as described by the authors, there is no missing value in the dataset
sum(duplicated(data)) #Checking for any duplicated rows in the data 

# STEP2 Focusing on variables of interest with description and summary
# Seven variables of interest included in the two aims. Mainly focusing on the sample statistics of the 7 variables
variables <- c("Sleep_Hours", 
               "Sleep_Quality_Score", 
               "Emotion_Regulation_Score",
               "Stress_Level",
               "Stroop_Task_Reaction_Time",
               "N_Back_Accuracy",
               "PVT_Reaction_Time")

mean_values <- sapply(data[variables], mean) # Computation of each variable's mean
variance_values <- sapply(data[variables], var) # Computation of each variable's variance in sample
SD_values <- sapply(data[variables], sd) #Computation of each variable's standard deviation in sample
descriptive_statistics_table <-cbind(Mean = mean_values,
                                     variance = variance_values,
                                     SD = SD_values)
round(descriptive_statistics_table,2) #Combine the values to be a table for summary and comparison

#=====================================================================================================================
# MODULE2: Necessary Data Preparation prior to formal data analysis
#=====================================================================================================================

# STEP1: Generation of mean-centered variables (sleep-hours, sleep quality and emotional regulation score for AIM2)
data$c_sleep_hours <- data$Sleep_Hours - mean(data$Sleep_Hours) # Mean-centering of sleep duration in a newly created column
data$c_sleep_quality <- data$Sleep_Quality_Score - mean(data$Sleep_Quality_Score) # Mean-centering of sleep quality in a newly created column
data$c_emotion_regulation <- data$Emotion_Regulation_Score - mean(data$Emotion_Regulation_Score) # Mean-centering of emotion regulation in a newly created column

#STEP2 Converting continous stress level to categorical variable with three groups based on criteria of level of stress (Perceived Stress Scale PSS-10)
# PSS-10 evaluates stress levels on a 0 to 40 scale. It classifies low stress group (0-10), moderate stress (14-26) and high stress group (27-40)
data$Moderate_stress <- ifelse(14 <= data$Stress_Level
                             &data$Stress_Level <= 26, 
                             1, 0)
data$High_stress <- ifelse(27 <= data$Stress_Level&
                                data$Stress_Level <= 40, 
                              1, 0)
data$Stress_Group <- ifelse(data$Moderate_stress == 1, #Classification of different stress groups
                               "Moderate Stress",
                               ifelse(data$High_stress == 1,
                                      "High Stress",
                                      "Low Stress"
                                      )
                            ) 
data$Stress_Group <- factor(data$Stress_Group,
                            levels = c("Low Stress", "Moderate Stress", "High Stress")
                            ) #Low stress group (stress level: 0-13) is baseline group of reference for later ANCOVA

#=====================================================================================================================
# MODULE3: Reusable Assumption Diagnostics Functions
#=====================================================================================================================
# For valid conventional sampling-distribution-based inference, certain assumptions ought to be resonably satisfied for each fitted regression model.
# In other words, regression assumptions are examined for each fitted model rather than for the dataset as a whole.

#This module defines reusable diagnostic functions so that the same checks can be consistently and conveniently applied to each model in the linear analysis
#As discussed in project overview, particular attention will be given to (1) approximate normality of errors; (2) Linearity of relationship between predictors and outcome (3)homoscedasticity (homogenity of variance of outcome variable)
# (4) influential observations (e.g., outliers in sample) and (5) multi-collinearity among predictors incorporated in multiple linear regression 

# CHECK 1: Diagnostics of Approximate Normality of Errors
# Because the OLS coefficient estimator is a linear function of the model errors, approximate normality of errors supports an approximately normal sampling distribution of coefficient estimates and therefore conventional t- and F-based inference.
# QQ plot and the Shapiro-Wilk test (S-W test) will be used for checking the assumption

Residuals_Normality <- function(model){
  Residuals_Model <- residuals(model) # Inset any linear regression model, the functions automatically compute its residuals for later calculations if applicable
  par(mfrow = c(1,2))
       hist(Residuals_Model,
       breaks = 10,
       freq = FALSE,
       col = "black",
       border = "white",
       main = "Histogram of Regression Residuals",
       xlab = "Residuals") # Constructing histogram describing the residual distributions for visualization of distribution of redisuals in the existing sample
  
       qqnorm(Residuals_Model,
         main = "Q-Q Plot of Regression Residuals",
         ) # Drawing QQ plot for visually comparison of consistency between observed values of residuals and expected values of residuals given normality of errors
  
       qqline(Residuals_Model,
         col = "red"
         )

       print(shapiro.test(Residuals_Model)) #S-W test for statistically testing normality of residuals where H0 = no violation of normality of errors
} # Expectedly, once model is constructed and inserted into the fucntion, hitogram of residuals, Q-Q plot of residuals and result of S-W test should be delivered. 
#Based on the result, deicding whether the assumption of normality of errors is violated or not visually and statistically. Remedy is then required if there is violation



