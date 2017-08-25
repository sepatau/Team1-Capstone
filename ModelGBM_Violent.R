###############################################################################
# Shant Hambarsoumian
# PREDICT 498.56 Capstone Team 1
# Tree Based Regression on Violent Crime Rate
###############################################################################

par(mfrow=c(1,1))

set.seed(1234)

library(dplyr)
library(ggplot2)
library(data.table)
library(gbm)

###############################################################################
# Import data sets
###############################################################################

data <- fread("data_imp_selectvar_violent.csv")
str(data)
data_sca <- fread("data_sca_imp_selectvar_violent.csv")
str(data_sca)

# Feature Generation or Variable Changes
data$FUNDED_IND <- as.factor(data$FUNDED_IND)
data_sca$FUNDED_IND <- as.factor(data$FUNDED_IND)

# Create new variable with both state and county
data$CS <- paste(data$COUNTY, data$STATE_SHORT, sep = ", ")
data$CS
data_sca$CS <- paste(data_sca$COUNTY, data_sca$STATE_SHORT, sep = ", ")
data_sca$CS

# Split into train and test sets
train <- data_sca %>% 
  select(-State, -COUNTY, -STATE_SHORT, -HOMICIDE_RATE, -FUND_AMOUNT, -FUNDED_IND, 
         -VIOLENT_CRIME_RATE, -CS, -V1) %>%
  sample_frac(0.8, replace = FALSE)

test <- data_sca %>% 
  select(-State, -COUNTY, -STATE_SHORT, -HOMICIDE_RATE, -FUND_AMOUNT, -FUNDED_IND, 
         -VIOLENT_CRIME_RATE, -CS, -V1) %>%
  setdiff(train)

###############################################################################
# Fit Tree Regression Model to determine subset of variables
###############################################################################

# Fit model with all variables
modelgbm_full <- gbm(ANNUAL_VIOLENT_CRIMES ~ ., 
                     data = train, 
                     distribution = "gaussian", 
                     cv.folds = 10,
                     interaction.depth = 5, 
                     n.trees = 1000,
                     shrinkage = 0.01)

modelgbm_full
summary.gbm(modelgbm_full)
pretty.gbm.tree(modelgbm_full)

test_model <- modelgbm_full

# Generate prediction for the test data set
test_pred <- predict(test_model, test, n.trees = 1000)

# Mean Squared Prediction Error
round(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2), 4) 

# Root Mean Squared Prediction Error
round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4) 

# Standard Error
round(sd((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)/sqrt(dim(test)[1]), 6)

modelvariable <- data.frame(rep(1, 55))
modelvariable$variable <- summary.gbm(modelgbm_full)[,1]
modelvariable$value <- summary.gbm(modelgbm_full)[,2]
modelvariable <- modelvariable[,-1]
modelvariable

modelvariable <- modelvariable %>% filter(value >= 0.3)
modelvariable$variable <- as.factor(modelvariable$variable)
modelvariable$variable

###############################################################################
# Select necessary variables
###############################################################################

# Split into train and test sets
train <- data_sca %>% 
  select(ANNUAL_VIOLENT_CRIMES, CHLAMYDIA_CASES, SINGLE_PARENT_HOUSEHOLDS, 
           LBW_BIRTHS, INJURY_DEATHS, CHILD_TOTAL_DEATHS, PREMATURE_DEATHS, 
           TEEN_BIRTHS, DRUG_POISONING_DEATHS, SEVERE_HOUSING_PROBLEMS, 
           PREMATURE_TOTAL_DEATHS, UNEMPLOYED, PRIMARY_CARE_PHYSICIANS, 
           SOME_COLLEGE_POPULATION, MOTOR_VEHICLE_DEATHS, LIMITED_TO_HEALTHYFOOD, 
           DENTISTS, PCT_RURAL, MEDICARE_ENROLLEES, DIABETICS, NOT_PROFICIENT_IN_ENGLISH, 
           DRIVING_DEATHS, SOME_COLLEGE, CHILDREN_IN_POVERTY, MAMMO_MEDICARE_ENROLLEES, 
           UNINSURED_CHILDREN, WORKERS, ALC_IMPAIRED_DRIVING_DEATHS, LABOR_FORCE, 
           DRIVE_ALONE, POPULATION, HOUSEHOLDS, PCT_SINGLE_PARENT_HOUSEHOLDS) %>%
  sample_frac(0.8, replace = FALSE)

test <- data_sca %>% 
  select(ANNUAL_VIOLENT_CRIMES, CHLAMYDIA_CASES, SINGLE_PARENT_HOUSEHOLDS, 
         LBW_BIRTHS, INJURY_DEATHS, CHILD_TOTAL_DEATHS, PREMATURE_DEATHS, 
         TEEN_BIRTHS, DRUG_POISONING_DEATHS, SEVERE_HOUSING_PROBLEMS, 
         PREMATURE_TOTAL_DEATHS, UNEMPLOYED, PRIMARY_CARE_PHYSICIANS, 
         SOME_COLLEGE_POPULATION, MOTOR_VEHICLE_DEATHS, LIMITED_TO_HEALTHYFOOD, 
         DENTISTS, PCT_RURAL, MEDICARE_ENROLLEES, DIABETICS, NOT_PROFICIENT_IN_ENGLISH, 
         DRIVING_DEATHS, SOME_COLLEGE, CHILDREN_IN_POVERTY, MAMMO_MEDICARE_ENROLLEES, 
         UNINSURED_CHILDREN, WORKERS, ALC_IMPAIRED_DRIVING_DEATHS, LABOR_FORCE, 
         DRIVE_ALONE, POPULATION, HOUSEHOLDS, PCT_SINGLE_PARENT_HOUSEHOLDS) %>%
  setdiff(train)

###############################################################################
# Model Building Tune variable selection and parameters
###############################################################################

# Create variable to test for number of trees optimization
number_trees <- seq(1000, 6000, 500)

# Optimize number of trees
for (i in number_trees) {
  model <- gbm(ANNUAL_VIOLENT_CRIMES ~ CHLAMYDIA_CASES + SINGLE_PARENT_HOUSEHOLDS + 
                 LBW_BIRTHS + INJURY_DEATHS + CHILD_TOTAL_DEATHS + PREMATURE_DEATHS + 
                 TEEN_BIRTHS + DRUG_POISONING_DEATHS + SEVERE_HOUSING_PROBLEMS + 
                 PREMATURE_TOTAL_DEATHS + UNEMPLOYED + PRIMARY_CARE_PHYSICIANS + 
                 SOME_COLLEGE_POPULATION+ MOTOR_VEHICLE_DEATHS+ LIMITED_TO_HEALTHYFOOD+ 
                 DENTISTS + PCT_RURAL+ MEDICARE_ENROLLEES + DIABETICS + NOT_PROFICIENT_IN_ENGLISH + 
                 DRIVING_DEATHS + SOME_COLLEGE + CHILDREN_IN_POVERTY + MAMMO_MEDICARE_ENROLLEES + 
                 UNINSURED_CHILDREN + WORKERS + ALC_IMPAIRED_DRIVING_DEATHS + LABOR_FORCE + 
                 DRIVE_ALONE + POPULATION + HOUSEHOLDS + PCT_SINGLE_PARENT_HOUSEHOLDS, 
               data = train, 
               distribution = "gaussian",
               interaction.depth = 5, 
               n.trees = i,
               shrinkage = 0.01)
  
  # Generate prediction for the test data set
  test_pred <- predict(model, test, n.trees = i)
  
  # Root Mean Squared Prediction Error
  print((round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4)))
}

# Create variable to test for interaction depth
idepth <- seq(2, 14, 2)

# Optimize interaction depth
for (i in idepth) {
  model <- gbm(ANNUAL_VIOLENT_CRIMES ~ CHLAMYDIA_CASES + SINGLE_PARENT_HOUSEHOLDS + 
                 LBW_BIRTHS + INJURY_DEATHS + CHILD_TOTAL_DEATHS + PREMATURE_DEATHS + 
                 TEEN_BIRTHS + DRUG_POISONING_DEATHS + SEVERE_HOUSING_PROBLEMS + 
                 PREMATURE_TOTAL_DEATHS + UNEMPLOYED + PRIMARY_CARE_PHYSICIANS + 
                 SOME_COLLEGE_POPULATION+ MOTOR_VEHICLE_DEATHS+ LIMITED_TO_HEALTHYFOOD+ 
                 DENTISTS + PCT_RURAL+ MEDICARE_ENROLLEES + DIABETICS + NOT_PROFICIENT_IN_ENGLISH + 
                 DRIVING_DEATHS + SOME_COLLEGE + CHILDREN_IN_POVERTY + MAMMO_MEDICARE_ENROLLEES + 
                 UNINSURED_CHILDREN + WORKERS + ALC_IMPAIRED_DRIVING_DEATHS + LABOR_FORCE + 
                 DRIVE_ALONE + POPULATION + HOUSEHOLDS + PCT_SINGLE_PARENT_HOUSEHOLDS, 
               data = train, 
               distribution = "gaussian",
               interaction.depth = i, 
               n.trees = 1500,
               shrinkage = 0.01)
  
  # Generate prediction for the test data set
  test_pred <- predict(model, test, n.trees = 1500)
  
  # Root Mean Squared Prediction Error
  print((round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4)))
}

# Create variable to test for shrinkage rate
shrinkscan <- seq(0.002, 0.008, 0.001)

# Optimize shrinkage rate
for (i in shrinkscan) {
  model <- gbm(ANNUAL_VIOLENT_CRIMES ~ CHLAMYDIA_CASES + SINGLE_PARENT_HOUSEHOLDS + 
                 LBW_BIRTHS + INJURY_DEATHS + CHILD_TOTAL_DEATHS + PREMATURE_DEATHS + 
                 TEEN_BIRTHS + DRUG_POISONING_DEATHS + SEVERE_HOUSING_PROBLEMS + 
                 PREMATURE_TOTAL_DEATHS + UNEMPLOYED + PRIMARY_CARE_PHYSICIANS + 
                 SOME_COLLEGE_POPULATION+ MOTOR_VEHICLE_DEATHS+ LIMITED_TO_HEALTHYFOOD+ 
                 DENTISTS + PCT_RURAL+ MEDICARE_ENROLLEES + DIABETICS + NOT_PROFICIENT_IN_ENGLISH + 
                 DRIVING_DEATHS + SOME_COLLEGE + CHILDREN_IN_POVERTY + MAMMO_MEDICARE_ENROLLEES + 
                 UNINSURED_CHILDREN + WORKERS + ALC_IMPAIRED_DRIVING_DEATHS + LABOR_FORCE + 
                 DRIVE_ALONE + POPULATION + HOUSEHOLDS + PCT_SINGLE_PARENT_HOUSEHOLDS,
               data = train, 
               distribution = "gaussian",
               interaction.depth = 6, 
               n.trees = 1500,
               shrinkage = i)
  
  # Generate prediction for the test data set
  test_pred <- predict(model, test, n.trees = 1500)
  
  # Root Mean Squared Prediction Error
  print((round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4)))
}

###############################################################################
# Fit Tree Regression Model
###############################################################################

# Fit model with subset variables and optimized parameters
modelgbm <- gbm(ANNUAL_VIOLENT_CRIMES ~ CHLAMYDIA_CASES + SINGLE_PARENT_HOUSEHOLDS + 
                LBW_BIRTHS + INJURY_DEATHS + CHILD_TOTAL_DEATHS + PREMATURE_DEATHS + 
                TEEN_BIRTHS + DRUG_POISONING_DEATHS + SEVERE_HOUSING_PROBLEMS + 
                PREMATURE_TOTAL_DEATHS + UNEMPLOYED + PRIMARY_CARE_PHYSICIANS + 
                SOME_COLLEGE_POPULATION+ MOTOR_VEHICLE_DEATHS+ LIMITED_TO_HEALTHYFOOD+ 
                DENTISTS + PCT_RURAL+ MEDICARE_ENROLLEES + DIABETICS + NOT_PROFICIENT_IN_ENGLISH + 
                DRIVING_DEATHS + SOME_COLLEGE + CHILDREN_IN_POVERTY + MAMMO_MEDICARE_ENROLLEES + 
                UNINSURED_CHILDREN + WORKERS + ALC_IMPAIRED_DRIVING_DEATHS + LABOR_FORCE + 
                DRIVE_ALONE + POPULATION + HOUSEHOLDS + PCT_SINGLE_PARENT_HOUSEHOLDS,
                data = train, 
                distribution = "gaussian", 
                cv.folds = 10,
                interaction.depth = 6, 
                n.trees = 1500,
                shrinkage = 0.007)

modelgbm
summary.gbm(modelgbm)
pretty.gbm.tree(modelgbm)

variableimportance <- data.frame(rep(NA, 32))
variableimportance$variables <- summary.gbm(modelgbm)[,1]
variableimportance$values <- summary.gbm(modelgbm)[,2]
variableimportance <- variableimportance[,-1]
colnames(variableimportance) <- c("variable", "value")
variableimportance

variableimportance %>%
  ggplot(aes(x = reorder(variable, value), y = value)) + 
  geom_bar(stat = "identity", fill = "purple4", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Tree Regression Relative Influence", y = " ", x = "")

###############################################################################
# Score model against test data Tree Regression
###############################################################################

test_model <- modelgbm

# Generate prediction for the test data set
test_pred <- predict(test_model, test, n.trees = 4000)

# Mean Squared Prediction Error
round(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2), 4) 

# Root Mean Squared Prediction Error
round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4) 

# Standard Error
round(sd((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)/sqrt(dim(test)[1]), 6)

