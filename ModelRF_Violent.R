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
library(randomForest)

###############################################################################
# Import data sets
###############################################################################

data <- fread("data_imp_selectvar_violent.csv")
str(data)
data_sca <- fread("data_sca_imp_selectvar_violent.csv")
str(data_sca)

# Feature Generation or Variable Changes
data$FUNDED_IND <- as.factor(data$FUNDED_IND)

# Create new variable with both state and county
data$CS <- paste(data$COUNTY, data$STATE_SHORT, sep = ", ")
data$CS

# Split into train and test sets
train <- data %>% 
  select(-State, -COUNTY, -STATE_SHORT, -HOMICIDE_RATE, -FUND_AMOUNT, -FUNDED_IND, -VIOLENT_CRIME_RATE, -CS) %>%
  sample_frac(0.8, replace = FALSE)

test <- data %>% 
  select(-State, -COUNTY, -STATE_SHORT, -HOMICIDE_RATE, -FUND_AMOUNT, -FUNDED_IND, -VIOLENT_CRIME_RATE, -CS) %>%
  setdiff(train)

###############################################################################
# Fit Tree Regression Model
###############################################################################

modelRF <- randomForest(ANNUAL_VIOLENT_CRIMES ~ CHLAMYDIA_CASES + SINGLE_PARENT_HOUSEHOLDS + LBW_BIRTHS +
                          INJURY_DEATHS + DRUG_POISONING_DEATHS + LIMITED_TO_HEALTHYFOOD + PRIMARY_CARE_PHYSICIANS +
                          PREMATURE_DEATHS + CHILD_TOTAL_DEATHS + DENTISTS, data = train, mtry = 5, importance = TRUE, n.trees = 1000)

modelRF
importance(modelRF)
varImpPlot(modelRF, main = "Random Forest Importance of Variables")
