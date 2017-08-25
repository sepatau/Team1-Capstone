###############################################################################
# Shant Hambarsoumian
# PREDICT 498.56 Capstone Team 1
# Multiple Linear Regression with Regularization on Violent Crime Rate
###############################################################################

par(mfrow=c(1,1))

set.seed(1234)

library(dplyr)
library(ggplot2)
library(data.table)
library(glmnet)
library(gridExtra)

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
  select(-State, -COUNTY, -STATE_SHORT, -HOMICIDE_RATE, -FUND_AMOUNT, -FUNDED_IND, 
         -VIOLENT_CRIME_RATE, -CS, - V1) %>%
  sample_frac(0.8, replace = FALSE)

test <- data %>% 
  select(-State, -COUNTY, -STATE_SHORT, -HOMICIDE_RATE, -FUND_AMOUNT, -FUNDED_IND, 
         -VIOLENT_CRIME_RATE, -CS, -V1) %>%
  setdiff(train)

###############################################################################
# MLR to check for variable significance and create subset for modelling
###############################################################################

modelMLR <- lm(ANNUAL_VIOLENT_CRIMES~., train)

summary(modelMLR)

pvalues <- as.data.frame(summary(modelMLR)$coefficients[,4])

pvalues$variable <- rownames(pvalues)

colnames(pvalues) <- c("pvalues", "variables")

pvalues <- pvalues %>% 
  filter(pvalues < 0.05) %>% 
  filter(variables != "(Intercept)")

pvalues

###############################################################################
# Modify train and test sets for appropriate variables
###############################################################################

# Split into train and test sets
train <- data_sca %>% 
  select(pvalues$variables, ANNUAL_VIOLENT_CRIMES) %>%
  sample_frac(0.8, replace = FALSE)

test <- data_sca %>% 
  select(pvalues$variables, ANNUAL_VIOLENT_CRIMES) %>%
  setdiff(train)

###############################################################################
# Fit MLR with subset of data
###############################################################################

modelMLR_subset <- lm(ANNUAL_VIOLENT_CRIMES~., train)

summary(modelMLR_subset)

# Generate prediction for the test data set
test_pred <- predict(modelMLR_subset, test)

# Mean Squared Prediction Error
round(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2), 4) 

# Root Mean Squared Prediction Error
round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4) 

# Standard Error
round(sd((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)/sqrt(dim(test)[1]), 6)


###############################################################################
# Define X and Y for regularization
###############################################################################

x <- model.matrix(ANNUAL_VIOLENT_CRIMES~., train)
x

y <- train$ANNUAL_VIOLENT_CRIMES
y

###############################################################################
# Tune lambda parameter for regularization
###############################################################################

# Define search grid for lambda parameter
grid <- 10^seq(1, -20, length = 70)

for (i in grid) {
  
  # Fit linear regularization
  modelridge <- glmnet(x, y, alpha = 0, lambda = i)
  
  # Define x matrix for test model
  xtest <- model.matrix(ANNUAL_VIOLENT_CRIMES~., test)
  
  # Generate prediction for the test data set
  test_pred <- predict(modelridge, s = i, newx = xtest)
  
  # Root Mean Squared Prediction Error
  print(round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4))
  
}

###############################################################################
# Fit optimal model and evaluate
###############################################################################

# Fit linear regularization
modelridge <- glmnet(x, y, alpha = 0, lambda = grid[22])

summary(modelridge)
modelridge
coefficients(modelridge)

# Store model coefficients in a dataframe
modelcoef <- data.frame(coefficients(modelridge)[,1])
modelcoef[,2] <- rownames(coefficients(modelridge))
colnames(modelcoef) <- c("value", "variable")
modelcoef

###############################################################################
# Score optimal model against test data
###############################################################################

# Define x matrix for test model
xtest <- model.matrix(ANNUAL_VIOLENT_CRIMES~., test)

# Generate prediction for the test data set
test_pred <- predict(modelridge, s = grid[22], newx = xtest)

# Mean Squared Prediction Error
round(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2), 4) 

# Root Mean Squared Prediction Error
round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4) 

# Standard Error
round(sd((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)/sqrt(dim(test)[1]), 6)

###############################################################################
# Model Visualizations
###############################################################################

# Bar plot of coefficients
modelcoef %>%
  filter(variable != "(Intercept)") %>%
  ggplot(aes(x = reorder(variable, value), y = value)) + 
  geom_bar(stat = "identity", fill = "purple4", alpha = 0.8) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Ridge Regression Coefficients", y = " ", x = "")

# LBW Births
lbwbirths <- data %>%
  ggplot(aes(x = LBW_BIRTHS, y = ANNUAL_VIOLENT_CRIMES)) + 
  geom_point(color = "purple4") +
  geom_smooth(color = "gold") +
  theme_bw() +
  labs(title = "Low Birth Weight vs Annual Violent Crimes", 
       y = "Number of Violent Crimes per Year", 
       x = "Number of Low Weight Births per Year")

# Some College Population
scp <- data %>%
  ggplot(aes(x = SOME_COLLEGE_POPULATION, y = ANNUAL_VIOLENT_CRIMES)) + 
  geom_point(color = "purple4") +
  geom_smooth(color = "gold") +
  theme_bw() +
  labs(title = "Some College Education vs Annual Violent Crimes", 
       y = "Number of Violent Crimes per Year", 
       x = "Population with Some College Education")

# Two parent household
house <- data %>%
  ggplot(aes(x = -HOUSEHOLDS, y = ANNUAL_VIOLENT_CRIMES)) + 
  geom_point(color = "purple4") +
  geom_smooth(color = "gold") +
  theme_bw() +
  labs(title = "Number One Parent Households vs Annual Violent Crimes", 
       y = "Number of Violent Crimes per Year", 
       x = "Number of One Parent Households")

grid.arrange(lbwbirths, scp, house)
