###############################################################################
# Shant Hambarsoumian
# PREDICT 498.56 Capstone Team 1
# Dimensionality Reduction and High Value Variable Identification
###############################################################################

library(dplyr)
library(ggplot2)
library(gbm)
library(reshape2)
library(caret)

###############################################################################
# Split data into train and test
###############################################################################

set.seed(123)

data_sca <- read.csv("imp_scaled_data.csv")
data <- read.csv("imp_data.csv")

train <- data_sca %>% 
  select(-State, -COUNTY, -STATE_SHORT, -VIOLENT_CRIME_RATE, -HOMICIDE_RATE, 
         -FUND_AMOUNT, -X) %>% 
  sample_frac(0.85)

test <- data_sca %>%
  select(-State, -COUNTY, -STATE_SHORT, -VIOLENT_CRIME_RATE, -HOMICIDE_RATE,
         -FUND_AMOUNT, -X) %>% 
  setdiff(train)

###############################################################################
# Final model for analysis of Funding by DOJ
###############################################################################

# Train model
model1 <- gbm(FUNDED_IND~., train, distribution = "bernoulli", 
              n.trees = 4000, interaction.depth = 20, shrinkage = 0.01)

# Generate prediction for the test data set
test_pred1 <- predict(model1, test, n.trees = 4000)

# Root Mean Squared Prediction Error
confusionMatrix(test_pred1, test$FUNDED_IND)

# Check model diagnostics
pretty.gbm.tree(model1)
rel.inf1 <- summary(model1)
rel.inf1

###############################################################################
# Generate Visualizations of DOJ Funding
###############################################################################

# Variables to keep
var.keep1 <- rel.inf1 %>% filter(rel.inf >= 0.938)

# Influence of all Variables
rel.inf1 %>%
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "List of Variables by Inluence", y = "", x = "")

# Most influential Variables
rel.inf1 %>%
  top_n(25, rel.inf) %>%
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Top 25 Most Influential Variables for DOJ Funding", y = "", x = "")

# Least influential Variables
rel.inf1 %>%
  top_n(-50, rel.inf) %>%
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Bottom 50 Least Influential Variables for DOJ Funding", y = "", x = "")

# Create data frame to store all low value variables and their values
lowvalue <- rel.inf1 %>% filter(rel.inf <= 0.938)
lowvalue <- lowvalue$var
lowvalue <- as.character(lowvalue)
lowvalue_df <- train %>% select(lowvalue)

# Plot low value variables against Annual Violent Crimes
# Melt data set to evalute variables individually
lowvalue_melt <- melt(lowvalue_df)
lowvalue_melt$FUNDED_IND <- rep(train$FUNDED_IND, 50)

# Violin Plot vs DOJ Funding low value variables
lowvalue_melt$FUNDED_IND <- as.factor(lowvalue_melt$FUNDED_IND)

lowvalue_melt %>%
  ggplot(aes( x = FUNDED_IND, y = value)) +
  geom_violin(alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "", y = "")

# Create data frame to store all high value variables and their values
highvalue <- rel.inf1 %>% filter(rel.inf >= 1.25)
highvalue <- as.character(highvalue$var)
highvalue_df <- train %>% select(highvalue)

# Plot low value variables against Annual Violent Crimes
# Melt data set to evalute variables individually
highvalue_melt <- melt(highvalue_df)
highvalue_melt$FUNDED_IND <- rep(train$FUNDED_IND, 25)

# Violinplot vs DOJ Funding high value variables
highvalue_melt$FUNDED_IND <- as.factor(highvalue_melt$FUNDED_IND)

highvalue_melt %>%
  ggplot(aes(x = FUNDED_IND, y = value)) +
  geom_violin(alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = "", y = "")

###############################################################################
# Create new data frame with only variables being kept and store as csv
###############################################################################

# Remove variable column and store
var.keep1 <- as.vector(var.keep1$var)

# Using above variable select needed columns
data <- data %>% select(var.keep1, State, COUNTY, STATE_SHORT, VIOLENT_CRIME_RATE, 
                        HOMICIDE_RATE, FUND_AMOUNT)
dim(data)

data_sca <- data_sca %>% select(var.keep1, State, COUNTY, STATE_SHORT, VIOLENT_CRIME_RATE, 
                                HOMICIDE_RATE, FUND_AMOUNT)
dim(data_sca)

# Save data frames to csv
write.csv(data, "data_imp_selectvar_funding.csv")
write.csv(data_sca, "data_sca_imp_selectvar_funding.csv")
