###############################################################################
# Shant Hambarsoumian
# PREDICT 498.56 Capstone Team 1
# Dimensionality Reduction and High Value Variable Identification
###############################################################################

library(dplyr)
library(ggplot2)
library(gbm)
library(reshape2)

###############################################################################
# Split data into train and test
###############################################################################

set.seed(123)

data_sca <- read.csv("imp_scaled_data.csv")
data <- read.csv("imp_data.csv")

train <- data_sca %>% 
  select(-State, -COUNTY, -STATE_SHORT, -VIOLENT_CRIME_RATE, -HOMICIDE_RATE, 
         -FUND_AMOUNT, -FUNDED_IND, -X) %>% 
  sample_frac(0.85)

test <- data_sca %>%
  select(-State, -COUNTY, -STATE_SHORT, -VIOLENT_CRIME_RATE, -HOMICIDE_RATE,
         -FUND_AMOUNT, -FUNDED_IND, -X) %>% 
  setdiff(train)

###############################################################################
# Final model for analysis of Annual Violent Crimes
###############################################################################

# Train model
model <- gbm(ANNUAL_VIOLENT_CRIMES~., train, distribution = "gaussian", 
             n.trees = 4000, interaction.depth = 20, shrinkage = 0.01)

# Generate prediction for the test data set
test_pred <- predict(model, test, n.trees = 4000)

# Root Mean Squared Prediction Error
print((round(sqrt(mean((test$ANNUAL_VIOLENT_CRIMES - test_pred)^2)), 4)))

# Check model diagnostics
pretty.gbm.tree(model)
rel.inf <- summary(model)
rel.inf

###############################################################################
# Generate Visualizations of Annual Violent Crimes
###############################################################################

# Variables to keep
var.keep <- rel.inf %>% filter(rel.inf >= 0.1)
var.keep

# Influence of all Variables
rel.inf %>%
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "List of Variables to be Included by Influence on Violent Crimes", y = "", x = "")

# Most influential Variables
rel.inf %>%
  top_n(25, rel.inf) %>%
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Top 25 Most Influential Variables", y = "", x = "")

# Least influential Variables
rel.inf %>%
  filter(rel.inf <= 0.1) %>%
  ggplot(aes(x = reorder(var, rel.inf), y = rel.inf)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(title = "Least Influential Variables for Violent Crimes", y = "", x = "")

# Create data frame to store all low value variables and their values
lowvalue <- rel.inf %>% filter(rel.inf <= 0.1)
loevalue <- lowvalue$var
lowvalue <- as.character(lowvalue)
lowvalue_df <- train %>% select(lowvalue)

# Plot low value variables against Annual Violent Crimes
# Melt data set to evalute variables individually
lowvalue_melt <- melt(lowvalue_df)
lowvalue_melt$ANNUAL_VIOLENT_CRIMES <- rep(train$ANNUAL_VIOLENT_CRIMES, 48)

# Scatterplots vs Annual violent crimes low value variables
lowvalue_melt %>%
  ggplot(aes( x = value, y = ANNUAL_VIOLENT_CRIMES)) +
  geom_point(alpha = 0.5, shape = 1) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "", y = "")

# Create data frame to store all high value variables and their values
highvalue <- rel.inf %>% filter(rel.inf >= 0.96)
highvalue <- as.character(highvalue$var)
highvalue_df <- train %>% select(highvalue)

# Plot low value variables against Annual Violent Crimes
# Melt data set to evalute variables individually
highvalue_melt <- melt(highvalue_df)
highvalue_melt$ANNUAL_VIOLENT_CRIMES <- rep(train$ANNUAL_VIOLENT_CRIMES, 25)

# Scatterplots vs Annual violent crimes high value variables
highvalue_melt %>%
  ggplot(aes( x = value, y = ANNUAL_VIOLENT_CRIMES)) +
  geom_point(alpha = 0.5, shape = 1) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "", y = "")

###############################################################################
# Create new data frame with only variables being kept and store as csv
###############################################################################

# Remove variable column and store
var.keep1 <- as.vector(var.keep1$var)

# Using above variable select needed columns
data <- data %>% select(var.keep1, State, COUNTY, STATE_SHORT, VIOLENT_CRIME_RATE, 
                        HOMICIDE_RATE, FUND_AMOUNT, FUNDED_IND)
dim(data)

data_sca <- data_sca %>% select(var.keep1, State, COUNTY, STATE_SHORT, VIOLENT_CRIME_RATE, 
                                HOMICIDE_RATE, FUND_AMOUNT, FUNDED_IND)
dim(data_sca)

# Save data frames to csv
write.csv(data, "data_imp_selectvar_violent.csv")
write.csv(data_sca, "data_sca_imp_selectvar_violent.csv")
