###############################################################################
# Shant Hambarsoumian
# PREDICT 498.56 Capstone Team 1
# EDA on Violent crimes data
###############################################################################

par(mfrow=c(1,1))

set.seed(1234)

library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(gridExtra)
library(maps)

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
train <- data %>% sample_frac(0.8, replace = FALSE)
test <- data %>% setdiff(train)

###############################################################################
# Relationship between violent crime rate and homicide rate
###############################################################################

data %>%
  ggplot(aes(x = HOMICIDE_RATE, y = ANNUAL_VIOLENT_CRIMES, 
             color = FUNDED_IND, shape = FUNDED_IND, size = FUND_AMOUNT)) + 
  geom_jitter() +
  theme_minimal() +
  labs(title = "Annual Violent Crimes vs Homicide Rate with Funding Indicator", 
       y = "Annual Violent Crimes", x = "Homicide Rate")

###############################################################################
# Plot most dangerous counties
###############################################################################

data %>%
  top_n(25, ANNUAL_VIOLENT_CRIMES) %>%
  ggplot(aes(x = reorder(CS, ANNUAL_VIOLENT_CRIMES), y = ANNUAL_VIOLENT_CRIMES, 
             fill = FUNDED_IND)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Dangerous Counties by Annual Number of Violent Crimes with Funding Indicator", y = "", x = "")

###############################################################################
# Scatter Plots with Regression Top 4 highest correlations
###############################################################################

crime_cor %>% top_n(6, abs(ANNUAL_VIOLENT_CRIMES))

plot1 <- train %>%
  ggplot(aes(x = CHLAMYDIA_CASES, y = ANNUAL_VIOLENT_CRIMES, 
             color = FUNDED_IND, shape = FUNDED_IND)) + 
  geom_jitter() +
  geom_smooth(method = "gam") +
  scale_shape(solid = FALSE) +
  theme_minimal() +
  guides(color = FALSE, shape = FALSE) +
  labs(title = "Chlamydia Cases vs Annual Violent Crimes", 
       y = "Annual Violent Crimes", x = "Chlamydia Cases")

plot2 <- train %>%
  ggplot(aes(x = PREMATURE_DEATHS, y = ANNUAL_VIOLENT_CRIMES, 
             color = FUNDED_IND, shape = FUNDED_IND)) + 
  geom_jitter() +
  geom_smooth(method = "gam") +
  scale_shape(solid = FALSE) +
  theme_minimal() +
  guides(color = FALSE, shape = FALSE) +
  labs(title = "Premature Deaths vs Annual Violent Crimes", 
       y = "Annual Violent Crimes", x = "Premature Deaths")

plot3 <- train %>%
  ggplot(aes(x = CHILD_TOTAL_DEATHS, y = ANNUAL_VIOLENT_CRIMES, 
             color = FUNDED_IND, shape = FUNDED_IND)) + 
  geom_jitter() +
  geom_smooth(method = "gam") +
  scale_shape(solid = FALSE) +
  theme_minimal() +
  guides(color = FALSE, shape = FALSE) +
  labs(title = "Child Total Deaths vs Annual Violent Crimes", 
       y = "Annual Violent Crimes", x = "Child Total Deaths")

plot4 <- train %>%
  ggplot(aes(x = SINGLE_PARENT_HOUSEHOLDS, y = ANNUAL_VIOLENT_CRIMES, 
             color = FUNDED_IND, shape = FUNDED_IND)) + 
  geom_jitter() +
  geom_smooth(method = "gam") +
  scale_shape(solid = FALSE) +
  theme_minimal() +
  guides(color = FALSE, shape = FALSE) +
  labs(title = "Single Parent Household vs Annual Violent Crimes", 
       y = "Annual Violent Crimes", x = "Single Parent Household")

grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

###############################################################################
# Chloropleth by State
###############################################################################

train_state <- train %>% 
  group_by(State) %>% 
  summarise(state_violence = sum(ANNUAL_VIOLENT_CRIMES))

train_state <- as.data.frame(train_state)
colnames(train_state) <- c("state", "violence")
train_state

states_map <- map_data("state")

train_statesmap <- merge(states_map, train_state, by.x = "region", by.y = "state")

train_statesmap %>%
  ggplot(aes(map_id = region, fill = violence)) +
  geom_map(map = states_map, colour = "black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")

###############################################################################
# Correlation Plot
###############################################################################

crime_cor <- train %>% select(-State, -COUNTY, -STATE_SHORT, -CS, -FUNDED_IND) %>% cor()
crime_cor <- as.data.frame(crime_cor)
crime_cor$variablenames <- colnames(crime_cor)
crime_cor <- crime_cor %>% select(variablenames, ANNUAL_VIOLENT_CRIMES)
rownames(crime_cor) <- 1:59
crime_cor$index <- 1:nrow(crime_cor)
crime_cor$facet <- rep(1, 59)

crime_cor %>%
  ggplot(aes(x = variablenames, y = facet, fill = ANNUAL_VIOLENT_CRIMES)) + 
  geom_tile() +
  facet_grid(~index)