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
library(corrplot)

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
# Relationship between annual violent crime rate and homicide rate
###############################################################################

data %>%
  ggplot(aes(x = VIOLENT_CRIME_RATE, y = ANNUAL_VIOLENT_CRIMES, 
             color = FUNDED_IND, shape = FUNDED_IND, size = FUND_AMOUNT)) + 
  geom_jitter(alpha = 0.7) +
  theme_bw() +
  scale_color_manual(values = c("gold", "purple4")) +
  labs(title = "Annual Violent Crimes vs Violent Crime Rate", 
       y = "Annual Violent Crimes", x = "Violent Crime Rate")

###############################################################################
# Plot most dangerous counties
###############################################################################

data %>%
  top_n(25, ANNUAL_VIOLENT_CRIMES) %>%
  ggplot(aes(x = reorder(CS, ANNUAL_VIOLENT_CRIMES), y = ANNUAL_VIOLENT_CRIMES, 
             fill = FUNDED_IND)) + 
  geom_bar(stat = "identity", alpha = 0.7) +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = c("gold", "purple4")) +
  labs(title = "Highest Annual Number of Violent Crimes with Funding Indicator", y = "", x = "")


###############################################################################
# Correlations
###############################################################################

# Create data frame with only numeric variables
data_numeric <- data_sca %>% select(-V1, -State, -COUNTY, -STATE_SHORT)

# Create correlation matrix and convert into a data frame
data_cor <- cor(data_numeric)
class(data_cor)
dim(data_cor)
data_cor <- as.data.frame(data_cor)

# Pull correlations with variable of interest and create table
data_cor$variable <- rownames(data_cor)

data_cor %>% 
  select(ANNUAL_VIOLENT_CRIMES, variable) %>%
  filter(variable != "ANNUAL_VIOLENT_CRIMES") %>%
  top_n(11, ANNUAL_VIOLENT_CRIMES)


