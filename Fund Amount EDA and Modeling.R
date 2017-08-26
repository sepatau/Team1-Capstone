###################################################################################
##################### Northwester University - MSPA ###############################
################### Team 1 - Capstone (2017 Summer Semester) ######################
###################################################################################

# Loading packages
library(lattice)
library(car)
library(MASS)
library(randomForest)
library(rpart)
library(forecast)
library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(RSNNS)
library(mgcv)
library(caret)
library(gbm)
library(glmnet)
library(reprtree)

###################################################################################
###################################### Loading Data ###############################
###################################################################################
raw_data <- read.csv("C:\\Users\\Max\\Documents\\Github\\Team1-Capstone\\data_imp_selectvar_funding.csv")
raw_data_std <- read.csv("C:\\Users\\Max\\Documents\\Github\\Team1-Capstone\\data_sca_imp_selectvar_funding.csv")
raw_data[,1] <- NULL
raw_data_std[,1] <- NULL  

# Removing State because we already have short-name of state
raw_data <- raw_data[,-54] 
raw_data_std <- raw_data_std[,-54]

# Combining State and County Together:
raw_data$ST_COUNTY <- as.factor(paste(raw_data$STATE_SHORT, raw_data$COUNTY,sep=" "))
raw_data_std$ST_COUNTY <- as.factor(paste(raw_data_std$STATE_SHORT, raw_data_std$COUNTY,sep=" "))

full_set <- read.csv("C:\\Users\\Max\\Documents\\Github\\Team1-Capstone\\imp_data.csv")

raw_data <- cbind(raw_data,POPULATION=full_set$POPULATION)
raw_data <- cbind(raw_data,HOUSEHOLD_INCOME=full_set$HOUSEHOLD_INCOME)
popul_std <- normalizeData(full_set$POPULATION, type="norm")
popul_std <- data.frame(POPULATION=popul_std)
raw_data_std <- cbind(raw_data_std,popul_std)


# Extracting only FUNDED Values from Raw Data:
raw_data_funded <- raw_data[raw_data$FUND_AMOUNT>0,]
raw_data_funded_std <- raw_data_std[raw_data_std$FUND_AMOUNT>0,]


###################################################################################
############################## TRAIN & TEST SPLIT #################################
############################### FUNDED DATA ONLY ##################################
###################################################################################
# Training
set.seed(54321)
train <- sample(nrow(raw_data_funded),round(nrow(raw_data_funded)*0.6),replace=FALSE)
raw_data_funded_train <- raw_data_funded[train,]
remaining <- raw_data_funded[-train,]

# Validation and Test
set.seed(12345)
val <- sample(nrow(remaining),round(nrow(remaining)*0.5),replace=FALSE)
raw_data_funded_valid <- remaining[val,]
raw_data_funded_test  <- remaining[-val,]

###################################################################################
### Now Standardization ###
# Training
set.seed(54321)
train <- sample(nrow(raw_data_funded_std),round(nrow(raw_data_funded_std)*0.6),replace=FALSE)
raw_data_funded_std_train <- raw_data_funded_std[train,]
remaining <- raw_data_funded_std[-train,]

# Validation and Test
set.seed(12345)
val <- sample(nrow(remaining),round(nrow(remaining)*0.5),replace=FALSE)
raw_data_funded_std_valid <- remaining[val,]
raw_data_funded_std_test  <- remaining[-val,]

###################################################################################
### Influential Point ###
### Removing CA Los Angeles:
raw_data_funded_train <- raw_data_funded_train[-67,]
raw_data_funded_std_train <- raw_data_funded_std_train[-67,]


#########################################################################################
################################USER DEFINED FUNCTIONS###################################
#########################################################################################
hist_func <- function(data,...){
  par(mfrow=c(2,2))
  names <- names(data)
  for (i in 1:ncol(data))
  {
    hist(data[,i], main=names[i], xlab=names[i])
  }
}


hist_func_with_trans <- function(data,trans,...){
  par(mfrow=c(2,2))
  names <- names(data)
  t_names <- names(trans)
  for (i in 1:ncol(data))
  {
    hist(data[,i], main=names[i], xlab=names[i])
    hist(trans[,i], main=paste("T_",t_names[i]), xlab=paste("T_",t_names[i]))
  }
}


#Box-and-Whisker Function:
bw_func <- function(data){
  for (i in 1:ncol(data)){
    graph <- bwplot(~ data[,i],
                    xlab=colnames(data)[i],
                    main=colnames(data)[i])
    print(graph)
  }
}

# XY Points Function:
gg_xypoints <- function(data){
  names <- names(data)
  for (i in 1:ncol(data)){
    g_var <- ggplot(data, aes(x=FUND_AMOUNT, y=data[,i])) + 
      geom_point() +
      labs(x="FUND_AMOUNT", y=names[i], title=paste("FUND AMOUNT vs",names[i]))
    print(g_var)
  }
}


#Density Function:
dens_func <- function(data){
  for (i in 1:ncol(data)){
    graph <- densityplot(~data[,i],
                         xlab=colnames(data)[i],type="g",
                         main=paste("Density Plot of ",colnames(data)[i],"and CARAVAN"))
    print(graph)
  }
}

###################################################################################
############################## TRAIN & TEST SPLIT #################################
############################### ALL OBSERVATIONS ##################################
###################################################################################
# Training
set.seed(54321)
train <- sample(nrow(raw_data),round(nrow(raw_data)*0.6),replace=FALSE)
raw_data_train <- raw_data[train,]
remaining <- raw_data[-train,]

# Validation and Test
set.seed(12345)
val <- sample(nrow(remaining),round(nrow(remaining)*0.5),replace=FALSE)
raw_data_valid <- remaining[val,]
raw_data_test  <- remaining[-val,]



###################################################################################
################################ Explantory Data Analysis #########################
###################################################################################

# Correlation:
cor_df <- data.frame(cor=apply(raw_data_train[,-c(54,55,59)],2,function(x) cor(x,raw_data_train$FUND_AMOUNT)))
cor_df$abs_cor <- abs(cor_df$cor)
cor_df <- cor_df[order(-cor_df$abs_cor),]
cor_df
row.names(cor_df)


# Plotting Funding to Violent Crime Rate:
ggplot(raw_data_train, aes(FUND_AMOUNT,VIOLENT_CRIME_RATE)) + 
  geom_point()

ggplot(raw_data_train, aes(FUND_AMOUNT,ANNUAL_VIOLENT_CRIMES)) + 
  geom_point()

raw_data_train$VC_TO_POP <- (raw_data_train$ANNUAL_VIOLENT_CRIMES / raw_data_train$POPULATION) * 100
raw_data_train$POP_TO_VC <- raw_data_train$POPULATION / raw_data_train$ANNUAL_VIOLENT_CRIMES


# Graph Comparing Funds to Annual Violen Crime:
raw_data_train %>% filter(FUND_AMOUNT >= 500000 | ANNUAL_VIOLENT_CRIMES>=5000) %>% 
  ggplot(aes(FUND_AMOUNT,ANNUAL_VIOLENT_CRIMES, label=ST_COUNTY)) + 
  geom_point(aes(size=POPULATION)) + 
  geom_text_repel(size=3,
                  box.padding = unit(0.05, "lines"),
                  point.padding = unit(0.1, "lines"),
                  segment.color = 'grey50',
                  col="#F8766D") +                 #This is the ggplot color for red#
  scale_x_continuous(labels = scales::comma) +
  labs(x="Fund Amount", y="Annual Violen Crimes",title="Funds vs Annual Violent Crimes Plot   [Funds > $500,000 or Crime > 5,000]")+
  theme(plot.title = element_text(hjust = 0.5)) 


# Comparing Annual Violent Crime / Population against Funds:
raw_data_train$VC_CATEGORY <- ifelse(raw_data_train$ANNUAL_VIOLENT_CRIMES >= 5000,5, 
                                     ifelse(raw_data_train$ANNUAL_VIOLENT_CRIMES < 5000 & raw_data_train$ANNUAL_VIOLENT_CRIMES >= 4000, 4, 
                                            ifelse(raw_data_train$ANNUAL_VIOLENT_CRIMES<4000 & raw_data_train$ANNUAL_VIOLENT_CRIMES>=3000,3,
                                                   ifelse(raw_data_train$ANNUAL_VIOLENT_CRIMES<3000 & raw_data_train$ANNUAL_VIOLENT_CRIMES>=2000,2,
                                                          ifelse(raw_data_train$ANNUAL_VIOLENT_CRIMES<2000 & raw_data_train$ANNUAL_VIOLENT_CRIMES>=1000,1,
                                                                 ifelse(raw_data_train$ANNUAL_VIOLENT_CRIMES<1000,0,6))))))
raw_data_train$VC_CATEGORY <- as.factor(raw_data_train$VC_CATEGORY)

raw_data_train$VC_TO_POP_CATEGORY <- ifelse(raw_data_train$VC_TO_POP > 10 ,8, 
                                            ifelse(raw_data_train$VC_TO_POP < 10 & raw_data_train$VC_TO_POP >= 5, 7, 
                                                   ifelse(raw_data_train$VC_TO_POP < 5.0 & raw_data_train$VC_TO_POP >= 3, 6, 
                                                          ifelse(raw_data_train$VC_TO_POP < 3.0 & raw_data_train$VC_TO_POP >= 1, 5,
                                                                 ifelse(raw_data_train$VC_TO_POP < 1.0 & raw_data_train$VC_TO_POP >= 0.8, 4, 
                                                                        ifelse(raw_data_train$VC_TO_POP< 0.8 & raw_data_train$VC_TO_POP>= 0.6,3,
                                                                               ifelse(raw_data_train$VC_TO_POP<0.6 & raw_data_train$VC_TO_POP>=0.4,2,
                                                                                      ifelse(raw_data_train$VC_TO_POP<0.4 & raw_data_train$VC_TO_POP>=0.2,1,
                                                                                             ifelse(raw_data_train$VC_TO_POP<0.2,0,99)))))))))
raw_data_train$VC_TO_POP_CATEGORY <- as.factor(raw_data_train$VC_TO_POP_CATEGORY)


# Bar graph with top 20 counties that received the most funds:
raw_data_train %>% filter(FUND_AMOUNT > 250000) %>% 
  ggplot(aes(x=reorder(ST_COUNTY,FUND_AMOUNT), y=FUND_AMOUNT, fill=VC_TO_POP_CATEGORY)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Funded Counties with Annual Violent Crime to Population Ratio",
       y="FUND AMOUNT", x="STATE & COUNTY", fill="Violent\nCrime\nto Popul.\nCategory\n (0-low)\n (5-high)") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = "center") 


# Homicide Rate vs Fund Amount:
raw_data_train %>% filter(FUND_AMOUNT > 0) %>% 
  ggplot(aes(FUND_AMOUNT,HOMICIDE_RATE, label=ST_COUNTY)) + 
  geom_point(aes(size=POPULATION)) + 
  geom_text_repel(size=3,
                  box.padding = unit(0.05, "lines"),
                  point.padding = unit(0.1, "lines"),
                  segment.color = 'grey50',
                  col="#F8766D") +                 #This is the ggplot color for red#
  scale_x_continuous(labels = scales::comma) +
  labs(x="Fund Amount", y="Homicide Rate",title="Funds vs Homicide Rate   [Funds > 0]")+
  theme(plot.title = element_text(hjust = 0.5)) 


##########################################################################################
##################### Review of Transformations - LOG, SQRT, BOXCOX: #####################
##########################################################################################
# BoxCox
non_categ_index <- sapply(raw_data_train, is.factor)
lambda = matrix(NA, nrow=60, ncol=1)
for (i in 1:60){
  if (non_categ_index[i]==FALSE){
    lambda[i,] <- BoxCox.lambda(raw_data_train[,i])
  }
  else 
    lambda[i,] <- NA
}

#Creating boxcox training dataset:
train_box_tran <- raw_data_train

not_factor_index <- which(!is.na(lambda[,1]))
for (i in not_factor_index){
  train_box_tran[,i] <- BoxCox(train_box_tran[,i],lambda[i,])
}


# Comparing Transformation:
shapiro = matrix(NA, nrow=60, ncol=1)
for (i in non_categ_index){
  shapiro[i] <- shapiro.test(raw_data_train[,i])$statistic
}


train_log_tran <- raw_data_train
for (i in 1:60){
  for (j in 1:1885){
    if (train_log_tran[j,i]==0){
      train_log_tran[j,i]=1
    }
  }
}


shapiro_log <- matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro_log[i] <- shapiro.test(log(train_log_tran[,i]))$statistic
}


shapiro_sqrt <-  matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro_sqrt[i] <- shapiro.test(sqrt(raw_data_train[,i]))$statistic
}

shapiro_boxcox <- matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro_boxcox[i] <- shapiro.test(train_box_tran[,i])$statistic
}



shapiro_tests <- cbind(shapiro,shapiro_log,shapiro_sqrt,shapiro_boxcox)
row.names(shapiro_tests) <- colnames(raw_data_train[1:60])
colnames(shapiro_tests) <- c("No-Tran","Log","Sqrt","BoxCox")

best <- apply(shapiro_tests,1, function(x) which.max(x))
best <- data.frame(matrix(best))
best$value <- as.numeric(best$matrix.best.)
best_names <- matrix(NA, nrow=60, ncol=1)
for (i in 1:60){
  value = best$value[i]
  best_names[i]  <- colnames(shapiro_tests)[value]
}
colnames(best_names) <- c("Best")

shapiro_tests <- cbind(shapiro_tests,best_names)
row.names(best_names) <- colnames(raw_data_train[1:60])


log_index <- which(best_names=="Log")
sqrt_index <- which(best_names=="Sqrt")
boxcox_index <- which(best_names=="BoxCox")
notran_index <- which(best_names=="No-Tran")


# Creating new transformed dataset:
train_transformed <- raw_data_train[,notran_index]

train_log_tran <- apply(train_log_tran[,log_index], 2, function(x) log(x))
train_transformed <- cbind(train_transformed,train_log_tran)

train_sqrt_tran <- apply(raw_data_train[,sqrt_index], 2, function(x) sqrt(x))
train_transformed <- cbind(train_transformed,train_sqrt_tran)

train_transformed <- cbind(train_transformed,train_box_tran[,boxcox_index])

train_transformed <- cbind(train_transformed,raw_data_train[,c(54,55,59)])
colnames <- colnames(raw_data_train[1:60])
train_transformed <- train_transformed[,colnames]


#Checking for influential points:
gg_xypoints(train_transformed[,c(1:10,58)])

#Checking correlations after transformation:
cor_df <- data.frame(cor=apply(train_transformed[,-c(54,55,59)],2,function(x) cor(x,train_transformed$FUND_AMOUNT)))
cor_df$abs_cor <- abs(cor_df$cor)
cor_df <- cor_df[order(-cor_df$abs_cor),]
cor_df


# Correlation:
cor_df <- data.frame(cor=apply(raw_data_train[,-c(54,55,59,63,64)],2,function(x) cor(x,raw_data_train$FUND_AMOUNT)))
cor_df$abs_cor <- abs(cor_df$cor)
cor_df <- cor_df[order(-cor_df$abs_cor),]
cor_df


# Determining Linearity:
xyplot(train_transformed$ANNUAL_VIOLENT_CRIMES~train_transformed$FUND_AMOUNT)
table(Violence=cut(train_transformed$ANNUAL_VIOLENT_CRIMES,5),Funds=cut(train_transformed$FUND_AMOUNT,5))

xyplot(train_transformed$ANNUAL_VIOLENT_CRIMES[train_transformed$FUND_AMOUNT==0]~
         train_transformed$PCT_EXCESSIVE_DRINKING[train_transformed$FUND_AMOUNT==0],
       ylab="POPULATION",xlab="VIOLENT CRIMES")





# Determining Normality Before and After Transformation:
par(mfrow=c(2,2))
for (i in 1:60){
  plot(raw_data_train[,i],raw_data_train$FUND_AMOUNT,
       ylab="DOJ_FUND_AMOUNT", xlab=colnames(raw_data_train)[i])
}

par(mfrow=c(2,2))
for (i in 1:60){
  plot(train_transformed[,i],train_transformed$FUND_AMOUNT,
       ylab="DOJ_FUND_AMOUNT", xlab=colnames(train_transformed)[i])
}


# Modeling After Transformation:
regfit_trans <- lm(FUND_AMOUNT~.,data=train_transformed[,-c(54,59)])
summary(regfit_trans)
regfit_trans_both <- stepAIC(regfit_trans, direction="both", k=7.541683)
summary(regfit_trans_both)


####################################################################################
########################### FUNDED DATASET TRANSFORMATIONS #########################
####################################################################################
# Transformations LOG, SQRT, BOXCOX:

# BoxCox
lambda_matrix <- data.frame(matrix(NA,ncol=2,nrow=60))
colnames(lambda_matrix) <- c("lambda", "lambda2")
for (i in 44:60){
  if (!is.factor(raw_data_funded_train[,i])){
    m1 <- boxcoxfit(raw_data_funded_train[,i], lambda2=TRUE)
    lambda_matrix[i,1] <- m1$lambda[[1]]
    lambda_matrix[i,2] <- m1$lambda[[2]]
  }
  else {
    lambda_matrix[i,1] <- NA
    lambda_matrix[i,2] <- NA
  }
  print(paste("Value",i,"Done!"))
}

lambda_index <- which(!is.na(lambda_matrix[1]))
lambda_na_index <- which(is.na(lambda_matrix[1]))

boxcoxTrans <- function(x, lam1, lam2) {
  if (lam1 == 0L) {
    data <- log(x + lam2)
  } else {
    data <- (((x + lam2)^lam1) - 1) / lam1
  }
  return(data)
}


skewed_vars <- seq(1,60,1)

raw_data_funded_train_trans <- data.frame(V1=matrix(NA,ncol=1,nrow=nrow(raw_data_funded_train)))
v_names <- colnames(raw_data_funded_train)
for (i in skewed_vars){
  if (!is.na(lambda_matrix[i,1])){
    data <- data.frame(boxcoxTrans(raw_data_funded_train[i],lambda_matrix[i,1],lambda_matrix[i,2]))
    colnames(data) <- v_names[i]
    raw_data_funded_train_trans <- cbind(raw_data_funded_train_trans,data)
  }
}
raw_data_funded_train_trans$V1 <- NULL
raw_data_funded_train_trans <- cbind(raw_data_funded_train_trans,raw_data_funded_train[,lambda_na_index])
raw_data_funded_train_trans <- raw_data_funded_train_trans[,colnames(raw_data_train_trans[1:60])]






# Comparing Transformation:
not_factor_index <- c()
for (i in 1:ncol(raw_data_funded_train)) {
  if (!is.factor(raw_data_funded_train[,i])) {
    not_factor_index <- append(not_factor_index,i)
  }
}

shapiro = matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro[i] <- shapiro.test(raw_data_funded_train[,i])$statistic
}


train_log_tran <- raw_data_funded_train
for (i in 1:60){
  for (j in 1:109){
    if (train_log_tran[j,i]==0){
      train_log_tran[j,i]=1
    }
  }
}


shapiro_log <- matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro_log[i] <- shapiro.test(log(train_log_tran[,i]))$statistic
}


shapiro_sqrt <-  matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro_sqrt[i] <- shapiro.test(sqrt(raw_data_funded_train[,i]))$statistic
}

shapiro_boxcox <- matrix(NA, nrow=60, ncol=1)
for (i in not_factor_index){
  shapiro_boxcox[i] <- shapiro.test(raw_data_funded_train_trans[,i])$statistic
}




shapiro_tests <- cbind(shapiro,shapiro_log,shapiro_sqrt,shapiro_boxcox)
row.names(shapiro_tests) <- colnames(raw_data_train[1:60])
colnames(shapiro_tests) <- c("No-Tran","Log","Sqrt","BoxCox")

best <- apply(shapiro_tests,1, function(x) which.max(x))
best <- data.frame(matrix(best))
best$value <- as.numeric(best$matrix.best.)
best_names <- matrix(NA, nrow=60, ncol=1)
for (i in 1:60){
  value = best$value[i]
  best_names[i]  <- colnames(shapiro_tests)[value]
}
colnames(best_names) <- c("Best")

shapiro_tests <- cbind(shapiro_tests,best_names)
write.csv(shapiro_tests,"C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\Assignment4\\shapiro_test.csv")



log_index <- which(best_names=="Log")
sqrt_index <- which(best_names=="Sqrt")
boxcox_index <- which(best_names=="BoxCox")
notran_index <- which(best_names=="No-Tran")



#Distribution of Log Transformation for Funded Observations:
par(mfrow=c(1,2))
hist(raw_data_funded_train$FUND_AMOUNT, xlab="Fund Amount", main="Fund Amount Dist.")
hist(log(raw_data_funded_train$FUND_AMOUNT), xlab="Log Fund Amount",  main="Log Fund Amount Dist.")




##########################################################################################
###################################### MODELING ##########################################
##########################################################################################

####################################################################################
### Multivariate Regression:
hist(log(raw_data_funded_train$FUND_AMOUNT))
shapiro.test(log(raw_data_funded_train$FUND_AMOUNT))

lm_fit <- lm(log(FUND_AMOUNT)~.-COUNTY-ST_COUNTY,data=raw_data_funded_train)
vif(lm_fit)


#The lasso:
x_funded_train= model.matrix(~.+0,raw_data_funded_train[,-c(54,56,58,59)])
y_funded_train= log(raw_data_funded_train$FUND_AMOUNT)

lasso_cv_out <- cv.glmnet(x_funded_train,y_funded_train,alpha=1)
plot(lasso_cv_out)
which.min(lasso_cv_out$cvm)
best_lambda <- lasso_cv_out$lambda.min; best_lambda
small_lambda_index <- which(lasso_cv_out$lambda == lasso_cv_out$lambda.min)
small_lambda_betas <- data.frame(coef=lasso_cv_out$glmnet.fit$beta[, small_lambda_index])



x_cols <- colnames(x_funded_train)
x_test <- model.matrix(~.+0,raw_data_funded_test[,-c(54,56,58,59)])
x_valid <- model.matrix(~.+0,raw_data_funded_valid[,-c(54,56,58,59)])
lasso_pred <- predict(lasso_cv_out,s=best_lambda, newx=x_valid[,x_cols])
lasso_pred_test <- predict(lasso_cv_out,s=best_lambda, newx=x_test[,x_cols])

mean((lasso_pred-log(raw_data_funded_valid$FUND_AMOUNT))^2) #MSE: 0.7517744
sqrt(mean((lasso_pred-log(raw_data_funded_valid$FUND_AMOUNT))^2))  #RMSE: 0.8670492
cor(lasso_pred,log(raw_data_funded_valid$FUND_AMOUNT)) #Rsqed = 0.6477939 | 0.4148242

mean((lasso_pred_test-log(raw_data_funded_test$FUND_AMOUNT))^2) #MSE: 0.60801
sqrt(mean((lasso_pred_test-log(raw_data_funded_test$FUND_AMOUNT))^2))  #RMSE: 0.77975
cor(lasso_pred_test,log(raw_data_funded_test$FUND_AMOUNT)) #Rsqed = 0.7140209 | 0.4756742



# Polynomial Regression:
x_funded_train= model.matrix(~poly(SEVERE_HOUSING_PROBLEMS, 3) + 
                               poly(AVG_DAILY_PM25, 3) + 
                               poly(MAMMO_MEDICARE_ENROLLEES, 3) + 
                               poly(ANNUAL_VIOLENT_CRIMES, 3) + 
                               poly(PRIMARY_CARE_PHYSICIANS, 3) + 
                               poly(TEEN_BIRTHS, 3) + 
                               poly(PCT_FEMALE, 3) + 
                               poly(GRADUATION_RATE, 3) + 
                               poly(CHLAMYDIA_RATE, 3) + 
                               poly(PCT_DRIVE_ALONE, 3) + 
                               poly(POP_IN_VIOLATIONS, 3) + 
                               poly(PCT_SOME_COLLEGE, 3) + 
                               poly(LONG_COMMUTE_DRIVE_ALONE, 3) + 
                               poly(PCT_FAIR_POOR_HEALTH, 3) + 
                               poly(PCT_NO_SOCIAL_EMO_SUPPORT, 3) + 
                               poly(INJURY_DEATHS, 3) + 
                               poly(VIOLENT_CRIME_RATE, 3) + 
                               poly(HOMICIDE_RATE, 3) + 
                               0,
                             data=raw_data_funded_train[,-c(54,55,58,59)])
y_funded_train <- log(raw_data_funded_train$FUND_AMOUNT)

lasso_mod <- glmnet(x=x_funded_train,
                    y=y_funded_train,
                    alpha=1,thresh=1e-12)
lasso_cv_out <- cv.glmnet(x_funded_train,y_funded_train,alpha=1)
plot(lasso_cv_out)
which.min(lasso_cv_out$cvm)
best_lambda <- lasso_cv_out$lambda.min; best_lambda
lasso_coef <- coef(lasso_cv_out); lasso_coef
small_lambda_index <- which(lasso_cv_out$lambda == lasso_cv_out$lambda.min)
small_lambda_betas <- data.frame(coef=lasso_cv_out$glmnet.fit$beta[, 29])



x_cols <- colnames(x_funded_train)
x_test <- model.matrix(~poly(SEVERE_HOUSING_PROBLEMS, 3) + 
                         poly(AVG_DAILY_PM25, 3) + 
                         poly(MAMMO_MEDICARE_ENROLLEES, 3) + 
                         poly(ANNUAL_VIOLENT_CRIMES, 3) + 
                         poly(PRIMARY_CARE_PHYSICIANS, 3) + 
                         poly(TEEN_BIRTHS, 3) + 
                         poly(PCT_FEMALE, 3) + 
                         poly(GRADUATION_RATE, 3) + 
                         poly(CHLAMYDIA_RATE, 3) + 
                         poly(PCT_DRIVE_ALONE, 3) + 
                         poly(POP_IN_VIOLATIONS, 3) + 
                         poly(PCT_SOME_COLLEGE, 3) + 
                         poly(LONG_COMMUTE_DRIVE_ALONE, 3) + 
                         poly(PCT_FAIR_POOR_HEALTH, 3) + 
                         poly(PCT_NO_SOCIAL_EMO_SUPPORT, 3) + 
                         poly(INJURY_DEATHS, 3) + 
                         poly(VIOLENT_CRIME_RATE, 3) + 
                         poly(HOMICIDE_RATE, 3) + 
                         0,data=raw_data_funded_test)

x_valid <- model.matrix(~poly(SEVERE_HOUSING_PROBLEMS, 3) + 
                          poly(AVG_DAILY_PM25, 3) + 
                          poly(MAMMO_MEDICARE_ENROLLEES, 3) + 
                          poly(ANNUAL_VIOLENT_CRIMES, 3) + 
                          poly(PRIMARY_CARE_PHYSICIANS, 3) + 
                          poly(TEEN_BIRTHS, 3) + 
                          poly(PCT_FEMALE, 3) + 
                          poly(GRADUATION_RATE, 3) + 
                          poly(CHLAMYDIA_RATE, 3) + 
                          poly(PCT_DRIVE_ALONE, 3) + 
                          poly(POP_IN_VIOLATIONS, 3) + 
                          poly(PCT_SOME_COLLEGE, 3) + 
                          poly(LONG_COMMUTE_DRIVE_ALONE, 3) + 
                          poly(PCT_FAIR_POOR_HEALTH, 3) + 
                          poly(PCT_NO_SOCIAL_EMO_SUPPORT, 3) + 
                          poly(INJURY_DEATHS, 3) + 
                          poly(VIOLENT_CRIME_RATE, 3) + 
                          poly(HOMICIDE_RATE, 3) + 
                          0, data=raw_data_funded_valid)
lasso_pred_valid <- predict(lasso_mod,s=best_lambda, newx=x_valid[,x_cols])
lasso_pred_test <- predict(lasso_mod,s=best_lambda, newx=x_test[,x_cols])

mean((lasso_pred_valid-log(raw_data_funded_valid$FUND_AMOUNT))^2) #MSE: 0.8409955
sqrt(mean((lasso_pred_valid-log(raw_data_funded_valid$FUND_AMOUNT))^2))  #RMSE: 0.9170581
cor(lasso_pred_valid,log(raw_data_funded_valid$FUND_AMOUNT)) #Rsqed = 0.6440286

mean((lasso_pred_test-log(raw_data_funded_test$FUND_AMOUNT))^2) #MSE: 0.953947
sqrt(mean((lasso_pred_test-log(raw_data_funded_test$FUND_AMOUNT))^2))  #RMSE: 0.9767021
cor(lasso_pred_test,log(raw_data_funded_test$FUND_AMOUNT)) #Rsqed = 0.6182641 | 0.4589227





#######################################################################################
### Random Forest ###
mygrid <- expand.grid(.mtry=1:16)
rf_tune <- caret::train(log(FUND_AMOUNT)~.-COUNTY-ST_COUNTY,
                        data=raw_data_funded_train,
                        tuneGrid=mygrid,
                        method="rf")

rf_fit <- randomForest(log(FUND_AMOUNT)~.-COUNTY-ST_COUNTY,data=raw_data_funded_train,mtry=4,importance=TRUE)

library(reprtree)
reprtree:::plot.getTree(rf_fit, labelVar=TRUE)
getTree(rf_fit)


reptree <- ReprTree(rf_fit, raw_data_funded_train, metric='d2')
plot(reptree, index=1)



#Creating an important variable bar plot:
rf_importance <- data.frame(imp=rf_fit$importance[,2])
rf_importance$var_names <- row.names(rf_importance)
rf_importance <- rf_importance[order(-rf_importance$imp),]
rf_importance %>% filter(imp>1.5) %>%
  ggplot(aes(x=reorder(var_names,imp),y=imp)) + geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Random Forest Variable Importance",
       x="Variables", y="Importance") + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification = "center") 


# valid set
rf_pred <- predict(rf_fit,newdata=raw_data_funded_valid,type="response")
mean((rf_pred-log(raw_data_funded_valid$FUND_AMOUNT))^2)  #MSE: 0.712788
sqrt(mean((rf_pred-log(raw_data_funded_valid$FUND_AMOUNT))^2)) # RMSE: 0.8442677
cor(rf_pred,log(raw_data_funded_valid$FUND_AMOUNT))^2 #Rsqed = 0.6626611

# test set
rf_pred <- predict(rf_fit,newdata=raw_data_funded_test,type="response")
mean((rf_pred-log(raw_data_funded_test$FUND_AMOUNT))^2) #MSE: 0.6408126
sqrt(mean((rf_pred-log(raw_data_funded_test$FUND_AMOUNT))^2))  #RMSE: 0.8005077
cor(rf_pred,log(raw_data_funded_test$FUND_AMOUNT)) #Rsqed = 0.6919794



#######################################################################################
### Gradient Boost Descend ###
par(mfrow=c(1,1))

gbm_matrix <- matrix(NA,ncol=3,nrow=16)
for (i in 1:16){
  set.seed(1)
  gbm_fit_cv <- gbm(log(FUND_AMOUNT)~.,
                    data=raw_data_funded_train[,-c(54,59)],
                    distribution="gaussian",
                    n.trees=5000,
                    shrinkage=0.001,
                    interaction.depth=i,
                    cv.folds=10)
  gbm_best_cv <- gbm.perf(gbm_fit_cv,method="cv",oobag.curve=TRUE)  
  gbm_pred <- predict(gbm_fit_cv,newdata=raw_data_funded_valid,n.trees=gbm_best_cv)
  gbm_rmse <- sqrt(mean((gbm_pred-log(raw_data_funded_valid$FUND_AMOUNT))^2))
  gbm_rsq <- cor(gbm_pred,log(raw_data_funded_valid$FUND_AMOUNT))
  gbm_matrix[i,1] <- gbm_best_cv
  gbm_matrix[i,2] <- gbm_rmse
  gbm_matrix[i,3] <- gbm_rsq
  print(paste("Iteration",i,"Done!"))
}

# Standardized Version
gbm_matrix_std <- matrix(NA,ncol=3,nrow=16)
for (i in 1:16){
  set.seed(1)
  gbm_fit_cv <- gbm(log(FUND_AMOUNT)~.,
                    data=raw_data_funded_std_train[,-c(54,59)],
                    distribution="gaussian",
                    n.trees=5000,
                    shrinkage=0.001,
                    interaction.depth=i,
                    cv.folds=10)
  gbm_best_cv <- gbm.perf(gbm_fit_cv,method="cv",oobag.curve=TRUE)  
  gbm_pred <- predict(gbm_fit_cv,newdata=raw_data_funded_std_valid,n.trees=gbm_best_cv)
  gbm_rmse <- sqrt(mean((gbm_pred-log(raw_data_funded_std_valid$FUND_AMOUNT))^2))
  gbm_rsq <- cor(gbm_pred,log(raw_data_funded_std_valid$FUND_AMOUNT))
  gbm_matrix_std[i,1] <- gbm_best_cv
  gbm_matrix_std[i,2] <- gbm_rmse
  gbm_matrix_std[i,3] <- gbm_rsq
  print(paste("Iteration",i,"Done!"))
}


gbm_fit   <- gbm(log(FUND_AMOUNT)~.,
                 data=raw_data_funded_train[,-c(54,59)],
                 distribution="gaussian",
                 n.trees=1000,
                 shrinkage=0.01,
                 interaction.depth=3)

# valid set
gbm_pred <- predict(gbm_fit, newdata=raw_data_funded_valid, n.trees=223)
mean((gbm_pred - log(raw_data_funded_valid$FUND_AMOUNT))^2)   #MSE: 0.7634177
sqrt(mean((gbm_pred - log(raw_data_funded_valid$FUND_AMOUNT))^2)) # RMSE: 0.8737378
cor(gbm_pred,log(raw_data_funded_valid$FUND_AMOUNT)) #0.6213857

# test set
gbm_test <- predict(gbm_fit, new_data=raw_data_funded_test, n.trees=274) 
mean((gbm_pred - log(raw_data_funded_test$FUND_AMOUNT))^2) # MSE: 1.401837
sqrt(mean((gbm_pred - log(raw_data_funded_test$FUND_AMOUNT))^2)) # RMSE: 1.183992
cor(gbm_pred,log(raw_data_funded_test$FUND_AMOUNT)) #0.1600924


#######################################################################################

### GAMS with Regularization of splines ###
gam1 <- gam(log(FUND_AMOUNT)~ 
              s(CHILDREN_IN_POVERTY, bs='ts') + 
              s(ANNUAL_VIOLENT_CRIMES, bs='ts') + 
              s(HOMICIDE_RATE, bs='ts') + 
              s(PCT_RURAL, bs='ts') +
              s(CHLAMYDIA_RATE, bs='ts') + 
              s(PCT_NO_SOCIAL_EMO_SUPPORT, bs='ts') +
              s(INJURY_DEATHS, bs='ts'),
            data=raw_data_funded_train,method="REML",select=TRUE)
summary(gam1)  #Adj R-squared 0.62
coef_df <- data.frame(round(coef(gam1),3)) ; coef_df
par(mfrow=c(2,2))
plot(gam1)
par(mfrow=c(1,1))


# 3-Fold Validation on the GAM Splines Model:
k_random <- sample(1:3,nrow(raw_data_funded_train),rep=TRUE)

gam_3fold <- matrix(NA,ncol=3,nrow=3)
colnames(gam_3fold) <- c("MSE","RMSE","Rsquared")
for (i in 1:3){
  gam1 <- gam(log(FUND_AMOUNT)~ 
                s(CHILDREN_IN_POVERTY, bs='ts') + 
                s(ANNUAL_VIOLENT_CRIMES, bs='ts') + 
                s(HOMICIDE_RATE, bs='ts') + 
                s(PCT_RURAL, bs='ts') +
                s(CHLAMYDIA_RATE, bs='ts') + 
                s(PCT_NO_SOCIAL_EMO_SUPPORT, bs='ts') +
                s(INJURY_DEATHS, bs='ts'),
              data=raw_data_funded_train[k_random!=i,],method="REML",select=TRUE)
  
  gam_pred <- predict(gam1, newdata=raw_data_funded_train[k_random==i,])
  gam_mse  <- mean((gam_pred - log(raw_data_funded_train$FUND_AMOUNT[k_random==i]))^2)  
  gam_rmse <- sqrt(mean((gam_pred - log(raw_data_funded_train$FUND_AMOUNT[k_random==i]))^2))
  gam_cor  <- cor(gam_pred, log(raw_data_funded_train$FUND_AMOUNT[k_random==i])) 
  
  gam_3fold[i,1] <- gam_mse
  gam_3fold[i,2] <- gam_rmse
  gam_3fold[i,3] <- gam_cor
}

# 3-Fold CV Figures:
# MSE = 0.6276223
# RMSE = 0.789648
# Rsqured = 0.7311518


gam_pred <- predict(gam1, newdata=raw_data_funded_valid)
mean((gam_pred - log(raw_data_funded_valid$FUND_AMOUNT))^2)  #1.163417
sqrt(mean((gam_pred - log(raw_data_funded_valid$FUND_AMOUNT))^2)) #1.078618
cor(gam_pred, log(raw_data_funded_valid$FUND_AMOUNT))  # 0.3819156

gam_pred <- predict(gam1, newdata=raw_data_funded_test) 
mean((gam_pred - log(raw_data_funded_test$FUND_AMOUNT))^2)  #0.8917597
sqrt(mean((gam_pred - log(raw_data_funded_test$FUND_AMOUNT))^2))  #0.9443303
cor(gam_pred, log(raw_data_funded_test$FUND_AMOUNT))  # 0.5511536