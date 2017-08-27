library(caret)
library(ROCR)
library(RCurl)
library(MASS)
library(ROSE)
library(klaR)
library(pROC)
library(gbm)
library(doParallel)
library(e1071)
library(ggplot2)
library(ltm)
library(plyr)

##IMPORT DATA
setwd("C:/Users/Alysssa/Desktop/MSPA/Capstone")
x <- getURL("https://raw.githubusercontent.com/NorthwesternDataScience/Team1-Capstone/master/data_sca_imp_selectvar_funding.csv")
raw_sca <- read.csv(text = x)
raw_sca$FUNDING <- ifelse(raw_sca$FUND_AMOUNT > 0, 1, 0)
raw_sca <-raw_sca[ , !(names(raw_sca) %in% c('State','COUNTY', 'STATE_SHORT', 'X', 'FUND_AMOUNT'))]

x <- getURL("https://raw.githubusercontent.com/NorthwesternDataScience/Team1-Capstone/master/data_imp_selectvar_funding.csv")
raw_data <- read.csv(text = x)
raw_data$FUNDING <- ifelse(raw_data$FUND_AMOUNT > 0, 1, 0)
raw_data <-raw_data[ , !(names(raw_data) %in% c('State','COUNTY', 'STATE_SHORT', 'X', 'FUND_AMOUNT'))]

##SPLIT DATA
set.seed(54321)
train <- sample(nrow(raw_data),round(nrow(raw_data)*0.6),replace=FALSE)
raw_data_train <- raw_data[train,]
remaining <- raw_data[-train,]
val <- sample(nrow(remaining),round(nrow(remaining)*0.5),replace=FALSE)
raw_data_val <- remaining[val,]
test <- remaining[-val,]
train <- raw_data_train
val <- raw_data_val

##BALANCE TRAINING SET
set.seed(1234)
data.rose <- ROSE(FUNDING ~ ., data = train, seed = 1)$data

##LOG TRANSFORMATIONS FOR LDA
log_train <- log(train+1)
log_val <- log(val+1)
log_test <- log(test+1)
log_rose <- ROSE(FUNDING ~ ., data = log_train, seed = 1)$data
log_sca <- log(abs(raw_sca+1))

###BISERIAL CORRELATIONS
bi_cor <- function(x) {
  biserial.cor(x=x, y=log_raw$FUNDING, use = "complete.obs", level = 1)
}
cors <- apply(pred_vars, 2, bi_cor)
cors <- data.frame(cors)
write.csv(cors, file = "cors.csv")

###COMPARISON OF MEANS BY FUNDING INDICATOR 
mns = aggregate(log_sca,log_sca['FUNDING'],mean)
rownames(mns) = mns$FUNDING
mns$FUNDING = NULL
op <- par(mar=c(11,3,2,2))
barplot(as.matrix(mns),beside=TRUE,cex.names=.6,las=2, col=c("grey87", "purple4"),main="Variable Means by Funding Indicator")
legend("bottomright", fill=c("grey87", "purple4"), c("N","Y"))
rm(op)

###DENSITY PLOTS
ggplot(log_raw, aes(x = VIOLENT_CRIME_RATE, fill = FUNDING)) + scale_fill_manual( values = c("dimgrey", "purple4")) + geom_density(alpha = 0.5) + labs(x="VIOLENT CRIME RATE")
ggplot(log_raw, aes(x = HOMICIDE_RATE, fill = FUNDING)) + scale_fill_manual( values = c("dimgrey", "purple4")) + geom_density(alpha = 0.5) + labs(x="HOMICIDE RATE")
ggplot(log_raw, aes(x = DRUG_POISONING_MORTALITY_RATE, fill = FUNDING)) + scale_fill_manual( values = c("dimgrey", "purple4")) + geom_density(alpha = 0.5)  + labs(x="DRUG POISONING MORTALITY RATE")
ggplot(log_raw, aes(x = PCT_ALC_IMPAIRED, fill = FUNDING))+ scale_fill_manual( values = c("dimgrey", "purple4")) + geom_density(alpha = 0.5) + labs(x="ALCOHOL RELATED VEHICLE DEATH RATE")

#########################################
#    LOGISTIC MODEL
#########################################
upper.fit=glm(FUNDING ~ ., family=binomial(link='logit'), data=data.rose)
lower.fit=glm(FUNDING ~ 1, family=binomial(link='logit'), data=data.rose)
stepwise.fit <- stepAIC(upper.fit,lower.fit,trace=T)
summary(stepwise.fit)
glm.fit=glm(FUNDING ~ SEVERE_HOUSING_PROBLEMS+DRUG_POISONING_MORTALITY_RATE+LIMITED_TO_HEALTHYFOOD+PCT_UNEMPLOYED+PCT_RURAL+HIV_CASES+AMERICAN_INDIAN+PRIMARY_CARE_PHYSICIANS+MOTOR_VEHICLE_MORTALITY_RATE+AGE_65_AND_GREATER+PCT_ALC_IMPAIRED+PCT_SOME_COLLEGE+PCT_FAIR_POOR_HEALTH+VIOLENT_CRIME_RATE, family=binomial(link='logit'), data=data.rose)
summary(glm.fit)

odds_ratio<- data.frame(exp(coef(glm.fit)))

p <- predict(glm.fit, newdata=data.rose, type="response")
pr <- prediction(p, data.rose$FUNDING)
log_prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

log_auc <- performance(pr, measure = "auc")
log_auc <- auc@y.values[[1]]

#########################################
#   LDA MODEL
#########################################
log_raw<- log(abs(raw_data+1))
log_raw$FUNDING <- factor(log_raw$FUNDING)
levels(log_raw$FUNDING) <- c("N", "Y")

log_rose$FUNDING <- factor(log_rose$FUNDING)
levels(log_rose$FUNDING) <- c("N", "Y")

log_train <- log(train+1)
log_val <- log(val+1)
log_test <- log(test+1)
log_rose <- ROSE(FUNDING ~ ., data = log_train, seed = 1)$data
log_sca <- log(abs(raw_sca+1))

filterVarImp(log_rose[,-56], log_rose$FUNDING)

ctrl <- trainControl(method = "repeatedcv",   
                     number = 5,						
                     summaryFunction=twoClassSummary,	
                     classProbs=TRUE)

objModel <- train(FUNDING~., data=log_rose, method='lda',  metric = "ROC", preProc = c("center", "scale"), trControl = ctrl)

vars <- varImp(objModel,scale=F)

plot(varImp(objModel,scale=F), main="LDA ROC Variable Importance", top=20)

lda_varimp <- lda(FUNDING~SEVERE_HOUSING_PROBLEMS+PRIMARY_CARE_PHYSICIANS+ANNUAL_VIOLENT_CRIMES+LIMITED_TO_HEALTHYFOOD+SOME_COLLEGE+MAMMO_MEDICARE_ENROLLEES+DRUG_POISONING_DEATHS+CHLAMYDIA_CASES+UNEMPLOYED+UNINSURED_CHILDREN+SOME_COLLEGE_POPULATION+CHILD_TOTAL_DEATHS+INJURY_DEATHS+PCT_RURAL+TEEN_BIRTHS+CHILDREN_IN_POVERTY+ASIAN+MOTOR_VEHICLE_MORTALITY_RATE, data=log_rose)

p <- predict(lda_varimp,log_rose)$posterior[,2]
pr <- prediction(p, log_rose$FUNDING)
lda_prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#########################################
#   GBM Model
#########################################
data.rose$FUNDING <- factor(data.rose$FUNDING)
levels(data.rose$FUNDING) <- c("N", "Y")

val$FUNDING <- factor(val$FUNDING)
levels(val$FUNDING) <- c("N", "Y")

ctrl <- trainControl(method = "repeatedcv",   
                     number = 5,						
                     summaryFunction=twoClassSummary,	
                     classProbs=TRUE,
                     allowParallel = TRUE)


grid <- expand.grid(interaction.depth=c(1,2,3),
                    n.trees=c(500,1000,5000),	        
                    shrinkage=c(0.001, 0.01,0.1),
                    n.minobsinnode = 10)

set.seed(1951)
registerDoParallel(4)
getDoParWorkers()

gbm.tune <- train(FUNDING ~ ., data=data.rose,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid,
                  verbose=FALSE)

gbm.tune$bestTune
plot(gbm.tune)  		
res <- gbm.tune$results
res

summary(gbm.tune$bestTune)

gbmimp <- varImp(gbm.tune, scale=FALSE)

plot(gbmimp, main="GBM Overall Variable Importance",top=20)

##Confusion Matrix
gbm.pred <- predict(gbm.tune,data.rose)
confusionMatrix(gbm.pred,data.rose$FUNDING)  

##ROC CURVE
gbm.probs <- predict(gbm.tune,data.rose,type="prob")
gbm.ROC <- roc(predictor=gbm.probs$N,
               response=data.rose$FUNDING,
               levels=rev(levels(data.rose$FUNDING)))
gbm.ROC$auc
plot(gbm.ROC,main="GBM ROC")

p <- predict(gbm.tune,val,type="raw")
p_val <- ifelse(p=="N",0,1)
pr <- prediction(p_val, val$FUNDING)
gbm_prf <- performance(pr, measure = "tpr", x.measure = "fpr")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

##Comparative ROC Chart
plot(log_prf, main="Comparative ROC Chart", xlab="False Positive Rate", ylab="TPR", col="black", lwd=1.5)
plot(lda_prf, add = TRUE, col="purple4", lwd=1.5)
plot(gbm_prf, add = TRUE, col="dimgrey", lwd=1.5)
legend("bottomright", c("Logistic (AUC: 0.812)", "LDA (AUC: 0.828)", "GBM (AUC: 0.599)"), lty=c(1,1,1,1), col=c("black", "purple4", "dimgrey"))
