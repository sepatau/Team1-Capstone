#Capstone Project#
#Crime Data#

library(xlsx)
library(VIM)
library(plyr)

files <- list.files(path="C:\\Users\\Max\\Documents\\Northwestern\\PRED CHR 498\\2014 Data")
dir = "C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\2014 CHR Data\\"


#Inializing crime_df with first file - Alabama:
measure_data <- read.xlsx("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\2014 CHR Data\\2014 County Health Rankings Alabama Data - v6.xls", sheetName="Ranked Measure Data",startRow=2)
measure_data$County <- as.character(measure_data$County)
measure_data$County[1] <- "STATE_RECORD"
measure_data$County <- as.factor(measure_data$County)
col_to_remove <- grep("X95|Z.Score",names(measure_data))
measure_data_clean <- measure_data[,-col_to_remove]
crime_df <- measure_data_clean
empty_row <- dim(crime_df)[1]
crime_df <- crime_df[-empty_row,]


for (i in 11:51){
  file = paste(dir,files[i],sep="")
  print(files[i])

  measure_data <- read.xlsx(file, sheetName="Ranked Measure Data", startRow=2)
  measure_data$County <- as.character(measure_data$County)
  measure_data$County[1] <- "STATE_RECORD"
  measure_data$County <- as.factor(measure_data$County)
  
  col_to_remove <- grep("X95|Z.Score",names(measure_data))
  measure_data_clean <- measure_data[,-col_to_remove]                      #Removing Z.Scores and 95%
  dupe_col_to_remove <- grep("X..With.Access.1",names(measure_data_clean)) #Arizon had a correction not in others.
  
  if (length(dupe_col_to_remove) > 0){
    measure_data_clean <- measure_data_clean[,-dupe_col_to_remove]
  }

  empty_row <- dim(measure_data_clean)[1]
  measure_data_clean <- measure_data_clean[-empty_row,]
  crime_df <- rbind(crime_df,measure_data_clean)
  print("COMPLETED!") 
}


colnames(crime_df) <- c('FIPS','STATE','COUNTY','PREMATURE_DEATHS','PREMATURE_YPLL_RATE',
                        'POOR_FAIR_HEALTH_SAMPLE_SIZE','PCT_FAIR_POOR_HEALTH','POOR_PHYSICAL_SAMPLE_SIZE',
                        'POOR_PHYSICAL_UNHEALTHY_DAYS','POOR_MENTAL_SAMPLE_SIZE','POOR_MENTAL_UNHEALTHY_DAYS',
                        'UNRELIABLE','LBW_BIRTHS','LIVE_BIRTHS','PCT_LBW','SMOKING_SAMPLE_SIZE','PCT_SMOKERS',
                        'PCT_OBESE','FOOD_ENVIR_INDEX','PCT_PHYSICALLY_INACTIVE','PCT_WITH_ACCESS',
                        'DRINKING_SAMPLE_SIZE','PCT_EXCESSIVE_DRINKING','ALC_IMPAIRED_DRIVING_DEATHS',
                        'DRIVING_DEATHS','PCT_ALC_IMPAIRED','CHLAMYDIA_CASES','CHLAMYDIA_RATE','TEEN_BIRTHS',
                        'TEEN_POPULATION','TEEN_BIRTH_RATE','UNINSURED','PCT_UNINSURED',
                        'PRIMARY_CARE_PHYSICIANS','PRIMARY_CARE_PHYS_RATE','PRIMARY_CARE_PHYS_RATIO',
                        'DENTISTS','DENTISTS_RATE','DENTISTS_RATIO','DENTISTS_PREV','DENTISTS_RATE_PREV',
                        'DENTISTS_RATIO_PREV','MHP','MHP_RATE','MHP_RATIO','MHP_PREV','MHP_RATE_PREV',
                        'MHP_RATIO_PREV','MEDICARE_ENROLLEES','ACSC_RATE','DIABETICS','PCT_HBA1C',
                        'MAMMO_MEDICARE_ENROLLEES','PCT_MAMMOGRAPHY','COHORT_SIZE','GRADUATION_RATE',
                        'SOME_COLLEGE','SOME_COLLEGE_POPULATION','PCT_SOME_COLLEGE','UNEMPLOYED',
                        'LABOR_FORCE','PCT_UNEMPLOYED','CHILDREN_IN_POVERTY','PCT_CHILDREN_IN_POVERTY',
                        'SOCIAL_EMOTIONAL_SAMPLE_SIZE','PCT_NO_SOCIAL_EMO_SUPPORT','SINGLE_PARENT_HOUSEHOLDS',
                        'HOUSEHOLDS','PCT_SINGLE_PARENT_HOUSEHOLDS','ANNUAL_VIOLENT_CRIMES',
                        'VIOLENT_CRIME_RATE','INJURY_DEATHS','INJURY_DEATHS_RATE','AVG_DAILY_PM25',
                        'PCT_POP_VIOLATIONS','POP_IN_VIOLATIONS','SEVERE_HOUSING_PROBLEMS',
                        'PCT_SEVERE_HOUSING_PROBLEMS','DRIVE_ALONE','WORKERS','PCT_DRIVE_ALONE',
                        'WORKERS_DRIVE_ALONE','LONG_COMMUTE_DRIVE_ALONE')

write.csv(crime_df,file="C:\\Users\\Max\\Documents\\Northwestern\\PRED CHR 498\\Ranked Measure Data")



############## 'Additional Measure Data' Tab #################

#Initializing additional_data_df from first file - Alabama:
additional_data <- read.xlsx("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\2014 CHR Data\\2014 County Health Rankings Alabama Data - v6.xls", sheetName="Additional Measure Data",startRow=2)
additional_data$County <- as.character(additional_data$County)
additional_data$County[1] <- "STATE_RECORD"
additional_data$County <- as.factor(additional_data$County)
col_to_remove <- grep("X95|Z.Score",names(additional_data))
additional_data_clean <- additional_data[,-col_to_remove]
empty_row <- dim(additional_data_clean)[1]
##### New DataFrame - additional_data_df #####
additional_data_df <- additional_data_clean[-empty_row,]

for (i in 6:51){
  file = paste(dir,files[i],sep="")
  print(files[i])
  
  additional_data <- read.xlsx(file, sheetName="Additional Measure Data", startRow=2)
  additional_data$County <- as.character(additional_data$County)
  additional_data$County[1] <- "STATE_RECORD"
  additional_data$County <- as.factor(additional_data$County)
  
  col_to_remove <- grep("X95|Z.Score",names(additional_data))
  additional_data_clean <- additional_data[,-col_to_remove]    #Removing Z.Scores and 95%

  empty_row <- dim(additional_data_clean)[1]
  additional_data_clean <- additional_data_clean[-empty_row,]
  
  additional_data_df <- rbind(additional_data_df,additional_data_clean)
  
  print("COMPLETED!") 
}

empty_col <- dim(additional_data_df)[2]
additional_data_df <- additional_data_df[,-empty_col]


colnames(additional_data_df) <- c('FIPS','STATE','COUNTY','POPULATION','AGE_LESS_THAN_18',
                                  'AGE_65_AND_GREATER','AFRICAN_AMERICAN','AMERICAN_INDIAN',
                                  'ASIAN','NATIVE_HAWAIIAN','HISPANIC','NON_HISPANIC',
                                  'NOT_PROFICIENT_IN_ENGLISH','PCT_NOT_PROFICIENT_IN_ENGLISH',
                                  'PCT_FEMALE','PCT_RURAL','PCT_DIABETIC','HIV_CASES','HIV_RATE',
                                  'PREMATURE_TOTAL_DEATHS','PREMATURE_AGE_ADJ_MORTALITY',
                                  'INFANT_MORTALITY_RATE','CHILD_TOTAL_DEATHS','CHILD MORTALITY RATE',
                                  'PCT_FOOD_INSECURITY','LIMITED_TO_HEALTHYFOOD',
                                  'PCT_LIMITED_TO_HEALTHYFOOD','MOTOR_VEHICLE_DEATHS',
                                  'MOTOR VEHICLE_MORTALITY_RATE','DRUG_POISONING_DEATHS',
                                  'DRUG_POISONING_MORTALITY_RATE','UNINSURED_ADULTS',
                                  'PCT_UNINSURED_ADULTS','UNINSURED_CHILDREN','PCT_UNINSURED_CHILDREN',
                                  'HEALTH_CARE_COSTS','DOCTOR_COST_HIGH_SAMPLE_SIZE',
                                  'PCT_DOCTOR_COST_COULDNT_ACCESS','OTHER_PCP_RATE','OTHER_PCP_RATIO',
                                  'OTHER_PCP_RATE_PREV','OTHER_PCP_RATIO_PREV','HOUSEHOLD_INCOME',
                                  'PCT_CHILDREN_FREE_LUNCH','HOMICIDE_RATE')

write.csv(additional_data_df,file="C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\Additional_Data.csv")
additional_data_df[1:3] <- NULL


county_health_crime_data <- cbind(crime_df,additional_data_df)
write.csv(county_health_crime_data,file="C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\county_health_crime_data.csv", row.names = FALSE)



###############################################################################################################
########################## LOADING BACK 2014_County_Health_Crime_Data #########################################
###############################################################################################################

# Load CSV File:
county_health_crime_data <- read.csv("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\2014_county_health_crime_data.csv", header = TRUE, na.strings = c("", NA))






###############################################################################################################
########################## LOADING & MERGING DOJ's FUNDING AMOUNTS ############################################
###############################################################################################################

doj_df <- read.csv("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\Project Docs\\FIPS_AMOUNTS.csv")
doj_df <- doj_df[order(doj_df$FIPS),]
doj_county_df$AMOUNT <- ifelse(is.na(doj_county_df$AMOUNT),0,doj_county_df$AMOUNT)


all_data <- merge(x=county_health_crime_data, y=doj_df, by="FIPS", all.x=TRUE) 
all_data$FUND_AMOUNT <- ifelse(is.na(all_data$AMOUNT),0,all_data$AMOUNT)
all_data$AMOUNT <- NULL


# Loading State Acronyms
state_short <- read.csv("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\States_Shorts.csv")
all_data$STATE <- toupper(all_data$STATE)
all_data$COUNTY <- toupper(all_data$COUNTY)
all_data <- merge(x=all_data, y=state_short, by="STATE", all.x=TRUE)


# Loading State Fund Amounts
all_county_data <- all_data[-state_records,]
all_state_data <- all_data[state_records,]


state_fund_amounts <- read.csv("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\Project Docs\\STATE_FUND_AMOUNT.csv")
state_records <- which(all_data$COUNTY=="STATE_RECORD")
all_state_data <- merge(x=all_state_data, y=state_fund_amounts, by.x="STATE_SHORT", by.y="STATE", all.x=TRUE)
all_state_data$FUND_AMOUNT <- all_state_data$AMOUNT
all_state_data$AMOUNT <- NULL
all_state_data$FUND_AMOUNT <- ifelse(is.na(all_state_data$FUND_AMOUNT),0,all_state_data$FUND_AMOUNT)

all_county_data$FUNDED_IND <- ifelse(all_county_data$FUND_AMOUNT>0,1,0)
all_state_data$FUNDED_IND <- ifelse(all_state_data$FUND_AMOUNT>0,1,0)

var_names <- c('STATE','FIPS','COUNTY','PREMATURE_DEATHS','PREMATURE_YPLL_RATE','POOR_FAIR_HEALTH_SAMPLE_SIZE',
               'PCT_FAIR_POOR_HEALTH','POOR_PHYSICAL_SAMPLE_SIZE','POOR_PHYSICAL_UNHEALTHY_DAYS',
               'POOR_MENTAL_SAMPLE_SIZE','POOR_MENTAL_UNHEALTHY_DAYS','UNRELIABLE','LBW_BIRTHS',
               'LIVE_BIRTHS','PCT_LBW','SMOKING_SAMPLE_SIZE','PCT_SMOKERS','PCT_OBESE','FOOD_ENVIR_INDEX',
               'PCT_PHYSICALLY_INACTIVE','PCT_WITH_ACCESS','DRINKING_SAMPLE_SIZE','PCT_EXCESSIVE_DRINKING',
               'ALC_IMPAIRED_DRIVING_DEATHS','DRIVING_DEATHS','PCT_ALC_IMPAIRED','CHLAMYDIA_CASES',
               'CHLAMYDIA_RATE','TEEN_BIRTHS','TEEN_POPULATION','TEEN_BIRTH_RATE','UNINSURED','PCT_UNINSURED',
               'PRIMARY_CARE_PHYSICIANS','PRIMARY_CARE_PHYS_RATE','PRIMARY_CARE_PHYS_RATIO','DENTISTS',
               'DENTISTS_RATE','DENTISTS_RATIO','DENTISTS_PREV','DENTISTS_RATE_PREV','DENTISTS_RATIO_PREV',
               'MHP','MHP_RATE','MHP_RATIO','MHP_PREV','MHP_RATE_PREV','MHP_RATIO_PREV','MEDICARE_ENROLLEES',
               'ACSC_RATE','DIABETICS','PCT_HBA1C','MAMMO_MEDICARE_ENROLLEES','PCT_MAMMOGRAPHY','COHORT_SIZE',
               'GRADUATION_RATE','SOME_COLLEGE','SOME_COLLEGE_POPULATION','PCT_SOME_COLLEGE','UNEMPLOYED',
               'LABOR_FORCE','PCT_UNEMPLOYED','CHILDREN_IN_POVERTY','PCT_CHILDREN_IN_POVERTY',
               'SOCIAL_EMOTIONAL_SAMPLE_SIZE','PCT_NO_SOCIAL_EMO_SUPPORT','SINGLE_PARENT_HOUSEHOLDS',
               'HOUSEHOLDS','PCT_SINGLE_PARENT_HOUSEHOLDS','ANNUAL_VIOLENT_CRIMES','VIOLENT_CRIME_RATE',
               'INJURY_DEATHS','INJURY_DEATHS_RATE','AVG_DAILY_PM25','PCT_POP_VIOLATIONS','POP_IN_VIOLATIONS',
               'SEVERE_HOUSING_PROBLEMS','PCT_SEVERE_HOUSING_PROBLEMS','DRIVE_ALONE','WORKERS','PCT_DRIVE_ALONE',
               'WORKERS_DRIVE_ALONE','LONG_COMMUTE_DRIVE_ALONE','POPULATION','AGE_LESS_THAN_18','AGE_65_AND_GREATER',
               'AFRICAN_AMERICAN','AMERICAN_INDIAN','ASIAN','NATIVE_HAWAIIAN','HISPANIC','NON_HISPANIC',
               'NOT_PROFICIENT_IN_ENGLISH','PCT_NOT_PROFICIENT_IN_ENGLISH','PCT_FEMALE','PCT_RURAL','PCT_DIABETIC',
               'HIV_CASES','HIV_RATE','PREMATURE_TOTAL_DEATHS','PREMATURE_AGE_ADJ_MORTALITY','INFANT_MORTALITY_RATE',
               'CHILD_TOTAL_DEATHS','CHILD_MORTALITY_RATE','PCT_FOOD_INSECURITY','LIMITED_TO_HEALTHYFOOD',
               'PCT_LIMITED_TO_HEALTHYFOOD','MOTOR_VEHICLE_DEATHS','MOTOR_VEHICLE_MORTALITY_RATE','DRUG_POISONING_DEATHS',
               'DRUG_POISONING_MORTALITY_RATE','UNINSURED_ADULTS','PCT_UNINSURED_ADULTS','UNINSURED_CHILDREN',
               'PCT_UNINSURED_CHILDREN','HEALTH_CARE_COSTS','DOCTOR_COST_HIGH_SAMPLE_SIZE','PCT_DOCTOR_COST_COULDNT_ACCESS',
               'OTHER_PCP_RATE','OTHER_PCP_RATIO','OTHER_PCP_RATE_PREV','OTHER_PCP_RATIO_PREV','HOUSEHOLD_INCOME',
               'PCT_CHILDREN_FREE_LUNCH','HOMICIDE_RATE','FUND_AMOUNT','STATE_SHORT','FUNDED_IND')

colnames(all_county_data) <- var_names
colnames(all_state_data) <- var_names

all_county_data$STATE <- as.factor(all_county_data$STATE)
all_county_data$FIPS <- as.factor(all_county_data$FIPS)
all_county_data$COUNTY <- as.factor(all_county_data$COUNTY)
all_state_data$STATE <- as.factor(all_state_data$STATE)
all_state_data$FIPS <- as.factor(all_state_data$FIPS)
all_state_data$COUNTY <- as.factor(all_state_data$COUNTY)

write.csv(all_county_data, "C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\2014_all_county_data.csv", row.names=FALSE)
write.csv(all_state_data, "C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\2014_all_state_data.csv", row.names=FALSE)



