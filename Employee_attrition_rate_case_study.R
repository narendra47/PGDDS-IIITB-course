#################################################################################################################
##---------------------------           HR Case Study           -----------------------------------------------##
#################################################################################################################

#Install and Load the required packages

#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")


library(lubridate)
library(dplyr)
library(tidyr)
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(lattice)
library(caret)
library(GGally)
library(cowplot)
library(caTools)
library(ROCR)


## Loading all files for the case study
emp_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)  
mgr_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
emp_data <- read.csv("general_data.csv", stringsAsFactors = F)
emp_in_time <- read.csv("in_time.csv", stringsAsFactors = F)
emp_out_time <- read.csv("out_time.csv", stringsAsFactors = F)


##Looking at the structure of data 
str(emp_survey_data)
str(mgr_survey_data)
str(emp_data)
str(emp_in_time)
str(emp_out_time)

## Let's clean and extract some valuable info from employee daily checkin and checkout time stamp.
## Removing columns from data frame where all values in the column is NA, looks like these are holidays at office.
emp_in_time <- emp_in_time[,colSums(is.na(emp_in_time))<nrow(emp_in_time)]
emp_out_time <- emp_out_time[,colSums(is.na(emp_out_time))<nrow(emp_out_time)]

## Since emp_in_time and emp_out_time both capturing everyday checkin and checkout time stamp for the employee
## Let's take difference of these two to get how much time the employee spent in office

emp_daily_office_hours <- emp_in_time

for (i in 2:(length(emp_in_time))) {
  emp_daily_office_hours[,i] <- as.numeric(difftime(as.POSIXct(emp_out_time[,i], format = "%Y-%m-%d %H:%M:%S"), 
                                                    as.POSIXct(emp_in_time[,i], format = "%Y-%m-%d %H:%M:%S"),
                                                    units = "mins"))
}


## Creating a new column, which will store number of leave the employee had taken. I am considering all employee 
## are getting official holiday on same day, which we have already removed in previous steps.

emp_daily_office_hours$leave_taken <- 0

emp_daily_office_hours$leave_taken <- apply(is.na(emp_daily_office_hours[,2:250]), 1, sum)

## Creating another new column, which will store average minute stay in office of the employee. Here I am not adding
## the day when employee was on leave.

emp_daily_office_hours$avg_hours <- 0

emp_daily_office_hours$avg_hours <- rowMeans(emp_daily_office_hours[,2:250], na.rm = T)

## Removing all checkin and checkout columns from emp_daily_office_hours dataframe, as we have already got avg office hour 
## any employee and number of leaves they have taken.

emp_daily_office_hours <- emp_daily_office_hours[, -(2:250)]

## Renaming the name of column X to EmployeeID as mentioned in the file.
colnames(emp_daily_office_hours)[1] <- "EmployeeID"

## By dividing avg_hours by 60 we can come to know if employee is spending the required hours (i.e 8hrs) in office.
emp_daily_office_hours$avg_hours <- emp_daily_office_hours$avg_hours/60

## Let's look at the extracted data from emp_daily_office_hours data frame
str(emp_daily_office_hours)

#----------------------------------------------------------------------------------------------------------------------

## Checking duplicate entries in emp and mgr survey data before merging them
which(duplicated(emp_survey_data$EmployeeID))
which(duplicated(mgr_survey_data$EmployeeID))

## Checking identical EmployeeID for these datasets
setdiff(emp_survey_data$EmployeeID,mgr_survey_data$EmployeeID)

survey_data <- merge(emp_survey_data, mgr_survey_data, by="EmployeeID")

## After looking at the emp_data dataframe, we can obserb that Over18, EmployeeCount,
## StandardHours are having same value across the rows, so will remove these columns from the
## emp_data dataframe, though we will use this value (i.e, 8 hrs) to derive a new column after we merge this with 
## emp_daily_office_hours data frame.
unique(emp_data$EmployeeCount)
unique(emp_data$Over18)
unique(emp_data$StandardHours)
emp_data <- emp_data[,-c(8,16,18)]


## Checking duplicate and  identical entries in emp data before merging with survey data
which(duplicated(emp_data$EmployeeID))

setdiff(emp_data$EmployeeID,survey_data$EmployeeID)

emp_data_with_servey <- merge(emp_data, survey_data, by="EmployeeID")

## Merging emp_daily_office_hours and emp_data_with_servey, forming master employee data.
## First let's look at any differences in the these two data frame before merging.
setdiff(emp_data_with_servey$EmployeeID,emp_daily_office_hours$EmployeeID)

emp_master_data <- merge(emp_data_with_servey, emp_daily_office_hours, by="EmployeeID")

str(emp_master_data)

#################################################################################################
## Plotting the graph and visualise them
#################################################################################################
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


ggplot(emp_master_data, aes(x=Age,fill=Attrition))+ geom_bar()

plot_grid(ggplot(emp_master_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(emp_master_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_master_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_master_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_master_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(emp_master_data, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

##Let's look at outliers for numeric variables such as  using box plot

boxplot(emp_master_data$MonthlyIncome)
boxplot(emp_master_data$Age)
boxplot(emp_master_data$PercentSalaryHike)

#################################################################################################
## Data cleaning/prepartion and deriving new matrix

## Cleaning employee master data, for various columns which as NA values
## We will either replace NA with mean, median or 0, depending on the variable
## Here for EnvironmentSatisfaction, we are replacing it with median because it is correct replacement of NA
emp_master_data$EnvironmentSatisfaction[which(is.na(emp_master_data$EnvironmentSatisfaction))] <- median(emp_master_data$EnvironmentSatisfaction, na.rm = T)

## Replacing NA with mean for WorkLifeBalance
emp_master_data$WorkLifeBalance[which(is.na(emp_master_data$WorkLifeBalance))] <- median(emp_master_data$WorkLifeBalance, na.rm = T)

## Replacing NA with median for JobSatisfaction
emp_master_data$JobSatisfaction[which(is.na(emp_master_data$JobSatisfaction))] <- median(emp_master_data$JobSatisfaction, na.rm = T)

## Replacing NA with median for TotalWorkingYears, considering the employee is fresher and don't have total working years
emp_master_data$TotalWorkingYears[which(is.na(emp_master_data$TotalWorkingYears))] <- median(emp_master_data$TotalWorkingYears, na.rm = T)

## Replacing NA with median
emp_master_data$NumCompaniesWorked[which(is.na(emp_master_data$NumCompaniesWorked))] <- median(emp_master_data$NumCompaniesWorked, na.rm = T)

## Checking if any NA left in the master data frame
which(is.na(emp_master_data))

## New matrix for working hours, let say if the employee worked less than 8 hrs then it can be considered as he didn't work sufficient hrs in office
## similarly if he had worked for more than 9 hrs then we can say he worked over time. Let's create this variable

emp_master_data$avg_clock_remark <- NA
employeeOfficeTime <- function(x){
  if(x < 8){
    x <- "LessClocked"
  }  else if(x > 9){
    x <- "OverClocked"
  }  else{
    x <- "SufficeClocked"
  }
}

emp_master_data$avg_clock_remark <- sapply(emp_master_data$avg_hours, FUN = employeeOfficeTime)

##Converting numberic scale such as Education, Environment Satisfaction etc to their respective levels
## as given the data dictionary file

## Education
educationFun <- function(x){
  if(x == 1){
    x <- "Below College"
  } else if( x == 2){
    x <- "College"
  } else if (x == 3){
    x <- "Bachelor"
  } else if (x == 4){
    x <- "Master"
  } else {
    x <- "Doctor"
  }
}
emp_master_data$Education <- sapply(emp_master_data$Education, FUN = educationFun)

## EnvironmentSatisfaction
envSatisfactionFun <- function(x){
  if(x == 1){
    x <- "Low"
  } else if( x == 2){
    x <- "Medium"
  } else if (x == 3){
    x <- "High"
  } else {
    x <- "Very High"
  }
}
emp_master_data$EnvironmentSatisfaction <- sapply(emp_master_data$EnvironmentSatisfaction, FUN = envSatisfactionFun)

## jobInvolvement 
jobInvolvementFun <- function(x){
  if(x == 1){
    x <- "Low"
  } else if( x == 2){
    x <- "Medium"
  } else if (x == 3){
    x <- "High"
  } else {
    x <- "Very High"
  }
}
emp_master_data$JobInvolvement <- sapply(emp_master_data$JobInvolvement, FUN = jobInvolvementFun)

## JobSatisfaction
jobSatisfactionFun <- function(x){
  if(x == 1){
    x <- "Low"
  } else if( x == 2){
    x <- "Medium"
  } else if (x == 3){
    x <- "High"
  } else {
    x <- "Very High"
  }
}
emp_master_data$JobSatisfaction <- sapply(emp_master_data$JobSatisfaction, FUN = jobSatisfactionFun)

## PerformanceRating
performanceRateFun <- function(x){
  if(x == 1){
    x <- "Low"
  } else if( x == 2){
    x <- "Good"
  } else if (x == 3){
    x <- "Excellent"
  } else {
    x <- "Outstanding"
  }
}
emp_master_data$PerformanceRating <- sapply(emp_master_data$PerformanceRating, FUN = performanceRateFun)


## WorkLifeBalance
workLifeFun <- function(x){
  if(x == 1){
    x <- "Bad"
  } else if( x == 2){
    x <- "Good"
  } else if (x == 3){
    x <- "Better"
  } else {
    x <- "Best"
  }
}
emp_master_data$WorkLifeBalance <- sapply(emp_master_data$WorkLifeBalance, FUN = workLifeFun)

## Distance, converting distance variable to three categorical variables
distanceFun <- function(x){
  if(x > 20){
    x <- "Very Far"
  } else if( x > 10){
    x <- "Far"
  } else if( x > 5){
    x <- "Near"
  } else {
    x <- "Very Near"
  }
}
emp_master_data$DistanceFromHome <- sapply(emp_master_data$DistanceFromHome, FUN = distanceFun)


##Converting categorical variables into numeric variables, having two levels
emp_master_data$Attrition <- ifelse(emp_master_data$Attrition == "Yes", 1, 0)
emp_master_data$Gender <-  ifelse(emp_master_data$Gender == "Male", 1 , 0)

## Creating dummy variables for other categorical variables
str(emp_master_data)
## Business Travel
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = emp_master_data))
dummy_1 <- dummy_1[,-1]

## Department
dummy_2 <- data.frame(model.matrix( ~Department, data = emp_master_data))
dummy_2 <- dummy_2[,-1]

## Education Field
dummy_3 <- data.frame(model.matrix( ~EducationField, data = emp_master_data))
dummy_3 <- dummy_3[,-1]

## Job Role
dummy_4 <- data.frame(model.matrix( ~JobRole, data = emp_master_data))
dummy_4 <- dummy_4[,-1]

## Marital Status
dummy_5 <- data.frame(model.matrix( ~MaritalStatus, data = emp_master_data))
dummy_5 <- dummy_5[,-1]

## DistanceFromHome
dummy_6 <- data.frame(model.matrix( ~DistanceFromHome, data = emp_master_data))
dummy_6 <- dummy_6[,-1]

## Education
dummy_7 <- data.frame(model.matrix( ~Education, data = emp_master_data))
dummy_7 <- dummy_7[,-1]

## EnvironmentSatisfaction
dummy_8 <- data.frame(model.matrix( ~EnvironmentSatisfaction, data = emp_master_data))
dummy_8 <- dummy_8[,-1]

## JobSatisfaction
dummy_9 <- data.frame(model.matrix( ~JobSatisfaction, data = emp_master_data))
dummy_9 <- dummy_9[,-1]

## WorkLifeBalance
dummy_10 <- data.frame(model.matrix( ~WorkLifeBalance, data = emp_master_data))
dummy_10 <- dummy_10[,-1]

## JobInvolvement
dummy_11 <- data.frame(model.matrix( ~JobInvolvement, data = emp_master_data))
dummy_11 <- dummy_11[,-1]

## PerformanceRating
dummy_12 <- data.frame(model.matrix( ~PerformanceRating, data = emp_master_data))
dummy_12 <- dummy_12[,-1]

## avg_clock_remark
dummy_13 <- data.frame(model.matrix( ~avg_clock_remark, data = emp_master_data))
dummy_13 <- dummy_13[,-1]


## Normalizing variables by using scale function
emp_master_data$Age <- scale(emp_master_data$Age)
emp_master_data$MonthlyIncome <- scale(emp_master_data$MonthlyIncome)
emp_master_data$JobLevel <- scale(emp_master_data$JobLevel)
emp_master_data$NumCompaniesWorked <- scale(emp_master_data$NumCompaniesWorked)
emp_master_data$PercentSalaryHike <- scale(emp_master_data$PercentSalaryHike)
emp_master_data$StockOptionLevel <- scale(emp_master_data$StockOptionLevel)
emp_master_data$TotalWorkingYears <- scale(emp_master_data$TotalWorkingYears)
emp_master_data$TrainingTimesLastYear <- scale(emp_master_data$TrainingTimesLastYear)
emp_master_data$YearsAtCompany <- scale(emp_master_data$YearsAtCompany)
emp_master_data$YearsSinceLastPromotion <- scale(emp_master_data$YearsSinceLastPromotion)
emp_master_data$leave_taken <- scale(emp_master_data$leave_taken)
emp_master_data$YearsWithCurrManager <- scale(emp_master_data$YearsWithCurrManager)

## Merging all dummy tables and employee data

emp_final <- cbind(emp_master_data[, c(2,3,9,10,13:21,27)], dummy_1, dummy_2, dummy_3, dummy_4, dummy_5, dummy_6,
                   dummy_7, dummy_8, dummy_9, dummy_10, dummy_11, dummy_12, dummy_13)

str(emp_final)


##=================================================================================================================##
# splitting the data between training and test
set.seed(100)

indices = sample.split(emp_final$Attrition, SplitRatio = 0.7)

train_data = emp_final[indices,]

test = emp_final[!(indices),]


###############################################################################################################
## Model building using Logistics Regression

model_1 <- glm(Attrition~., data = emp_final, family = "binomial")
summary(model_1)

model_2 <- stepAIC(model_1, direction = "both")


## Removing variables which are marked + after stepAIC function, we left with 37 variables, 
## let's take them all and create a new model

model_3 <- glm(formula = Attrition ~ JobInvolvementMedium + JobRoleLaboratory.Technician + EducationDoctor + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobRoleManufacturing.Director + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + EducationFieldMarketing + BusinessTravelTravel_Rarely + EducationFieldOther +
                 EducationFieldTechnical.Degree  + JobRoleSales.Executive + TrainingTimesLastYear + EducationFieldLife.Sciences +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion + EducationFieldMedical +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_3)
vif(model_3)

## Let's remove EducationFieldLife.Sciences, which has high vif and less significant

model_4 <- glm(formula = Attrition ~ JobInvolvementMedium + JobRoleLaboratory.Technician + EducationDoctor + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobRoleManufacturing.Director + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + EducationFieldMarketing + BusinessTravelTravel_Rarely + EducationFieldOther +
                 EducationFieldTechnical.Degree  + JobRoleSales.Executive + TrainingTimesLastYear +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion + EducationFieldMedical +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_4)
vif(model_4)

## Removing  JobInvolvementMedium, as it has high vif and less significient
model_5 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + EducationDoctor + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobRoleManufacturing.Director + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + EducationFieldMarketing + BusinessTravelTravel_Rarely + EducationFieldOther +
                 EducationFieldTechnical.Degree  + JobRoleSales.Executive + TrainingTimesLastYear +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion + EducationFieldMedical +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_5)
vif(model_5)

## Removing  EducationFieldMedical, as it is less significant
model_6 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + EducationDoctor + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobRoleManufacturing.Director + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + EducationFieldMarketing + BusinessTravelTravel_Rarely + EducationFieldOther +
                 EducationFieldTechnical.Degree  + JobRoleSales.Executive + TrainingTimesLastYear +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_6)
vif(model_6)

## Removing EducationFieldTechnical.Degree for high vif and insignificant
model_7 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + EducationDoctor + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobRoleManufacturing.Director + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + EducationFieldMarketing + BusinessTravelTravel_Rarely + EducationFieldOther +
                 JobRoleSales.Executive + TrainingTimesLastYear +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_7)
vif(model_7)

## Removing JobRoleManufacturing.Director for its insignificant value

model_8 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + EducationDoctor + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + EducationFieldMarketing + BusinessTravelTravel_Rarely + EducationFieldOther +
                 JobRoleSales.Executive + TrainingTimesLastYear +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_8)
vif(model_8)

## Removing EducationFieldMarketing and EducationDoctor for its insignificant value and high vif
model_9 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + YearsAtCompany +
                 JobRoleResearch.Scientist + MaritalStatusMarried + JobInvolvementLow +
                 DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                 Age + JobRoleResearch.Director + BusinessTravelTravel_Rarely + EducationFieldOther +
                 JobRoleSales.Executive + TrainingTimesLastYear +
                 JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                 WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                 NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                 avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_9)
vif(model_9)

## Removing YearsAtCompany and MaritalStatusMarried for high vif and p-value
model_10 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + JobRoleResearch.Scientist + JobInvolvementLow +
                  DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                  Age + JobRoleResearch.Director + BusinessTravelTravel_Rarely + EducationFieldOther +
                  JobRoleSales.Executive + TrainingTimesLastYear +
                  JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_10)
vif(model_10)

## Removing EducationFieldOther
model_11 <- glm(formula = Attrition ~ JobRoleLaboratory.Technician + JobRoleResearch.Scientist + JobInvolvementLow +
                  DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobInvolvementVery.High + JobRoleSales.Executive + 
                  Age + JobRoleResearch.Director + BusinessTravelTravel_Rarely + JobRoleSales.Executive + TrainingTimesLastYear +
                  JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_11)
vif(model_11)

## Now the model variables which have high vif are also very significant, so we are not removing them.
## Let's remove the variables which are insignificant by looking at their p-value
## JobRoleLaboratory.Technician, JobInvolvementLow, JobInvolvementVery.High 

model_12 <- glm(formula = Attrition ~ JobRoleResearch.Scientist + 
                  DistanceFromHomeVery.Far + EnvironmentSatisfactionVery.High + JobRoleSales.Executive + 
                  Age + JobRoleResearch.Director + BusinessTravelTravel_Rarely + JobRoleSales.Executive + TrainingTimesLastYear +
                  JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_12)
vif(model_12)

## Again repeting last step and removing the variables which are less significant
## Removing JobRoleResearch.Scientist, DistanceFromHomeVery.Far and EnvironmentSatisfactionVery.High
model_13 <- glm(formula = Attrition ~ JobRoleSales.Executive + 
                  Age + JobRoleResearch.Director + BusinessTravelTravel_Rarely + JobRoleSales.Executive + TrainingTimesLastYear +
                  JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_13)
vif(model_13)

## Again repeating last step and removing the variables which are less significant
## Removing JobRoleResearch.Director
model_14 <- glm(formula = Attrition ~ JobRoleSales.Executive + Age + BusinessTravelTravel_Rarely + JobRoleSales.Executive + TrainingTimesLastYear +
                  JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_14)
vif(model_14)

## Removing JobRoleSales.Executive, as it is less significant compare to other variables
model_15 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Rarely + TrainingTimesLastYear +
                  JobSatisfactionLow + WorkLifeBalanceBest + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_15)
vif(model_15)

## Removing WorkLifeBalanceBest, as it is less significant compare and has high vif value
model_16 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Rarely + TrainingTimesLastYear +
                  JobSatisfactionLow + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  WorkLifeBalanceGood + TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_16)
vif(model_16)

## Removing WorkLifeBalanceGood, as it is less significant compare and has high vif value
model_17 <- glm(formula = Attrition ~ Age + BusinessTravelTravel_Rarely + TrainingTimesLastYear +
                  JobSatisfactionLow + JobSatisfactionVery.High + EnvironmentSatisfactionLow +
                  TotalWorkingYears + YearsWithCurrManager + YearsSinceLastPromotion +
                  NumCompaniesWorked + BusinessTravelTravel_Frequently + WorkLifeBalanceBetter + MaritalStatusSingle +
                  avg_clock_remarkSufficeClocked + avg_clock_remarkOverClocked, family = "binomial", data=train_data)

summary(model_17)
vif(model_17)

## Here all variables are highly significant, so we are not removing any variables further. Though we can see few variables
## have high vif. Let's assume model_17 as final model.

final_model <- model_17
############################################################################################################
##--   Model validation and verification
############################################################################################################
test_predict <- predict(final_model, type = "response", newdata = test[,-2])

summary(test_predict)

test$probability <- test_predict
View(test)

## Let's keep probaility threshold at 0.5, i.e, more than 0.5 means attrition and less is not
test_pred_attrition <- factor(ifelse(test_predict >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_pred_attrition,test_actual_attrition)

## Creating confusion matrix
test_confusion_matrix <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_confusion_matrix

# Let's find out the optimal probalility cutoff 

perform_fun <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_predict >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_predict)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fun(s[i])
}

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]

# Let's choose a cutoff value of 0.1616 for final model
test_cutoff_attrition <- factor(ifelse(test_predict >=0.154, "Yes", "No"))

confusion_matrix_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

accuracy_val <- confusion_matrix_final$overall[1]

sensitivity_val <- confusion_matrix_final$byClass[1]

specificity_val <- confusion_matrix_final$byClass[2]

accuracy_val

sensitivity_val

specificity_val

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

predicted_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_test<- performance(predicted_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_test, "y.values")[[1]] - 
  (attr(performance_test, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 
####################################################################
# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_attrition, test_predict, groups = 10)

plot(x=Churn_decile$Cumlift,y=Churn_decile$decile,type = 'o')
plot(x=Churn_decile$Gain,y=Churn_decile$decile,type ='o')

