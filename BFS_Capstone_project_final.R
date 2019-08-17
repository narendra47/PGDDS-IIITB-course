####################################################################################################
#########################                  BFSI Capstone Project            #########################
####################################################################################################

##Adding required libraries
library(dplyr)
library(MASS)
library(tidyr)
library(caret)
library(ggplot2)
library(CARS)
library(randomForest)
library(ROSE)
library(ROCR)
library(e1071)
library(caTools)
require(scales)
library(hexbin)
library(corrplot)



##Loading data from csv files
credit_bureau_df <- read.csv("https://cdn.upgrad.com/UpGrad/temp/3fa11a2f-f702-45e4-a7fd-17110148588d/Credit%20Bureau%20data.csv",stringsAsFactors = F, na.strings = c(""))
demographic_df <- read.csv("https://cdn.upgrad.com/UpGrad/temp/a790b665-7819-4490-87d9-b046340095fd/Demographic%20data.csv",stringsAsFactors = F,na.strings = c(""))

str(credit_bureau_df)
str(demographic_df)

#Coverting martial stuatus column name to smaller name
names(demographic_df)[4] <- 'Marital_Status'

#Number of rows and columns
dim(credit_bureau_df)   
# 71295x19
dim(demographic_df)
# 71295x12
##same no of rows

setdiff(credit_bureau_df$Application.ID,demographic_df$Application.ID)
##setdiff shows that the application id is same in both the datafiles so this can be used to join both the datasets

summary(demographic_df)
#min age is showing as -3, it means data issues are there
#gender,marital status,Education,profession,type_of_residence has blank values
#min Income is -0.5 which again is a data issue
#There are NA's in performance tag and no of dependants

summary(credit_bureau_df)
# From the summary we can see the presence of open home loan, outstanding balance and target variable
# performance tag has NA's.
# We will treat NA's post merge of these two data frame

##checking NA's in demographic_df dataset
sapply(demographic_df, function(x) sum(is.na(x))*100/nrow(demographic_df))
## ~2% of performance tag values are NA. This attribute is a binary attribute and can't have NAs. So we can consider these records as rejected applicants.
## no of dependatns have only 3 NA values, so we can ignore them

##checking NA's in credit bureau dataset
sapply(credit_bureau_df, function(x) sum(is.na(x))*100/nrow(credit_bureau_df))
##Avgas.CC.Utilization.in.last.12.months has 1.48% has NA values, here also our target variable is having ~2% of missing values

##checking duplicated values
sum(duplicated(demographic_df$Application.ID))
which(duplicated(demographic_df$Application.ID)==TRUE)
## 27587 42638 59023 these demographic_df application ids are duplicated

sum(duplicated(credit_bureau_df$Application.ID))
which(duplicated(credit_bureau_df$Application.ID)==TRUE)
## 27587 42638 59023 these credit_bureau_df application ids are duplicated

# Since all these having different details, except application id. So We have decided to keep them.
# Here we are changing their 
credit_bureau_df[27587,]$Application.ID <- max(credit_bureau_df$Application.ID) + 1

credit_bureau_df[42638,]$Application.ID <- max(credit_bureau_df$Application.ID) + 2

credit_bureau_df[59023,]$Application.ID <- max(credit_bureau_df$Application.ID) + 3

demographic_df[27587,]$Application.ID <- max(demographic_df$Application.ID) + 1 

demographic_df[42638,]$Application.ID <- max(demographic_df$Application.ID) + 2

demographic_df[59023,]$Application.ID <- max(demographic_df$Application.ID) + 3

# Checking if any duplicate value left here
which(duplicated(demographic_df$Application.ID)==TRUE)
which(duplicated(credit_bureau_df$Application.ID)==TRUE)


##duplicate application ids are same in booth the datasets, so we are removing them for further analysis
#demographic_df <- demographic_df[-which(duplicated(demographic_df$Application.ID) == TRUE), ]
#credit_bureau_df <- credit_bureau_df[-which(duplicated(credit_bureau_df$Application.ID) == TRUE), ]

# Let's check if the target variable in these two dataframe have other/mismatch values for the same row.
target_var_diff <- demographic_df$Performance.Tag - credit_bureau_df$Performance.Tag
table(target_var_diff)
# We can see all value is 0, so we can conclude that the target variable (performance tag) is 
# same in both of data frame for the application id.

# Merging these two datasets into one for further analysis
master_df <- merge(demographic_df,credit_bureau_df, by = "Application.ID")

# Removing application id and one of performance tag as they are redundant
master_df <- master_df[, -c(1, 12)]

master_df_bk <- master_df

names(master_df_bk)[28] <- 'Performance_Tag'

summary(master_df)

## NAs treatment
#  For Gender, Marital_status, Profession column we will replace empty/NA value with most frequent.

master_df$Gender[which(is.na(master_df$Gender))] <- 'M'
master_df$Marital_Status[which(is.na(master_df$Marital_Status))] <- 'Married'
master_df$Profession[which(is.na(master_df$Profession))] <- 'SAL'

# For Education, Type of residence we will replace NA with 'other' variables.
master_df$Education[which(is.na(master_df$Education))] <- 'Others'
master_df$Type.of.residence[which(is.na(master_df$Type.of.residence))] <- 'Others'

# For NAs in No. of dependent, we will replace them with 0. Assuming that they don;t have any dependent
master_df$No.of.dependents[which(is.na(master_df$No.of.dependents))] <- 0

# For NAs in Avg CC utilization, we will replace them with 0. Assuming that they don't use CC now
master_df$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_df$Avgas.CC.Utilization.in.last.12.months))] <- 0

# For NAs in Presence.of.open.home.loan and outstanding.balance, we will impute them with median value
master_df$Presence.of.open.home.loan[which(is.na(master_df$Presence.of.open.home.loan))] <- median(master_df$Presence.of.open.home.loan, na.rm = T)
master_df$Outstanding.Balance[which(is.na(master_df$Outstanding.Balance))] <- median(master_df$Outstanding.Balance, na.rm = T)

# There is one NA in No.of.trades.opened.in.last.6.months, we will impute them with median value
master_df$No.of.trades.opened.in.last.6.months[which(is.na(master_df$No.of.trades.opened.in.last.6.months))] <-
  median(master_df$No.of.trades.opened.in.last.6.months, na.rm = T)

summary(master_df)

# We can see there are 1425 target variables are NA, we will store them in different dataframe, and
# will use in later stage and try to calculate the scorecard for these applicants as well.
na_index <- which(is.na(master_df$Performance.Tag.y))
master_df_with_NA <- master_df[na_index,]

master_df <- master_df[- na_index, ]

# Checking if any NAs left in the masterdf
which(is.na(master_df) == TRUE)
summary(master_df)

# to show the value in deimal
options(scipen = 999)
####------------------------------------------------------------------------------------------------------
# Outliers treatment

# For negative values such as -3, 0 in Age and for negative income, we will replace them with median value as 
# the the no of such applicant's are less than 1%. 
master_df$Age[which(master_df$Age < 1)] <- median(master_df$Age)
master_df$Income[which(is.na(master_df$Income))] <- 0

ggplot(master_df[which(master_df$Education == 'Phd'),], aes(Age)) + geom_histogram()
quantile(master_df[which(master_df$Education == 'Phd'),]$Age,seq(0,1,0.01))
boxplot(master_df$Age~master_df$Education,horizontal = TRUE)

# We can observe that even applicant's below age of 25 also having Phd, will replace the age of applicants accordingly
mean_phd_age <- round(mean(filter(master_df, Age >= 27 & Education == 'Phd')$Age))
master_df <- master_df %>% mutate(Age = ifelse(Age < 27 & Education == 'Phd', mean_phd_age, Age))
ggplot(master_df[which(master_df$Education == 'Phd'),], aes(Age)) + geom_histogram()

boxplot(master_df$Age)
hist(master_df$Age)
quantile(master_df$Age)

boxplot(master_df$Income)
hist(master_df$Income)

boxplot(master_df$Avgas.CC.Utilization.in.last.12.months)
hist(master_df$Avgas.CC.Utilization.in.last.12.months)
# It has some outliers, but will not ceil them as it may loss some important info

boxplot(master_df$No.of.times.90.DPD.or.worse.in.last.6.months)
hist(master_df$No.of.times.90.DPD.or.worse.in.last.6.months)
# No outliers for any of DPD data

boxplot(master_df$No.of.trades.opened.in.last.12.months)
hist(master_df$No.of.trades.opened.in.last.12.months)

boxplot(master_df$Outstanding.Balance)
hist(master_df$Outstanding.Balance)

quantile(master_df$No.of.months.in.current.residence)
boxplot(master_df$No.of.months.in.current.residence)

###########################################################################################
#*****************************Univariate Analysis**********************************

# creating function for univariate analysis of categorical variables

univar_categorical <- function(dataset,var,var_name){
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 50, hjust = 1)
    ) 
}

univar_categorical(master_df,master_df$Gender,"Gender Distrubution")
univar_categorical(master_df,master_df$Marital_Status,"Marital Status Distribution")
univar_categorical(master_df,master_df$No.of.dependents,"No.of.dependents Distribution")
univar_categorical(master_df,master_df$Education,"Education Distribution")

univar_categorical(master_df,master_df$Profession,"Profession Distribution")
univar_categorical(master_df,master_df$Type.of.residence,"Type.of.residence Distribution")
univar_categorical(master_df,master_df$Performance.Tag,"Performace.Tag Distribution") #x-axis labels angles need to change

##Univariate  analysis for continuous variables
univar_continuous <- function(dataset, var){
  dataset %>% ggplot(aes(x=var))+
    geom_histogram(aes(fill=..count..))
}

# Age
univar_continuous(master_df,master_df$Age)
boxplot(master_df$Age)

##2 Income - Income plays critical role
univar_continuous(master_df,master_df$Income)
summary(factor(master_df$Income))

## No.of.months.in.current.residence
univar_continuous(master_df,master_df$No.of.months.in.current.residence)
summary((master_df$No.of.months.in.current.residence))
quantile(master_df$No.of.months.in.current.residence,seq(0,1,0.025))

## No.of.months.in.current.company
univar_continuous(master_df,master_df$No.of.months.in.current.company)
summary((master_df$No.of.months.in.current.company))
quantile(master_df$No.of.months.in.current.company,seq(0,1,0.025))

str(master_df)

##we need to decide if these incorrect values will be replaced by mean or simple we should ignore them...
## after correcting these 2 attributes our data is cleaned and ready to work on
## please feel free to add/modify if anything is missed/incorrect

# Bivariate analysis on Continuous variable

ggplot(master_df, aes(x= Age, y = Income))+geom_hex(bins=20)
ggplot(master_df, aes(x= Income,fill=Gender)) + geom_density() + facet_grid(.~Gender) #look similar, no inference
# Bivariate analysis on Categorical variable
ggplot(master_df, aes(x=Gender,fill=Performance.Tag.y))+geom_bar(position = "dodge") 

################################################################################################

# Renaming target variable
names(master_df)[28] <- 'Performance_Tag'

# Creating a backup file for master_df
master_df_bkup <- master_df

View(master_df)

#--------------------------------------------------------------------------------------

####################################################################################
######## Feature Standardisation  for Logistics Regression##########################
####################################################################################

#  Sereparting Numeric and Factor variables 

numeric_var <- c("Age",
                 "Income",
                 "No.of.months.in.current.residence",
                 "No.of.months.in.current.company",
                 "No.of.times.90.DPD.or.worse.in.last.6.months",
                 "No.of.times.60.DPD.or.worse.in.last.6.months",
                 "No.of.times.30.DPD.or.worse.in.last.6.months",
                 "No.of.times.90.DPD.or.worse.in.last.12.months",
                 "No.of.times.60.DPD.or.worse.in.last.12.months",
                 "No.of.times.30.DPD.or.worse.in.last.12.months",
                 "Avgas.CC.Utilization.in.last.12.months",
                 "No.of.trades.opened.in.last.6.months",
                 "No.of.trades.opened.in.last.12.months",
                 "No.of.PL.trades.opened.in.last.6.months",
                 "No.of.PL.trades.opened.in.last.12.months",
                 "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                 "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                 "Outstanding.Balance",
                 "Total.No.of.Trades",
                 "Presence.of.open.home.loan",
                 "Presence.of.open.auto.loan",
                 "No.of.dependents")

master_df_num <- master_df[,numeric_var]  #creating DF with all numeric variables

# Scaling numeric columns
master_df_num <- data.frame(sapply(master_df_num, scale))
str(master_df_num)

target_var <- c("Performance_Tag")
Performance_Tag <- as.factor(master_df[,target_var])

factor_var <- c("Gender",
                "Marital_Status",
                "Education",
                "Profession",
                "Type.of.residence")

master_df_fact <- master_df[,factor_var]

master_df_fact <- data.frame(sapply(master_df_fact, as.factor))
str(master_df_fact)

# creating dummy variables
dummy_vars<- data.frame(sapply(master_df_fact,function(x) data.frame(model.matrix(~x-1,data =master_df_fact))[,-1])) 


## Merging all scaled numerical value and dummies for final dataframe
master_df_final <- cbind(master_df_num,dummy_vars,Performance_Tag)

str(master_df_final)


## Correlation analysis

correlation_df<- master_df[,numeric_var]

corr_index<- cor(correlation_df) 

corrplot(corr_index,title = "correlation graph", method = "circle", order = "alphabet",
         type = "upper", tl.cex = 0.5, tl.col = 'blue')

# From the correlation graph we can see high correlation among no of times 30/60/90 DPDs variables. Also same can be
# observed among trade variables.

## Splitting the data into train and test datasets for logistics regression

set.seed(100)

indices <- sample.split(master_df_final$Performance_Tag, SplitRatio = 0.7)

train_data <- master_df_final[indices,]

test_data <- master_df_final[!(indices),]

## SMOTE using ROSE, since the data is highly imbalance we are using ROSE function to make data more balance.

bal_train_data <- ROSE(Performance_Tag ~ ., data = train_data, seed = 1)$data

table(bal_train_data$Performance_Tag)

#install.packages("DMwR")
#library(DMwR)

#trainSplit <- SMOTE(Performance_Tag ~ ., train_data, perc.over = 1000, perc.under=2000, k=5)
#table(trainSplit$Performance_Tag)


################################################################
############### Logistic Regression ############################
################################################################

#Initial model based on logistics regression

model_1 = glm(Performance_Tag ~ ., data = bal_train_data, family = "binomial")
summary(model_1)

# Using stepAIC function to find the insignificant variables
model_2<- stepAIC(model_1, direction="both")
summary(model_2)


# Removing insignificant vars from the model as given after stepAIC function
model_3 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF + Presence.of.open.auto.loan + Education.xProfessional +
                 Type.of.residence.xOwned + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 No.of.trades.opened.in.last.12.months + Gender + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)
summary(model_3)

## check multi collinearity through VIF function
vif(model_3)

# Removing Presence.of.open.auto.loan as it is insignificant variable, and building model again
model_4 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF + Education.xProfessional +
                 Type.of.residence.xOwned + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 No.of.trades.opened.in.last.12.months + Gender + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)

summary(model_4)
vif(model_4)

# Removing Education.xProfessional as it is insignificant variable in the model
model_5 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                 Type.of.residence.xOwned + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 No.of.trades.opened.in.last.12.months + Gender + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)

summary(model_5)
vif(model_5)

# Removing Type.of.residence.xOwned from the model
model_6 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 No.of.trades.opened.in.last.12.months + Gender + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)

summary(model_6)
vif(model_6)

# Removing No.of.trades.opened.in.last.12.months from the model building
model_7 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Gender + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)

summary(model_7)
vif(model_7)

# Removing Gender from the model as it has high correlation value
model_8 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. +
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)

summary(model_8)
vif(model_8)

# Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans from the model and build again
model_9 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + No.of.times.90.DPD.or.worse.in.last.6.months +
                 Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                 No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                 No.of.months.in.current.company + Income + 
                 No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                 No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
               family = "binomial", data = bal_train_data)

summary(model_9)
vif(model_9)

# Removing No.of.times.90.DPD.or.worse.in.last.6.months from the model and build again
model_10 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  Presence.of.open.home.loan + Total.No.of.Trades + No.of.times.60.DPD.or.worse.in.last.12.months +
                  No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                  No.of.months.in.current.company + Income + 
                  No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_10)
vif(model_10)

# Removing Total.No.of.Trades from the model
model_11 <- glm(formula = Performance_Tag ~ Profession.xSE_PROF +
                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  Presence.of.open.home.loan + No.of.times.60.DPD.or.worse.in.last.12.months +
                  No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                  No.of.months.in.current.company + Income + 
                  No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_11)
vif(model_11)

# Removing Profession.xSE_PROF from the model
model_12 <- glm(formula = Performance_Tag ~  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  Presence.of.open.home.loan + No.of.times.60.DPD.or.worse.in.last.12.months +
                  No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                  No.of.months.in.current.company + Income + 
                  No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_12)
vif(model_12)


# Removing Presence.of.open.home.loan from the model
model_13 <- glm(formula = Performance_Tag ~  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  No.of.times.60.DPD.or.worse.in.last.12.months +
                  No.of.months.in.current.residence + Profession.xSE + No.of.times.60.DPD.or.worse.in.last.6.months +
                  No.of.months.in.current.company + Income + 
                  No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_13)
vif(model_13)

# Removing No.of.times.60.DPD.or.worse.in.last.6.months from the model
model_14 <- glm(formula = Performance_Tag ~  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  No.of.times.60.DPD.or.worse.in.last.12.months + No.of.months.in.current.residence +
                  No.of.months.in.current.company + Income + Profession.xSE +
                  No.of.times.30.DPD.or.worse.in.last.12.months +  No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_14)
vif(model_14)

# Removing No.of.times.30.DPD.or.worse.in.last.12.months from the model
model_15 <- glm(formula = Performance_Tag ~  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  No.of.times.60.DPD.or.worse.in.last.12.months + No.of.months.in.current.residence +
                  No.of.months.in.current.company + Income + Profession.xSE + No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_15)
vif(model_15)

# Removing No.of.months.in.current.residence from the model
model_16 <- glm(formula = Performance_Tag ~  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  No.of.times.60.DPD.or.worse.in.last.12.months + 
                  No.of.months.in.current.company + Income + Profession.xSE + No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_16)
vif(model_16)

# Removing Profession.xSE from the model
model_17 <- glm(formula = Performance_Tag ~  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                  No.of.times.60.DPD.or.worse.in.last.12.months + 
                  No.of.months.in.current.company + Income +  No.of.times.30.DPD.or.worse.in.last.6.months +
                  No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +
                  No.of.PL.trades.opened.in.last.12.months + Avgas.CC.Utilization.in.last.12.months, 
                family = "binomial", data = bal_train_data)

summary(model_17)
vif(model_17)

# Here I find all these variables are significant, and deciding the finalize the logistics regression model as model_17

## Final model
final_model <- model_17

#######################################################################
############################ Model Evaluation #########################
# Predict probabilities of test data
test_pred <- predict(final_model, type = "response", newdata = test_data)

# Let's see summary of predictions
#row.names(test_pred) <- NULL
summary(test_pred)

# Let's use the probability cutoff of 50%.
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test_data$Performance_Tag==1,"Yes","No"))

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

### Confusion Matrix and Statistics

#   Reference
#   Prediction    No   Yes
#           No  11780   284
#           Yes  8296   600

#         Accuracy : 0.5917             
#         95% CI : (0.584, 0.5973)    
#         No Information Rate : 0.9578             
#         P-Value [Acc > NIR] : 1                  

#         Kappa : 0.0498             

#         Mcnemar's Test P-Value : <0.0000000000000002

#            Sensitivity : 0.68173            
#            Specificity : 0.58774

# Accuracy is low, let's find out optimal probability cutoff value


perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,.90,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]

cutoff

# 0.5134343 0.5224242

# Let's choose a cutoff value of 0.52242 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.522, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc   # 64.82

sens  # 60.52

spec  # 65.00

# Confusion Matrix and Statistics

# Reference
# Prediction    No   Yes
#         No  13051   349
#         Yes  7026   535

# Accuracy : 0.6482             
# 95% CI : (0.6416, 0.6546)   
# No Information Rate : 0.9578             
# P-Value [Acc > NIR] : 1                  

# Kappa : 0.0554             

# Mcnemar's Test P-Value : <0.0000000000000002

#            Sensitivity : 0.60520            
#            Specificity : 0.65005  



### Exporting Data for Excel Analysis (KS, Gain, Lift etc.) ######

myeval <- matrix(nrow = length(test_pred),ncol = 2)
myeval[,1] <- test_pred
myeval[,2] <- test_actual_attrition
colnames(myeval) <- c("Predicted_Prob","Actual_Labels")
write.csv(myeval,"myeval.csv")

### KS -statistic - Test Data ######
#on testing  data
library(ROCR)

pred_object_test<- prediction(as.numeric(test_cutoff_attrition), as.numeric(test_actual_attrition))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

# KS statics is 25.5%

# Finding the area under curve
lr_auc <- performance(pred_object_test, measure = "auc")
lr_auc <- lr_auc@y.values[[1]]
lr_auc

# Area under curve is -  .6276 (62.76%)


# Lift & Gain Chart 

library(dplyr)

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

Performance_decile = lift(as.numeric(test_actual_attrition), as.numeric(test_pred), groups = 10)
Performance_decile

Gain <- c(0,Performance_decile$Gain)
Deciles <- c(0,Performance_decile$bucket)
plot(y=Gain,x=Deciles,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

Random_Gain <- seq(from=0,to=100,by=10)
lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="red")

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="darkgreen")


legend("bottomright",col=c("darkgreen","black","red"),lwd =c(2,2,2,2),c("Perfect Model","Actual Model","Random Model"), cex = 0.7)

# plotting the lift chart
Lift <- Gain/Random_Gain
Random_Lift <- Random_Gain/Random_Gain

plot(y=Lift,x=Deciles,type ="l",ylim=c(0,3.5),lwd = 2,xlab="Bucket",ylab="Lift",main = "Lift Chart",ylim<-c())
lines(y=Random_Lift,x=Deciles,type ="l",lwd = 2, col="red")

legend("topright",col=c("black","red"),lwd =c(2,2,2),c("Actual Model","Random Model"), cex = 0.7)



# The logitstics regression giving accuracy of ~65%, but here we have not considered the WOE/IV values for logistics model building.
# In our next step we will replace all feature variable with the WOE values and then proceed with logistics regression


#---------------------------------------------------------------------------------------------------
#---  WOE/IV analysis and model building using WOE/IV
#----------------------- 
library(Information)

master_df <- master_df_bkup


#############################################################################
########## Changing continuous variables to WOE ############################
quantile(master_df$Age,seq(0,1,0.1))

master_df$Age_bin <- cut(master_df$Age, breaks = c(14,31,36,39,42,45,48,51,54,58,65))

quantile(master_df$Income,seq(0,1,0.1))

master_df$Income_bin <- cut(master_df$Income, breaks = c(-1,6,11,17,22,27,32,37,42,49,60))

quantile(master_df$No.of.months.in.current.residence,seq(0,1,0.1))

master_df$No.of.months.in.current.residence_bin <- cut(master_df$No.of.months.in.current.residence, breaks = c(5,10,29,50,73,98,126))

quantile(master_df$No.of.months.in.current.company,seq(0,1,0.1))

master_df$No.of.months.in.current.company_bin <- 
  cut(master_df$No.of.months.in.current.company, breaks = c(2,6,13,20,27,34,41,48,54,62,133))

quantile(master_df$No.of.times.90.DPD.or.worse.in.last.6.months,seq(0,1,0.2))

master_df$No.of.times.90.DPD.or.worse.in.last.6.months.bin <- 
  cut(master_df$No.of.times.90.DPD.or.worse.in.last.6.months, breaks = c(-1,1,2,3))

quantile(master_df$No.of.times.60.DPD.or.worse.in.last.6.months,seq(0,1,0.1))

master_df$No.of.times.60.DPD.or.worse.in.last.6.months.bin <- cut(master_df$No.of.times.60.DPD.or.worse.in.last.6.months, breaks = c(-1,1,3,5))

quantile(master_df$No.of.times.30.DPD.or.worse.in.last.6.months,seq(0,1,0.1))

master_df$No.of.times.30.DPD.or.worse.in.last.6.months.bin <- cut(master_df$No.of.times.30.DPD.or.worse.in.last.6.months, breaks = c(-1,1,2,5,7))

quantile(master_df$No.of.times.90.DPD.or.worse.in.last.12.months,seq(0,1,0.1))

master_df$No.of.times.90.DPD.or.worse.in.last.12.months.bin <- cut(master_df$No.of.times.90.DPD.or.worse.in.last.12.months, breaks = c(-1,0,1,5))

quantile(master_df$No.of.times.60.DPD.or.worse.in.last.12.months,seq(0,1,0.1))

master_df$No.of.times.60.DPD.or.worse.in.last.12.months.bin <- cut(master_df$No.of.times.60.DPD.or.worse.in.last.12.months, breaks = c(-1,1,2,5,7))

quantile(master_df$No.of.times.30.DPD.or.worse.in.last.12.months,seq(0,1,0.1))

master_df$No.of.times.30.DPD.or.worse.in.last.12.months.bin <- cut(master_df$No.of.times.30.DPD.or.worse.in.last.12.months, breaks = c(-1,1,3,6,9))

quantile(master_df$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.05))

master_df$Avgas.CC.Utilization.in.last.12.months_bin <- cut(master_df$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1,5,7,9,12,15,23,39,54,76,113,9999))

quantile(master_df$No.of.trades.opened.in.last.6.months,seq(0,1,0.1))

master_df$No.of.trades.opened.in.last.6.months_bin <- cut(master_df$No.of.trades.opened.in.last.6.months, breaks = c(-1,1,2,3,4,5,12))

quantile(master_df$No.of.trades.opened.in.last.12.months,seq(0,1,0.1))

master_df$No.of.trades.opened.in.last.12.months_bin <- cut(master_df$No.of.trades.opened.in.last.12.months, breaks = c(-1,1,2,3,4,6,8,10,13,28))

quantile(master_df$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.1))

master_df$No.of.PL.trades.opened.in.last.6.months_bin <- cut(master_df$No.of.PL.trades.opened.in.last.6.months, breaks = c(-1,0,1,2,6))

quantile(master_df$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.1))

master_df$No.of.PL.trades.opened.in.last.12.months_bin <- cut(master_df$No.of.PL.trades.opened.in.last.12.months, breaks = c(-1,1,2,3,4,5,6,12))

quantile(master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.1))

master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._bin <- cut(master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., breaks = c(-1,1,2,3,5,10))



quantile(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.1))

master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._bin <- cut(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(-1,1,2,3,4,5,6,9,20))

quantile(master_df$Outstanding.Balance,seq(0,1,0.025))

master_df$Outstanding.Balance_bin <- cut(master_df$Outstanding.Balance, breaks = c(-1.0,6883.0,26096.2,388142.4,586671.0,775675.0,974586.4,1362124.2,2962665.8,3304586.0,5218801.0,9999999.0))

quantile(master_df$Total.No.of.Trades,seq(0,1,0.1))

master_df$Total.No.of.Trades_bin <- cut(master_df$Total.No.of.Trades, breaks = c(-1,0,2,3,4,5,6,7,9,11,20,44))

master_df$Performance_Tag <- as.numeric(master_df$Performance_Tag)

IV1 <- create_infotables(master_df, y="Performance_Tag", bins=10, parallel=FALSE)

IV_value1 <- data.frame(IV1$Summary)

View(IV_value1)


library(plyr)

master_df$Age_woe <- mapvalues(master_df$Age_bin, from = IV1$Tables$Age_bin$Age_bin, to = IV1$Tables$Age_bin$WOE)

master_df$Income_woe <- mapvalues(master_df$Income_bin, from = IV1$Tables$Income_bin$Income_bin, to = IV1$Tables$Income_bin$WOE)

master_df$No.of.months.in.current.residence_woe <- mapvalues(master_df$No.of.months.in.current.residence_bin, from = IV1$Tables$No.of.months.in.current.residence_bin$No.of.months.in.current.residence_bin, to = IV1$Tables$No.of.months.in.current.residence_bin$WOE)

master_df$No.of.months.in.current.company_woe <- mapvalues(master_df$No.of.months.in.current.company_bin, from = IV1$Tables$No.of.months.in.current.company_bin$No.of.months.in.current.company_bin, to = IV1$Tables$No.of.months.in.current.company_bin$WOE)

master_df$Gender_woe <- mapvalues(master_df$Gender, from = IV1$Tables$Gender$Gender, to = IV1$Tables$Gender$WOE)

master_df$Marital_Status_woe <- mapvalues(master_df$Marital_Status, from = IV1$Tables$Marital_Status$Marital_Status, to = IV1$Tables$Marital_Status$WOE)

master_df$No.of.dependents_woe <- mapvalues(master_df$No.of.dependents, from = IV1$Tables$No.of.dependents$No.of.dependents, to = IV1$Tables$No.of.dependents$WOE)

master_df$Education_woe <- mapvalues(master_df$Education, from = IV1$Tables$Education$Education, to = IV1$Tables$Education$WOE)

master_df$Profession_woe <- mapvalues(master_df$Profession, from = IV1$Tables$Profession$Profession, to = IV1$Tables$Profession$WOE)

master_df$Type.of.residence_woe <- mapvalues(master_df$Type.of.residence, from = IV1$Tables$Type.of.residence$Type.of.residence, to = IV1$Tables$Type.of.residence$WOE)



master_df$No.of.times.90.DPD.or.worse.in.last.6.months_woe <- mapvalues(master_df$No.of.times.90.DPD.or.worse.in.last.6.months.bin, from = IV1$Tables$No.of.times.90.DPD.or.worse.in.last.6.months.bin$No.of.times.90.DPD.or.worse.in.last.6.months.bin, to = IV1$Tables$No.of.times.90.DPD.or.worse.in.last.6.months.bin$WOE)

master_df$No.of.times.60.DPD.or.worse.in.last.6.months_woe <- mapvalues(master_df$No.of.times.60.DPD.or.worse.in.last.6.months.bin, from = IV1$Tables$No.of.times.60.DPD.or.worse.in.last.6.months.bin$No.of.times.60.DPD.or.worse.in.last.6.months.bin, to = IV1$Tables$No.of.times.60.DPD.or.worse.in.last.6.months.bin$WOE)

master_df$No.of.times.30.DPD.or.worse.in.last.6.months_woe <- mapvalues(master_df$No.of.times.30.DPD.or.worse.in.last.6.months.bin, from = IV1$Tables$No.of.times.30.DPD.or.worse.in.last.6.months.bin$No.of.times.30.DPD.or.worse.in.last.6.months.bin, to = IV1$Tables$No.of.times.30.DPD.or.worse.in.last.6.months.bin$WOE)

master_df$No.of.times.90.DPD.or.worse.in.last.12.months_woe <- mapvalues(master_df$No.of.times.90.DPD.or.worse.in.last.12.months.bin, from = IV1$Tables$No.of.times.90.DPD.or.worse.in.last.12.months.bin$No.of.times.90.DPD.or.worse.in.last.12.months.bin, to = IV1$Tables$No.of.times.90.DPD.or.worse.in.last.12.months.bin$WOE)

master_df$No.of.times.60.DPD.or.worse.in.last.12.months_woe <- mapvalues(master_df$No.of.times.60.DPD.or.worse.in.last.12.months.bin, from = IV1$Tables$No.of.times.60.DPD.or.worse.in.last.12.months.bin$No.of.times.60.DPD.or.worse.in.last.12.months.bin, to = IV1$Tables$No.of.times.60.DPD.or.worse.in.last.12.months.bin$WOE)

master_df$No.of.times.30.DPD.or.worse.in.last.12.months_woe <- mapvalues(master_df$No.of.times.30.DPD.or.worse.in.last.12.months.bin, from = IV1$Tables$No.of.times.30.DPD.or.worse.in.last.12.months.bin$No.of.times.30.DPD.or.worse.in.last.12.months.bin, to = IV1$Tables$No.of.times.30.DPD.or.worse.in.last.12.months.bin$WOE)

master_df$Avgas.CC.Utilization.in.last.12.months_woe <- mapvalues(master_df$Avgas.CC.Utilization.in.last.12.months_bin, from = IV1$Tables$Avgas.CC.Utilization.in.last.12.months_bin$Avgas.CC.Utilization.in.last.12.months_bin, to = IV1$Tables$Avgas.CC.Utilization.in.last.12.months_bin$WOE)

master_df$No.of.trades.opened.in.last.6.months_woe <- mapvalues(master_df$No.of.trades.opened.in.last.6.months_bin, from = IV1$Tables$No.of.trades.opened.in.last.6.months_bin$No.of.trades.opened.in.last.6.months_bin, to = IV1$Tables$No.of.trades.opened.in.last.6.months_bin$WOE)

master_df$No.of.trades.opened.in.last.12.months_woe <- mapvalues(master_df$No.of.trades.opened.in.last.12.months_bin, from = IV1$Tables$No.of.trades.opened.in.last.12.months_bin$No.of.trades.opened.in.last.12.months_bin, to = IV1$Tables$No.of.trades.opened.in.last.12.months_bin$WOE)

master_df$No.of.PL.trades.opened.in.last.6.months_woe <- mapvalues(master_df$No.of.PL.trades.opened.in.last.6.months_bin, from = IV1$Tables$No.of.PL.trades.opened.in.last.6.months_bin$No.of.PL.trades.opened.in.last.6.months_bin, to = IV1$Tables$No.of.PL.trades.opened.in.last.6.months_bin$WOE)

master_df$No.of.PL.trades.opened.in.last.12.months_woe <- mapvalues(master_df$No.of.PL.trades.opened.in.last.12.months_bin, from = IV1$Tables$No.of.PL.trades.opened.in.last.12.months_bin$No.of.PL.trades.opened.in.last.12.months_bin, to = IV1$Tables$No.of.PL.trades.opened.in.last.12.months_bin$WOE)

master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe <- mapvalues(master_df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._bin, from = IV1$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._bin$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._bin, to = IV1$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._bin$WOE)

master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe <- mapvalues(master_df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._bin, from = IV1$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._bin$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._bin, to = IV1$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._bin$WOE)

master_df$Outstanding.Balance_woe <- mapvalues(master_df$Outstanding.Balance_bin, from = IV1$Tables$Outstanding.Balance_bin$Outstanding.Balance_bin, to = IV1$Tables$Outstanding.Balance_bin$WOE)

master_df$Total.No.of.Trades_woe <- mapvalues(master_df$Total.No.of.Trades_bin, from = IV1$Tables$Total.No.of.Trades_bin$Total.No.of.Trades_bin, to = IV1$Tables$Total.No.of.Trades_bin$WOE)


master_df_woe <- master_df[,grep("woe",names(master_df))]

master_df_woe <- cbind(master_df_woe,master_df$Performance_Tag)

names(master_df_woe)

names(master_df_woe)[26] <- 'Performance_Tag'

# Let's proceed with model building in logistics regression

## Splitting the data into train and test datasets
library(caTools)
set.seed(100)

master_df_woe <- as.data.frame(lapply(master_df_woe, function(x) as.numeric(paste(x))))
master_df_woe$Performance_Tag <- as.factor(master_df_woe$Performance_Tag)

indices_woe <- sample.split(master_df_woe$Performance_Tag, SplitRatio = 0.7)
train_data_woe <- master_df_woe[indices_woe,]
test_data_woe <- master_df_woe[!(indices_woe),]
str(master_df_woe)

## SMOTE using ROSE, since the data is highly imbalance. Using rose function to balance the data
library(ROSE)

bal_train_data_woe <- ROSE(Performance_Tag ~ ., data = train_data_woe, seed = 1)$data

table(bal_train_data_woe$Performance_Tag)


#Initial model
woe_model_1 = glm(Performance_Tag ~ ., data = bal_train_data_woe, family = "binomial")
summary(woe_model_1)

# Using stepAIC function to find the insignificant variables
library(MASS)
woe_model_2<- stepAIC(woe_model_1, direction="both")
summary(woe_model_2)

woe_model_3<- glm(formula = Performance_Tag ~ No.of.times.90.DPD.or.worse.in.last.6.months_woe + 
                    No.of.times.30.DPD.or.worse.in.last.6.months_woe + No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                    No.of.times.60.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                    Avgas.CC.Utilization.in.last.12.months_woe + No.of.trades.opened.in.last.6.months_woe + 
                    No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Outstanding.Balance_woe + Total.No.of.Trades_woe + Age_woe + 
                    Income_woe + No.of.months.in.current.company_woe + Education_woe , family = "binomial", data = bal_train_data)
summary(woe_model_3)

woe_model_IV <- glm(Performance_Tag ~ Avgas.CC.Utilization.in.last.12.months_woe + 
                      No.of.trades.opened.in.last.12.months_woe + 
                      No.of.PL.trades.opened.in.last.12.months_woe + 
                      Total.No.of.Trades_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe +
                      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                      Outstanding.Balance_woe + 
                      No.of.PL.trades.opened.in.last.6.months_woe + 
                      No.of.times.90.DPD.or.worse.in.last.12.months_woe , data = bal_train_data_woe, family = "binomial")

summary(woe_model_IV)

## Final model
final_model <- woe_model_IV 

test_pred_woe <- predict(final_model, type = "response", newdata = test_data_woe)

table(test_pred_woe)

# Let's see summary of predictions
summary(test_pred_woe)


# Let's use the probability cutoff of 50%.
test_pred_attrition_woe <- factor(ifelse(test_pred_woe >= 0.50, "Yes", "No"))
test_actual_attrition_woe <- factor(ifelse(test_data_woe$Performance_Tag==1,"Yes","No"))

library(e1071)
library(caret)
test_conf <- confusionMatrix(test_pred_attrition_woe, test_actual_attrition_woe, positive = "Yes")
test_conf


perform_fn_woe <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(pred_prob_rf[,2] >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition_woe, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,0.90,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]

cutoff

# 0.5134343 0.5224242

####################################################################################
# Imputing the cutoff value, then will see the accuracy.

test_cutoff_attrition_woe <- factor(ifelse(test_pred_woe >=0.53, "Yes", "No"))

conf_final_woe <- confusionMatrix(test_cutoff_attrition_woe, test_actual_attrition, positive = "Yes")

acc <- conf_final_woe$overall[1]

sens <- conf_final_woe$byClass[1]

spec <- conf_final_woe$byClass[2]

acc   # 64.82

sens  # 60.52

spec  # 65.00


# Still we can see the accuracy has not improved much, let's now try with random forest model.

#---------------------------------------------
#---  Random forest
#---------------------------------------------

master_df_rf <- master_df_bkup

master_df_rf[, numeric_var] <- lapply(numeric_var, function(x) as.numeric(as.character(master_df_rf[, x])))

master_df_rf[, factor_var] <- lapply(factor_var, function(x) as.factor(as.character(master_df_rf[, x])))

master_df_rf$Performance_Tag <- as.factor(master_df_rf$Performance_Tag)

master_df_bkup <- master_df_rf

# master_df_rf <- ROSE(Performance_Tag ~ ., data = master_df_rf, seed = 11)$data

# Suffling the data
shuffledata <- master_df_rf[sample(nrow(master_df_rf)), ]

# Split the data into train and test
ntrain <- as.integer(nrow(shuffledata)*0.8)
train_data_rf <- shuffledata[1:ntrain, ]
test_data_rf <- shuffledata[(ntrain+1):nrow(shuffledata), ]

library(ROSE)
traindata_rf <- ROSE(Performance_Tag ~ ., data = train_data_rf, seed = 11)$data

table(traindata_rf$Performance_Tag)

#library(DMwR)
#smoted_data <- SMOTE(Performance_Tag~., data = train_data, perc.over=2000, k = 3)
#table(smoted_data$Performance_Tag)
#table(traindata$Performance_Tag)
#traindata <- smoted_data

# loading randomForest library
library(randomForest)

set.seed(999)

rf_model <- randomForest(Performance_Tag ~ ., data=traindata_rf, proximity=FALSE,
                         ntree=200, mtry=3, do.trace=TRUE, na.action=na.omit)
rf_model

# OOB estimate of  error rate: 27.91%

# Predicting on test data
testPred <- predict(rf_model, newdata=test_data_rf)

table(testPred, test_data_rf$Performance_Tag)

# Confusion metrix
test_pred_performance_rf <- factor(ifelse(testPred == 1, "Yes", "No"))

test_actual_performance_rf <- factor(ifelse(test_data_rf$Performance_Tag==1,"Yes","No"))

test_conf_lr <- confusionMatrix(test_pred_performance_rf, test_actual_performance_rf, positive = "Yes")

test_conf_lr

# 
#               Reference
# Prediction    No   Yes
#         No  12162   480
#         Yes  1212   120

# Accuracy : 0.8789  
# Sensitivity : 0.200000           
# Specificity : 0.909376 


#   Prediction in terms of probability and then will find out the optimal probalility cutoff
pred_prob_rf <- predict(rf_model, test_data_rf, type = "prob")


perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(pred_prob_rf[,2] >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_performance_rf, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,0.90,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]

cutoff

# 0.2009091 0.2079798

# Imputing the cutoff value, then will see the accuracy.

test_pred_cutoff<- factor(ifelse(pred_prob_rf[,2] >= 0.24, "yes", "no"))

test_actual_val <- factor(ifelse(test_data_rf$Performance_Tag == 1, "yes", "no"))

conf_rf <- confusionMatrix(test_pred_cutoff, test_actual_val, positive = "yes")

conf_rf


# Reference
# Prediction   no  yes
#         no  8593  224
#         yes 4762  395

# Accuracy : 0.6432             
#95% CI : (0.6352, 0.6511)   
# No Information Rate : 0.9557             
# P-Value [Acc > NIR] : 1                  

# Kappa : 0.0626             
# Mcnemar's Test P-Value : <0.0000000000000002
                                             
#            Sensitivity : 0.63813            
#            Specificity : 0.64343            


#-------------------------------------------------------------------------------------------------------
# Making model using woe value of top 9 predicators based on IV value.

library(woeBinning)
library(scorecard)

# woe binning ------ 
master_df_bin <- master_df_bkup

master_df_woe_bal <- ROSE(Performance_Tag ~ ., data = master_df_bin, seed = 11)$data

bins = woebin(master_df_woe_bal, "Performance_Tag")

# Let's see what woe bin graphs says about the top 6 variables.

woebin_plot(bins$Avgas.CC.Utilization.in.last.12.months)

woebin_plot(bins$No.of.trades.opened.in.last.6.months)

woebin_plot(bins$No.of.times.90.DPD.or.worse.in.last.6.months)

woebin_plot(bins$Total.No.of.Trades)

woebin_plot(bins$No.of.PL.trades.opened.in.last.6.months)

data_woe = woebin_ply(master_df_bin, bins)

#modelling on data_woe - logistics regression
woe_model = glm(Performance_Tag ~ ., family = binomial(), data = data_woe)

woe_model1 <- stepAIC(woe_model, direction = "both")



#modelling on woe dataframe
woe_model_final <- glm(Performance_Tag ~  Avgas.CC.Utilization.in.last.12.months_woe + No.of.PL.trades.opened.in.last.12.months_woe + 
                         Outstanding.Balance_woe +  No.of.trades.opened.in.last.12.months_woe + 
                         No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                         No.of.times.30.DPD.or.worse.in.last.6.months_woe + Total.No.of.Trades_woe
                       , family = "binomial", data = data_woe)

summary(woe_model_final)

## Final model
final_model <- woe_model_IV 


####################################################
pred_population = predict(final_model, type = "response", newdata = master_df_woe)

names(master_df_woe)

# Computing the odds for good in the given dataset. 
# The odds for the good is  Odd(good) =  (1-P(bad))/P(bad)

odds_for_good<-sapply(pred_population,function(x) (1-x)/x)

#computing  ln(odd(good))
log_odds_for_good<-sapply(odds_for_good,function(x)log(x))

#We are using the following formula for computing the application score card.
#400 + slope * (ln(odd(good)) - ln(10)) where slope is 20/log(2)

slope <-20/log(2)

application_score_card <- sapply(log_odds_for_good,function(x) 400 + slope * (x - log(10)))

#making dataframe with score card
score_card_df<-cbind(master_df_woe,application_score_card)
summary(application_score_card)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 307.4   323.3   337.4   338.4   358.2   362.5
#scores range from 307.4 to 362.5 for applicants.

#high application score indicate less risk for defaulting.
#mean score for approved customers is 338

###Let's find the cutoff score, in the final model the cuttoff was 0.51233

application_cutoff_score <- 400 + (slope * (log((1-0.51233)/0.51233) - log(10)))
application_cutoff_score

# ~ 333

ggplot(score_card_df,aes(application_score_card))+geom_histogram()
#Histogram clearly shows that the application score of applicants are below 333  are few which meets our expectation.
boxplot(application_score_card)

#No.of applicants above score 333
length(which(application_score_card>333))
# 40242
#No.of applicants below score 333
length(which(application_score_card<333))
# 29628

###############################################

# Credit loss:
# The applicants who have been selected by the bank and then defaulted are credit loss for the bank.

# Credit loss without the model
prop.table(table(master_df$Performance_Tag)) 

# 0          1 
# 0.95780736 0.04219264 
# Percentage of applicant approved without model ~ 4.2%

# With the model the credit loss 
# Percentage of applicant approved and then defaulted with model = (1425/69870) ~ 1.9%

# Reduction in credit Loss after using the model	= 4.2-1.9 ~ 2.3 %

# Approval Rate with model
length(which(application_score_card<333))/(length(which(application_score_card<333)) + length(which(application_score_card>333)))
# 42%

# Here we have reached to conclusion, CredX should target those applicant's which are having high score card.
# This will help them to reach right customer, and also save in term of credit loss.

# More conversion rate means better business.

################## END OF model on master data Analysis#####################################
######################################################################################################

#####################  Model on Demographic data ###################################################

## Loading necessary libraries
library(dplyr)
library(MASS)
library(tidyr)
library(caret)
library(ggplot2)

Demographic_data <- read.csv("Demographic data.csv",stringsAsFactors = F,na.strings = c(""," "))
str(Demographic_data)
names(Demographic_data)[4] <- 'Marital_Status'
dim(Demographic_data)  ## 71295    12

##checking duplicated values
sum(duplicated(Demographic_data))

## duplicate record found in both the dataset and reoving them
dup_recs <- Demographic_data[duplicated(Demographic_data$Application.ID)|duplicated(Demographic_data$Application.ID,fromLast = TRUE),]
# As there 6 duplicates, there is high discripancy in all attributes of duplicate records. so deleting records
Demographic_data <- Demographic_data[!(duplicated(Demographic_data$Application.ID)|duplicated(Demographic_data$Application.ID,fromLast = TRUE)),]

# removing application id
Demographic_data$Application.ID <- NULL

##checking NA's in demographic dataset
sapply(Demographic_data, function(x) sum(is.na(x)))       ## NA count

Demographic_data_backup <- Demographic_data
Demographic_data_woe <- Demographic_data_backup
##checking NA's in demographic dataset
sapply(Demographic_data_woe, function(x) sum(is.na(x)))       ## NA count

## Age is at least 18 to apply for a credit card. 
Demographic_data_woe$Age[Demographic_data_woe$Age < 18] <- 18

## Income
Demographic_data_woe$Income[Demographic_data_woe$Income < 0] <- 0

# Cut and paste the df which having Performance tag as NA 
Demographic_Target_NA1 <- Demographic_data_woe[which(is.na(Demographic_data_woe$Performance.Tag)),]
# remove records with NA target in original data frame
Demographic_data_woe <- Demographic_data_woe[which(!is.na(Demographic_data_woe$Performance.Tag)),]

## Replacing NA's with NA_<text>, as a seperate category
Demographic_data_woe[which(is.na(Demographic_data_woe$Gender)),]$Gender <- 'NA_gender'
Demographic_data_woe[which(is.na(Demographic_data_woe$Marital_Status)),]$Marital_Status <- 'NA_marital'
Demographic_data_woe[which(is.na(Demographic_data_woe$No.of.dependents)),]$No.of.dependents <- 'NA_depndents'
Demographic_data_woe[which(is.na(Demographic_data_woe$Education)),]$Education <- 'NA_education'
Demographic_data_woe[which(is.na(Demographic_data_woe$Profession)),]$Profession <- 'NA_profession'
Demographic_data_woe[which(is.na(Demographic_data_woe$Type.of.residence)),]$Type.of.residence <-'NA_residence'

### Separting numerical and categprica variables
NUMERIC_ATRS <- c("Age",
                  "Income",
                  "No.of.months.in.current.residence",
                  "No.of.months.in.current.company")

FACTOR_ATRS <- c("Gender",
                 "Marital_Status",
                 "No.of.dependents",
                 "Education",
                 "Profession",
                 "Type.of.residence")

str(Demographic_data_woe)

### Note: bin should not be in factors
Demographic_data_woe$Age_bin <- cut(Demographic_data_woe$Age, breaks = c(17,31,36,39,42,45,48,51,54,58,65))
quantile(Demographic_data_woe$Income,seq(0,1,0.1))
Demographic_data_woe$Income_bin <- cut(Demographic_data_woe$Income, breaks = c(-1,6,11,17,22,27,32,37,42,49,60))
quantile(Demographic_data_woe$No.of.months.in.current.residence,seq(0,1,0.1))
Demographic_data_woe$No.of.months.in.current.residence_bin <- cut(Demographic_data_woe$No.of.months.in.current.residence, breaks = c(5,10,29,50,73,98,126))
quantile(Demographic_data_woe$No.of.months.in.current.company,seq(0,1,0.1))
Demographic_data_woe$No.of.months.in.current.company_bin <- cut(Demographic_data_woe$No.of.months.in.current.company, breaks = c(2,6,13,20,27,34,41,48,54,62,133))

Demographic_data_woe$Performance.Tag <- as.numeric(Demographic_data_woe$Performance.Tag)
IV1 <- create_infotables(Demographic_data_woe, y="Performance.Tag", bins=10, parallel=FALSE)
IV_value1 <- data.frame(IV1$Summary)
print(IV1$Tables$Age_bin)
print(IV1$Tables$Income_bin)
print(IV1$Tables$No.of.months.in.current.residence_bin)
print(IV1$Tables$No.of.months.in.current.company_bin)

print(IV1$Tables$Gender)

sum(IV1$Tables$Age_bin$IV)
IV1$Summary

plot_infotables(IV1,"Age")
plot_infotables(IV1, IV1$Summary$Variable[1:8], same_scale=FALSE, show_values = TRUE)

Demographic_data_woe$Age_woe <- mapvalues(Demographic_data_woe$Age_bin, from = IV1$Tables$Age_bin$Age_bin, to = IV1$Tables$Age_bin$WOE)
Demographic_data_woe$Income_woe <- mapvalues(Demographic_data_woe$Income_bin, from = IV1$Tables$Income_bin$Income_bin, to = IV1$Tables$Income_bin$WOE)
Demographic_data_woe$No.of.months.in.current.residence_woe <- mapvalues(Demographic_data_woe$No.of.months.in.current.residence_bin, from = IV1$Tables$No.of.months.in.current.residence_bin$No.of.months.in.current.residence_bin, to = IV1$Tables$No.of.months.in.current.residence_bin$WOE)
Demographic_data_woe$No.of.months.in.current.company_woe <- mapvalues(Demographic_data_woe$No.of.months.in.current.company_bin, from = IV1$Tables$No.of.months.in.current.company_bin$No.of.months.in.current.company_bin, to = IV1$Tables$No.of.months.in.current.company_bin$WOE)
Demographic_data_woe$Gender_woe <- mapvalues(Demographic_data_woe$Gender, from = IV1$Tables$Gender$Gender, to = IV1$Tables$Gender$WOE)
Demographic_data_woe$Marital_Status_woe <- mapvalues(Demographic_data_woe$Marital_Status, from = IV1$Tables$Marital_Status$Marital_Status, to = IV1$Tables$Marital_Status$WOE)
Demographic_data_woe$No.of.dependents_woe <- mapvalues(Demographic_data_woe$No.of.dependents, from = IV1$Tables$No.of.dependents$No.of.dependents, to = IV1$Tables$No.of.dependents$WOE)
Demographic_data_woe$Education_woe <- mapvalues(Demographic_data_woe$Education, from = IV1$Tables$Education$Education, to = IV1$Tables$Education$WOE)
Demographic_data_woe$Profession_woe <- mapvalues(Demographic_data_woe$Profession, from = IV1$Tables$Profession$Profession, to = IV1$Tables$Profession$WOE)
Demographic_data_woe$Type.of.residence_woe <- mapvalues(Demographic_data_woe$Type.of.residence, from = IV1$Tables$Type.of.residence$Type.of.residence, to = IV1$Tables$Type.of.residence$WOE)

data.frame(colnames(Demographic_data_woe))


Demographic_data_woe1 <- Demographic_data_woe[,grep("_woe",names(Demographic_data_woe))]
Demographic_data_woe1 <- cbind(Demographic_data_woe1,Demographic_data_woe$Performance.Tag)
data.frame(colnames(Demographic_data_woe1))
str(Demographic_data_woe1)
names(Demographic_data_woe1)[11] <- 'Performance.Tag'
table(Demographic_data_woe1$Performance.Tag)
# 0      1 
# 66917  2947 
View(Demographic_data_woe1)

# Reverting 0 with 1
Demographic_data_woe1$Performance.Tag <- ifelse(Demographic_data_woe1$Performance.Tag == 0,1,0)

Demographic_data_woe1 <- as.data.frame(lapply(Demographic_data_woe1, function(x) as.numeric(paste(x))))
dim(Demographic_data_woe1)
head(Demographic_data_woe1$Age_woe)
str(Demographic_data_woe1)
str(IV1$Tables$Age_bin$WOE)

## Splitting the data into train and test datasets
install.packages("caTools")
library(caTools)
set.seed(98)
#Demographic_data_woe1 <- lapply(Demographic_data_woe1, as.numeric)

indices11 <- sample.split(Demographic_data_woe1$Performance.Tag, SplitRatio = 0.7)
train_woe <- Demographic_data_woe1[indices11,]
test_woe <- Demographic_data_woe1[!(indices11),]

table(train_woe$Performance.Tag)
# 0     1 
# 46759  2062 

train_woe$Performance.Tag <- as.factor(train_woe$Performance.Tag)
train_woe_smote <- SMOTE(Performance.Tag~.,train_woe, perc.over =1800, K = 5, perc.under = 100 )
table(train_woe_smote$Performance.Tag)

############### Logistic Regression  on ############################
###############    Demographic Data     ############################

#Initial model
dmodel_w1 = glm(Performance.Tag ~ ., data = train_woe_smote, family = "binomial")
summary(dmodel_w1)
vif(dmodel_w1)

# Stepwise selection
library("MASS")
dmodel_w2<- stepAIC(dmodel_w1, direction="both")
summary(dmodel_w2)
vif(dmodel_w2)

# Model 3
dmodel_w3 <- glm(formula = Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.residence_woe + 
                   No.of.months.in.current.company_woe + Marital_Status_woe + 
                   No.of.dependents_woe + Profession_woe , 
                 family = "binomial", data = train_woe_smote)
summary(dmodel_w3)
vif(dmodel_w3)


final_model_woe <- dmodel_w3    ## Final model
############################ Model Evaluation #########################
# Predict probabilities of test data
test_pred_woe <- predict(final_model_woe, type = "response", newdata = test_woe)

# Let's see summary of predictions
summary(test_pred_woe)
# Let's use the probability cutoff of 50%.
test_dpred_perf_woe <- factor(ifelse(test_pred_woe >= 0.5, "Yes", "No"))
test_dactual_perf_woe <- factor(ifelse(test_woe$Performance.Tag==1,"Yes","No"))
library(e1071)
test_conf_woe <- confusionMatrix(test_dpred_perf_woe, test_dactual_perf_woe, positive = "Yes")
test_conf_woe

#  let's find out optimal probability cutoff value
perform_fn_woe <- function(cutoff) 
{
  predicted_performance <- factor(ifelse(test_pred_woe >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_performance, test_dactual_perf_woe, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_pred_woe)
ss = seq(.01,.76,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn_woe(ss[i])
} 

plot(ss, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(ss,OUT[,2],col="darkgreen",lwd=2)
lines(ss,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- ss[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
# 0.4872727


##*******************************************************
## No important predicator variables found in Demographic data analysis.


