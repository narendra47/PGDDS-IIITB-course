
###########################################################################################################################

##-------------------------------      Linear Regression Assignment      ------------------------------------------------##

###########################################################################################################################

##-- Installing and Loading required R library

install.packages("CARS")
install.packages("MASS")

library(CARS)
library(MASS)
library(stringr)
library(dplyr)

##--  Loading car price csv file to workspace

car_price <- read.csv("CarPrice_Assignment.csv")

View(car_price)

summary(car_price)

##---------------------------------------------------------------------------------------------------------------------##
##--  Data Cleaning and preparation
##---------------------------------------------------------------------------------------------------------------------##

## As mentioned in the data preparation section, taking only car company name rather full name of car and model

car_price$CarName <- str_extract(car_price$CarName, '\\S+')

View(car_price)

##  By Looking at the data it is found that few carname is mistakenely mis-spelled/shortened, let see different car company here

unique(car_price$CarName)

## Here mazda is wrongly named as maxda, and same is in case of volkswagen, toyota and porsche. 
## Also some car name is in caps, and shortend. Correcting all of them, and give uniform/correct name.

car_name_correction <- function(x){
                        if(x == "maxda"){ 
                          "mazda"
                        } else if(x == "porcshce"){ 
                          "porsche"
                        } else if(x == "toyouta"){ 
                          "toyota"
                        } else if(x == "vokswagen"){ 
                          "volkswagen"
                        } else if(x == "vw"){ 
                          "volkswagen"
                        } else x
              }
car_price$CarName <- sapply(car_price$CarName, car_name_correction)

car_price$CarName <- str_to_lower(car_price$CarName)

unique(car_price$CarName)

str(car_price)

##--  As we can see there are categorical variable, let's convert them into dummy variables one by one
##--  For fuel type lets convert them into binary data, having 1 as gas and 0 as diesel, for aspiration std as 1 and turbo 0, 
##--  for enginelocation front 1 and rear 0, for doornumber four as 1 and two as 0

levels(car_price$fueltype)<- c(0,1)
levels(car_price$aspiration)<- c(1,0)
levels(car_price$enginelocation)<- c(1,0)
levels(car_price$doornumber) <- c(1,0)

##--  Converting all above level variables to numeric, as it is still in factor.
car_price$fueltype <- as.numeric(car_price$fueltype)
car_price$aspiration <- as.numeric(car_price$aspiration)
car_price$enginelocation <- as.numeric(car_price$enginelocation)
car_price$doornumber <- as.numeric(car_price$doornumber)


##--  Converting more than 2 categorical variables to dummy variables

dummy_carbody <- data.frame(model.matrix(~carbody, data = car_price))
dummy_carbody <- dummy_carbody[,-1]

dummy_drivewheel <- data.frame(model.matrix(~drivewheel, data = car_price))
dummy_drivewheel <- dummy_drivewheel[,-1]

dummy_enginetype <- data.frame(model.matrix(~enginetype, data = car_price))
dummy_enginetype <- dummy_enginetype[,-1]

dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem, data = car_price))
dummy_fuelsystem <- dummy_fuelsystem[,-1]

dummy_carname <- data.frame(model.matrix(~CarName, data=car_price))
dummy_carname <- dummy_carname[,-1]

dummy_cylindernum <- data.frame(model.matrix(~cylindernumber, data = car_price))
dummy_cylindernum <- dummy_cylindernum[,-1]

car_price_1<- cbind(car_price[,-c(3,7,15,18)], dummy_carname, dummy_fuelsystem, dummy_enginetype, dummy_drivewheel, dummy_carbody,dummy_cylindernum)

##--  Converting cylinder number into integer, as it may be one of useful variable.

convert_cylindernumber_to_int <- function(x){
                                    if(x == "eight"){ 8 } 
                                    else if(x == "five"){ 5 } 
                                    else if(x == "four"){ 4 }
                                    else if(x == "six"){ 6 }
                                    else if(x == "three"){ 3 }
                                    else if(x == "twelve"){ 12 } 
                                    else if(x == "two"){ 2 }
                                  }

car_price_1$cylindernumber <- sapply(car_price_1$cylindernumber, convert_cylindernumber_to_int)

##--  By looking at data four wheel drive is one of promising variable, but converting them into dummy variable we looses this option, so converting 4wd to 4 and rest as 2.
wheel_drive <- function(x){
  if(x == "4wd"){ 4
  } else 2
}

car_price_1$drivewheel <- sapply(car_price_1$drivewheel, wheel_drive)

str(car_price_1)

car_price_2 <- car_price_1

##--  Deriving new matrix for further analysis, such as calculating average milage of the car, weight to power ration,
##--  milage to power ratio and total area required for parking

car_price_1$avgmpg <- round((car_price_1$highwaympg+car_price_1$citympg)/2)

car_price_1$wtpower_ratio <- round(car_price_1$curbweight/car_price_1$horsepower)

car_price_1$mpgpower_ratio <- round(car_price_1$avgmpg/car_price_1$horsepower)

car_price_1$parking_space_req <- round(car_price_1$carheight*car_price_1$carlength * car_price_1$carwidth)

##--  Setting seed value to 100 for repoducing same result.
set.seed(1000)

##--  Taking 70% of data for making the model, rest 30% for testing the same.
training_indices <- sample(1:nrow(car_price_1), 0.7*nrow(car_price_1)) 

training_data <- car_price_1[training_indices,]
test_data <- car_price_1[-training_indices, ]

summary(training_data)
##--  Removing few columns from training data which have all value as 
##which(mean(training_data$fuelsystemidi))

##--  Disable scintific notion

options(scipen = 999)

##--  Let's build model containing all variable

model_1 <- lm(price~., data = training_data)
summary(model_1)

##--  So the model has got very good R-squared(0.976) and adjusted R-squared (0.959), but it has got too many variables. 
##--  As we studies, there are multiple method to remove variables from the dataset, such as backward, forward and stepwise selection.
##--  Since we have 72 variables, backward and forward selection method would take much more time to remove each insignificant variables
##--  Here I am using stepAIC function (stepwise selection method) to remove variables.

calculate_aic <- stepAIC(model_1, direction = "both")

##--  After calculating AIC, it has marked which all variables should be removed, let's include only leftout variable to build 
##--  the subsequent model for further analysis.

model_2 <- lm(formula = price ~ doornumber + fuelsystemspdi + fueltype + drivewheel + fuelsystemmpfi +
                wheelbase + enginetypel + compressionratio + stroke + horsepower + cylindernumberfour +
                carbodysedan + parking_space_req + carlength + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + car_ID + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNametoyota + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda + enginesize + CarNamebmw + CarNamedodge, data = training_data)
summary(model_2)

##--  Let's check multicollinearity using vif function

vif(model_2)

##-- Let's remove variables having high VIF value, 

model_3 <- lm(formula = price ~ doornumber + fuelsystemspdi + fueltype + drivewheel + fuelsystemmpfi +
                wheelbase + enginetypel + compressionratio + stroke + horsepower + cylindernumberfour +
                carbodysedan + parking_space_req + carlength + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda + enginesize + CarNamebmw + CarNamedodge, data = training_data)
summary(model_3)

vif(model_3)

##--  Repeating same step as above, and remove variables having high VIF

model_4 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + fuelsystemmpfi +
                wheelbase + enginetypel + compressionratio + stroke + horsepower + cylindernumberfour +
                carbodysedan + parking_space_req + carlength + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda + enginesize + CarNamebmw + CarNamedodge, data = training_data)

summary(model_4)

vif(model_4)

##--  Removing car length as it has high correlation with parking space required, and it has low significance value. So removing it from the set.

model_5 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + fuelsystemmpfi +
                wheelbase + enginetypel + compressionratio + stroke + horsepower + cylindernumberfour +
                carbodysedan + parking_space_req + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda + enginesize + CarNamebmw + CarNamedodge, data = training_data)

summary(model_5)

vif(model_5)

##--  Removing Engine size as it has high VIF value, and low significance
model_6 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + fuelsystemmpfi +
                wheelbase + enginetypel + compressionratio + stroke + horsepower + cylindernumberfour +
                carbodysedan + parking_space_req + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)

summary(model_6)

vif(model_6)

##-- Removing cylindernumberfour and other variables having high VIF, and then building the model.
model_7 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + fuelsystemmpfi +
                wheelbase + enginetypel + compressionratio + stroke + horsepower +
                carbodysedan + parking_space_req + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_7)

vif(model_7)

##-- Removing enginetypel and other variables having high VIF, and then building the model.
model_8 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + fuelsystemmpfi +
                wheelbase + compressionratio + stroke + horsepower + parking_space_req +
                carbodysedan  + CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                carbodyhatchback + CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_8)

vif(model_8)

##-- Removing carbodysedan,  carbodyhatchback, compressionratio, fuelsystemmpfi variables having high VIF, and then building the model.
model_9 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + parking_space_req +
                wheelbase + stroke + horsepower +
                CarNameporsche + CarNamebuick + carbodywagon +
                fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_9)

vif(model_9)

##-- Removing stroke and CarNameporsche as they have high VIF, and then building the model.
model_10 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + parking_space_req +
                 wheelbase + horsepower + CarNamebuick + carbodywagon +
                 fuelsystem4bbl + carbodyhardtop + cylindernumber + aspiration + carheight + enginelocation +
                 CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                 CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                 peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio+
                 CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_10)

vif(model_10)

##-- Removing cylindernumber 
model_11 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + parking_space_req +
                 wheelbase + horsepower + CarNamebuick + carbodywagon +
                 fuelsystem4bbl + carbodyhardtop + aspiration + carheight + enginelocation +
                 CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                 CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                 peakrpm + CarNameplymouth + boreratio + CarNamemitsubishi + CarNamepeugeot + wtpower_ratio +
                 CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_11)

vif(model_11)

##-- Removing  boreratio and wtpower_ratio due to their high vif value.
model_12 <- lm(formula = price ~ doornumber + fuelsystemspdi + drivewheel + parking_space_req+
                 wheelbase + horsepower + CarNamebuick + carbodywagon +
                 fuelsystem4bbl + carbodyhardtop + aspiration + carheight + enginelocation +
                 CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                 CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                 peakrpm + CarNameplymouth + CarNamemitsubishi + CarNamepeugeot +
                 CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_12)

vif(model_12)

##-- Removing  fuelsystemspdi, wheelbase, carbodyhardtop and carheight as all of them has high VIF and low significance
model_13 <- lm(formula = price ~ doornumber + drivewheel + parking_space_req +
                 horsepower + CarNamebuick + carbodywagon +
                 fuelsystem4bbl + aspiration + enginelocation +
                 CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar + CarNameisuzu +
                 CarNamevolkswagen + CarNamechevrolet + CarNamerenault + CarNamenissan + CarNamemazda + 
                 peakrpm + CarNameplymouth + CarNamemitsubishi + CarNamepeugeot +
                 CarNamehonda +  CarNamebmw + CarNamedodge, data = training_data)
summary(model_13)

vif(model_13)

##--  Removing CarNamenissan, CarNamemazda, CarNamehonda, doornumber, CarNameisuzu, CarNamevolkswagen, CarNamechevrolet, CarNamerenault, CarNamedodge
##--  all of these variables are not having significant value

model_14 <- lm(formula = price ~ drivewheel + parking_space_req +
                 horsepower + CarNamebuick + carbodywagon +
                 fuelsystem4bbl + aspiration + enginelocation +
                 CarNamesaab + CarNamesubaru + CarNamevolvo + CarNamejaguar  +
                 peakrpm + CarNameplymouth + CarNamemitsubishi + CarNamepeugeot +
                 CarNamebmw, data = training_data)
summary(model_14)

vif(model_14)

##--  Removing CarNameplymouth, fuelsystem4bbl, CarNamesaab

model_15 <- lm(formula = price ~ drivewheel + horsepower + CarNamebuick + carbodywagon +
                 aspiration + enginelocation + parking_space_req +
                 CarNamesubaru + CarNamevolvo + CarNamejaguar  +
                 CarNamemitsubishi + CarNamepeugeot + peakrpm +
                 CarNamebmw, data = training_data)
summary(model_15)

vif(model_15)

##--  Removing aspiration, carbodywagon, CarNamemitsubishi, CarNamesubaru

model_16 <- lm(formula = price ~ drivewheel + horsepower + CarNamebuick + 
                 enginelocation + CarNamevolvo + CarNamejaguar  + parking_space_req + 
                 CarNamebmw, data = training_data)
summary(model_16)

vif(model_16)

##--  Removing drivewheel and CarNamevolvo due to their low significant value

model_17 <- lm(formula = price ~  horsepower + CarNamebuick + 
                 enginelocation +  CarNamejaguar  + parking_space_req + 
                 CarNamebmw, data = training_data)
summary(model_17)

vif(model_17)




##--  Multicollinearity of horsepower number is still more than 1.5 but it's significant, so not removing it.
##--  The model_17 has really good R-squared value 0.901 and adjusted R value of 0.896, so I can say that it is my final model,
##--  which has six variables, horsepower, CarNamebuick,enginelocation, CarNamejaguar, parking_space_req, CarNamebmw. Here parking_space_req is derived matrix from 
##--  given data, in last it is one of key variable to predict the price.
##--  Let's take test data to predict, and see how the model predicts the car price.

predict_price <- predict(model_17, test_data[,-22])
test_data$test_price <- predict_price

r <- cor(test_data$price, test_data$test_price)
r_quared <- r^2

##  The high correlation 0.91 shows that the model is able to predict almost similar car price.
