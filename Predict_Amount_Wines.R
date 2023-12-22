library(ggplot2)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(cowplot)
library(glmnet)
data_independent  <- read.csv(file = 'data - for student - independent variables.csv',na.strings=c("","NA")) 
data_dependent <- read.csv(file = 'data - for student - dependent variable.csv')
y <- data_dependent$AmountWines

# Name Varaibales #
first_name <- "Dhruv"
last_name <- "Saldanha"

### FEATURE SELECTION and Engineering ###
# Kid/Teen #
data_independent$Offspring <- ifelse((data_independent$Kid_home == 0) & (data_independent$Teen_home == 0),0,1)
data_independent$Kid_home_ <- ifelse((data_independent$Kid_home == 0),0,1)
data_independent$Teen_home_ <- ifelse((data_independent$Teen_home == 0),0,1)

# Cleaning Higher Education # 
data_independent$Highest_Education_ <- ifelse( data_independent$Highest_Education %in% c("Graduation","PhD","Master") ,1,0) # 1<- Educated = drink less; 2 <- Basic = Uneducated =  drink more
data_independent$Grad_ <- ifelse( data_independent$Highest_Education == "Graduation" ,1,0)
data_independent$PhD_ <- ifelse( data_independent$Highest_Education == "PhD" ,1,0)
data_independent$Master_ <- ifelse( data_independent$Highest_Education == "Master" ,1,0)
data_independent$NonGrad <- ifelse( data_independent$Highest_Education %in% c("2n Cycle","Basic") ,1,0)
data_independent$Graduate_ <- ifelse(data_independent$Highest_Education_ == 1,1,0)
data_independent$NonGraduate_ <- ifelse(data_independent$Highest_Education_ == 0,1,0)

# Cleaning Marital Status #
data_independent$Marital_Status_ <- ifelse(data_independent$Marital_Status %in% c("Married","Together"),1,0) # 1<- Married = drink less ; 2<- Unmarried = drink more
data_independent$Married_ <- ifelse(data_independent$Marital_Status %in% c("Married","Together"),1,0)
data_independent$UnMarried_ <- ifelse(data_independent$Marital_Status %in% c("Married","Together"),0,1)

# Cleaning Household Income #
data_independent$Household_Income_ <- parse_number(data_independent$Household_Income)
data_independent$Household_Income_[is.na(data_independent$Household_Income_)] <- mean(data_independent$Household_Income_, na.rm = TRUE)

# Selecting only numeric columns
data_independent <- data_independent %>% select_if(is.numeric)

# Lasson for selecting features
x <- data.matrix(select(data_independent,-c("Customer","Birth_Year","Kid_home","Teen_home","AmountFruits","AmountFish","Complain")))
lasso_ <- cv.glmnet(x,y,alpha = 1)
best_lambda <- lasso_$lambda.min

lasso <- glmnet(x,y,alpha = 1, lambda = 3.5)
coef(lasso)


# Train Model
model1 <- randomForest(y ~ data_independent$Household_Income_ + data_independent$Last_purchase+
          data_independent$AmountMeat+data_independent$AmountGold+data_independent$Web+data_independent$Catalog+
          data_independent$Store+data_independent$WebVisits+data_independent$ad1 +data_independent$ad2+ 
          data_independent$ad3+ data_independent$ad4+data_independent$ad5+data_independent$ad6+data_independent$Grad_+
          data_independent$PhD_+data_independent$Master_+data_independent$NonGrad+data_independent$Graduate_+
          data_independent$NonGraduate_+data_independent$Kid_home_+data_independent$AmountSweet, proximity=TRUE)


### Predicting  ###
model<-function(new_data_independent)   { 
  
  pred<-vector(length=nrow(new_data_independent)) # set up the output prediction
  pred <- predict(model1,new_data_independent)
  return(pred)
}

new_data <- data_independent %>% select ("Household_Income_","Last_purchase","AmountMeat","AmountGold","Web","Catalog",
                                         "Store","WebVisits","ad1", "ad2","ad3", "ad4","ad5","ad6","Grad_","PhD_",
                                         "Master_","NonGrad","Graduate_","NonGraduate_","Kid_home_",
                                         "AmountSweet")
preds <- model(new_data)

# RMSE
rmse<- sqrt(mean((y - preds)^2))


output<-data.frame(first_name,last_name,rmse)
write.table(output,file="finaloutput.csv",append=TRUE,col.names=FALSE,row.names=TRUE,sep=",")

source("predictdata.R")