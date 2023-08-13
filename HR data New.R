setwd("C:/R/Codes/Data sets")

library(dplyr)
library(car)
hr_train=read.csv("hr_train.csv")
head(hr_train)
hr_test=read.csv("hr_test.csv")

##You will need same set of vars on both train and test,its easier to manage that if you combine train and test
##in the beginning and then separate them once you are done with data preparation
##We'll fill test's response column with NA
hr_test$left= NA

hr_train$data = 'train'
hr_test$data = 'test'

all= rbind(hr_train,hr_test)

apply(all,2,function(x) length(unique(x)))
glimpse(all)

#Next we'll create dummy variables for remaining categorical variables

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 

all$promotion_last_5years = as.factor(all$promotion_last_5years)
all$Work_accident = as.factor(all$Work_accident)

#Remove the extra varaiables added before
hr_train = all %>% filter(data == 'train') %>% select(-data) 
hr_test= all %>% filter(data == 'test') %>% select(-left, -data) 

#perform VIF and remove variables with high multicollinearity 
for_vif = lm(left ~., data = hr_train) 
vif(for_vif)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif = lm(left ~.-sales, data = hr_train) 
sort(vif(for_vif),decreasing = T)[1:3]

#Use Random Forest model to predict the values
library(randomForest)

fit_hr= randomForest(as.factor(left)~.,data=hr_train)
fit_hr
importance(fit_hr)
varImpPlot(fit_hr)

score=predict(fit_hr,newdata= hr_test, type="prob")[,2]
write.csv(test.pred, "submision1.csv", row.names = F)






















#1st question - Find the variance of the target variable 'Price'
var(re_train$Price)

##2nd question - Find out how many observations have missing values for variable 'YearBuilt'?
library(dplyr)
sum(is.na(re_train$YearBuilt))

##3rd question - What is the difference in average price between house type h and t?
mean(re_train$Type_h)-mean(re_train$Type_t)

##4th - How many unique values variable postcode takes?
unique(re_train$Postcode)

#5th - how should you treat post code . As a categorical variable or numeric variable ? 
#Ans - numeric

#6th - Does distance follow a normal distribution?
#Ans - No
library(dplyr)
library(ggplot2)
ggplot(re_train,aes(x=Distance))+geom_histogram()










