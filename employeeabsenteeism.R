#remove previous data if any
rm(list = ls())

#set working directory
setwd("I:/DATA Scientist Assignments/Employee Absenteeism project")

#check current workinh directory
getwd()

#load some directories which will use in analysis
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)
rm(x)

#load data 
library("xlsx")
data_employee = read.xlsx("Absenteeism_at_work_Project.xls", sheetIndex = 1)
 
View(data_employee)
head(data_employee,10)
str(data_employee)
unique(data_employee$Month.of.absence)
length(colnames(data_employee))
names(data_employee)
#drop ID variables as it is not containing any significant information
data_employee = subset(data_employee, select = -(ID))

#Count the unique value of each variable
unique_val = data.frame(sapply(data_employee, function(x) length(unique(x))))


#replace 0 with NA in "Month.of.absence" variable because there is no month 0
data_employee$Month.of.absence[data_employee$Month.of.absence %in% 0] = NA

#divide Work.load.Average.day by 1000(Got to know from support)
data_employee$Work.load.Average.day. = data_employee$Work.load.Average.day./1000

#convert categorcal variable type
data_employee$Reason.for.absence = as.factor(data_employee$Reason.for.absence)
data_employee$Month.of.absence = as.factor(data_employee$Month.of.absence)
data_employee$Day.of.the.week = as.factor(data_employee$Day.of.the.week)
data_employee$Seasons = as.factor(data_employee$Seasons)
data_employee$Disciplinary.failure = as.factor(data_employee$Disciplinary.failure)
data_employee$Education = as.factor(data_employee$Education)
data_employee$Son = as.factor(data_employee$Son)
data_employee$Social.drinker = as.factor(data_employee$Social.drinker)
data_employee$Social.smoker = as.factor(data_employee$Social.smoker)
data_employee$Pet = as.factor(data_employee$Pet)

#divide continous and categorical variables
cnames = c("Transportation expense", "Distance from Residence to Work", "Service time", "Age", 
          "Work load Average/day", "Hit target", "Weight", "Body mass index", "Absenteeism time in hours")
cat_names = c("Reason for absence", "Month of absence", "Day of the week", "Seasons", "Disciplinary failure", "Education", 
              "Son", "Social drinker", "Social smoker", "Pet")

unique(data_employee$Reason.for.absence)
ggplot(data_employee, aes(x = Transportation.expense)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 3, by = 0.5), 
                 fill="blue",
                 alpha = .4,position="dodge") + 
  geom_density(col=4) +
  labs(title="Distribution of Transportation expense") +
  labs(x="Transportation expense", y="Density of Transportation expense") +
  theme(legend.position="top")


#------------------------------------------Data Pre Processing------------------------------------#
#Missing value analysis
#check weather target variables have missing value or not
sum(is.na(data_employee$Absenteeism.time.in.hours))
#remove those observations which "Absenteeism time in hours" has missing values
data_employee = data_employee[(!data_employee$Absenteeism.time.in.hours %in% NA),]

missingValue = data.frame(apply(data_employee,2, function(x){sum(is.na(x))}))

missingValue$Columns = row.names(missingValue)
names(missingValue)[1] =  "Missing_percentage"
missingValue$Missing_percentage = (missingValue$Missing_percentage/nrow(data_employee)) * 100
missingValue = missingValue[order(-missingValue$Missing_percentage),]
row.names(missingValue) = NULL
missingValue = missingValue[,c(2,1)]
write.csv(missingValue, "Miising_perc.csv", row.names = F)

#visualise missing values
 ggplot(data = missingValue[1:20,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
   geom_bar(stat = "identity",fill = "blue")+xlab("Parameter")+
   ggtitle("Missing data percentage") + theme_bw()

#applying mode method for categorical variables
 mode = function(x){
  uni = unique(x)
  uni[which.max(tabulate(match(x, uni)))]
}
 
for(i in cat_names){
  print(i)
  data_employee[,i][is.na(data_employee[,i])] = mode(data_employee[i])
} 
 
data_employee$Transportation.expense[40]
#Actual value = 179
#Mean  = 220.4613
#median = 225
#KNN = 179

#mean Imputation
data_employee$Transportation.expense[40] = NA
data_employee$Transportation.expense[is.na(data_employee$Transportation.expense)] = mean(data_employee$Transportation.expense, na.rm = T)
data_employee$Transportation.expense[40]

#Median imputation
data_employee$Transportation.expense[40] = NA
data_employee$Transportation.expense[is.na(data_employee$Transportation.expense)] = median(data_employee$Transportation.expense, na.rm = T)
data_employee$Transportation.expense[40]

#knn imputation
data_employee$Transportation.expense[40] = NA
data_employee = knnImputation(data_employee, k = 3)
data_employee$Transportation.expense[40]

#so we observed that with knn imputation we get acurate value so we will go with knn imputation
sum(is.na(data_employee))

#-------------Outlier analysis-------------------#
#data manupulation: convert string categories into factor numeric
for(i in 1:ncol(data_employee)){
  if(class(data_employee[,i]) == 'factor'){
    data_employee[,i] = factor(data_employee[,i], labels = (1:length(levels(factor(data_employee[,i])))))
  }
}  
rm(i)

#Boxplot to find outlier 
library(ggplot2)
number_index = sapply(data_employee, is.numeric)
numeric_data = data_employee[, number_index]
cnames = colnames(numeric_data)
for(i in 1:length(cnames)){
  assign(paste0("DB", i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(data_employee))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot for",cnames[i])))
}
rm(i)

## Plotting plots together
gridExtra::grid.arrange(DB1, DB2,DB3, ncol=3)
gridExtra::grid.arrange(DB4,DB5,DB6, ncol=3)
gridExtra::grid.arrange(DB7,DB8,DB9,DB10, ncol=4)

#Remove outlier using boxplot
temp = data_employee
data_employee = temp

for (i in cnames){
  val = data_employee[,i][data_employee[,i] %in% boxplot.stats(data_employee[,i])$out]
  print(val)
  data_employee[,i][data_employee[,i] %in% val] = NA
}
sum(is.na(data_employee$Height))

#replace NA with knn imputaion
data_employee = knnImputation(data_employee, k = 3)

#----------------feature selection-----------------#
#Correltion analysis for continous variales
corrgram(data_employee[,number_index], order = F ,upper.panel = panel.pie,
         text.panel = panel.txt, main ="correlation plot for numeric variables")

#we can observed that body mass and weight are highly correlated with each other

#ANOVA test for categorical variables
factorVal = sapply(data_employee, is.factor)
factorVariables = data_employee[, factorVal]
cat_variables = names(factorVariables)
for(i in cat_variables){
  print(i)
  anovaresult = summary(aov(formula = Absenteeism.time.in.hours~data_employee[,i],data_employee))
  print(anovaresult)
}
 
#Now we will remove the values which are highly correlated to eanch other and have >0.05 p value
data_employee = subset(data_employee, select = -c(Weight,Social.smoker,Education,Seasons,Day.of.the.week))
dim(data_employee)

#-------------feature scaling----------------------#
cat_del_ind = sapply(data_employee, is.numeric)
cat_del = data_employee[, cat_del_ind]
cnames_del = names(cat_del)
#skewness test
library(propagate)
for(i in cnames_del){
  print(i)
  skew = skewness(data_employee[,i])
  print(skew)
}
hist(data_employee$Transportation.expense, col = "blue", xlab = "Transportation.expense", ylab = "Frequency",
     main = "histogram of Transportation.expense")
hist(data_employee$Distance.from.Residence.to.Work, col = "blue", xlab = "Distance.from.Residence.to.Work", ylab = "Frequency",
     main = "histogram of Distance.from.Residence.to.Work")
hist(data_employee$Service.time, col = "blue", xlab = "Service.time", ylab = "Frequency",
     main = "histogram of Service.time")
hist(data_employee$Absenteeism.time.in.hours, col = "blue", xlab = "Absenteeism.time.in.hours", ylab = "Frequency",
     main = "histogram of Absenteeism.time.in.hours")

#logtransform
data_employee$Absenteeism.time.in.hours = log1p(data_employee$Absenteeism.time.in.hours)

#from above histograms we can say that data is not normally distributed so for that normalisation is best way
#Normalization
for(i in cnames_del){
  if(i != "Absenteeism.time.in.hours"){
    print(i)
    data_employee[,i] = (data_employee[,i] - min(data_employee[,i]))/(max(data_employee[,i]) - min(data_employee[,i]))
    print(data_employee[,i])
  }
}
rm(i)

#summary
for(i in cnames_del){
  print(summary(data_employee[,i]))
}
#as summary the data is in now normalised form 
#write the pre processed data to drive
write.csv(data_employee, "data_employee.csv", row.names = FALSE)



#-------------------------Model Development----------------------#
#Clean the environment
rmExcept("data_employee")

#Divide data into train and test using stratified sampling method
set.seed(6789)
train.index = sample(1:nrow(data_employee), .80 * nrow(data_employee))
train = data_employee[ train.index,]
test  = data_employee[-train.index,]

#RMSE
rmse = function(y,y1){
  sqrt(mean(abs(y-y1)^2))
}

#R square
rsquare = function(y,y1){
  cor(y,y1)^2
}

#---------------------Decision tree---------------#
#Load Libraries
library(rpart)
library(MASS)

#rpart for regression
DT_model = rpart(Absenteeism.time.in.hours ~ ., data = train, method = "anova")

#Predict for train cases
train_DT = predict(DT_model, train[-15])

#predict for test cases
test_DT = predict(DT_model, test[-15])

#rmse 
RMSE_DT_train = (rmse(train[,15], train_DT))
#RMSE_DT_train = 0.4296919
RMSE_DT_test = (rmse(test[,15], test_DT))
#MAPE_DT_test = 0.462681

#Rsquare
rquare_train_DT = rsquare(train[,15], train_DT)
#rquare_train = 0.5732321
rquare_test_DT = rsquare(test[,15], test_DT)
#rquare_test =  0.4682043


#------------------Random Forest-----------------#
library(randomForest)
#delevelop model using random forest
RF_model = randomForest(Absenteeism.time.in.hours~., data_employee, nTree = 500, importance = TRUE)

#apply on train data
RF_train_predict = predict(RF_model, train[,-15]) 

#apply on test data
RF_test_predict = predict(RF_model, test[,-15])

#RMSE for train
RF_RMSE_train = (rmse(train[,15], RF_train_predict))
#RMSE  0.2439306

#RMSE for test
RF_RMSE_test = (rmse(test[,15], RF_test_predict))
#RMSE 0.2704545

#RSquare for train
RSquare_train_RF= rsquare(train[,15], RF_train_predict)
#Rsquare 0.8830654

#RSquare for test
RSquare_test_RF = rsquare(test[,15], RF_test_predict)
#Rsquare  0.8290696

#----------------------Linear Regression----------------#
library(usdm)
colnames(data_employee)
cnames = c("Transportation.expense", "Distance.from.Residence.to.Work" ,"Service.time", "Age",
           "Work.load.Average.day.", "Hit.target", "Height","Body.mass.index", "Absenteeism.time.in.hours")
vif(data_employee[,cnames])
vifcor(data_employee[,cnames], th = 0.7)

#develop linear regression model
LR_model = lm(Absenteeism.time.in.hours~., data = train)
summary(LR_model)

#apply on train data
LR_train = predict(LR_model, train[,-15])

#apply on test
LR_test = predict(LR_model, test[,-15])

#RMSE for train
LR_RMSE_train = (rmse(train[,15],LR_train))
#RMSE  0.4168058

#RMSE for test
LR_RMSE_test = (RMSE(test[,15], LR_test))
#RMSE 0.4336782

#Rsquare for train
rsquare_train_LR = rsquare(train[,15],LR_train)
#0.5984452

#Rsquare for test
rsquare_test_LR = rsquare(test[,15], LR_test)
#0.5307052

#------------------Result-------------------#
result = data.frame(Model = c('Decision Tree', 'Ramdon Forest', 'Linear Regression'),
                    'MAPE Train' = c(RMSE_DT_train,RF_RMSE_train,LR_RMSE_train),
                    'MAPE Test' = c(RMSE_DT_test,RF_RMSE_test,LR_RMSE_test),
                    'RSquare Train' = c(rquare_train_DT,RSquare_train_RF,rsquare_train_LR),
                    'RSquare Test' = c(rquare_test_DT,RSquare_test_RF,rsquare_test_LR))
write.csv(result, "Result,csv", row.names = FALSE)


###############Thank You#####################







