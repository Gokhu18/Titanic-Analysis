#Set the working directory----
setwd("C:/Umaima/Data Science/Kaggle/Titanic/dataset")

getwd()

#1. Loading dataset-----------------------------------------------------------------------
train_file <- "train.csv"
train_ds = read.csv(train_file)

test_ds <- read.csv("test.csv")

-----------------------------------------------------------------------------------------------
#2. Exploratory Data Analysis ----
#2.a View data, summary-----------------------------------------------------------------------
head(train_ds)
View(train_ds)

summary(train_ds)

dim(train_ds)

str(train_ds)

colSums(is.na(train_ds))

colSums(is.(train_ds))

#2.b Checking the Gender of Passengers-----------------------------------------------------------------
table(train_ds$Sex)

plot(train_ds$Sex)

# Insight -- There are more Males than Females

#2.c Checking the Class of Passengers -----------------------
levels(train_ds$Pclass)
#PClass is defined as int. Converting it to factors

train_ds$Pclass <- factor(train_ds$Pclass)

levels(train_ds$Pclass)

table(train_ds$Pclass)
prop.table(table(train_ds$Pclass))

#Insight -- The are more passengers of Lower Class (3) than First Class.
# 55% Passengers are of Lower Class, 21% are of Middle Class, 24% are of First Class

num_pclass <- as.numeric(train_ds$Pclass)
vals <- table((train_ds$Pclass))
percent_vals <- round(prop.table(vals),2)*100
bar <- barplot(table(train_ds$Pclass))
text(x = bar, label = vals, pos = 3, cex = 1.0, col = "red")
text(x = bar, label = percent_vals , pos = 2, cex = 1.0, col = "blue")

#2.d Age analysis ----------------------------------------
summary(train_ds$Age)
str(train_ds$Age)

plot(train_ds$Age)

table(train_ds$Age)

barplot(table(train_ds$Age))

#Insight -- We find a bell shaped curve in the distribution of Age. 
# Highest no of passengers fall in the age group of 18-30
# Also there are 177 passenges (20%) who Age is not defined

#2.e SibSp - Defines no of siblings / spouse with the passenger----
summary(train_ds$SibSp)
str(train_ds$SibSp)
table(train_ds$SibSp)
percent_Sibsp <- round(prop.table(table(train_ds$SibSp)),2)*100
plot(train_ds$SibSp)
bar_Sibsp <- barplot(percent_Sibsp)
text(x=bar_Sibsp, labels = percent_Sibsp, pos=3, cex=1.0, col="red")

#Insight -- 68% passengers are travelling without Sibling/spouse 
#23% are travelling with 1 person which could possibly be a spouse
#Rest 9% are travelling with their siblings



#2.f Parch - Defines no of Parents / children with the passenger----
summary(train_ds$Parch)
str(train_ds$Parch)
sum(is.na(train_ds$Parch)) 

table(train_ds$Parch)
percent_Parch <- round(prop.table(table(train_ds$Parch)),2)*100
percent_Parch

plot(train_ds$Parch)

bar_Parch <- barplot(percent_Parch)
text(x = bar_Parch, labels = percent_Parch, pos=3, cex=1.0, col="red")

#Insights -- 76% of passengers are travelling without parent/children
#13% are travelling with 1 parent/children
#Rest 11% are travelling with >1 and <= 6 parent/children

#8. Embarked - Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton
summary(train_ds$Embarked)
str(train_ds$Embarked)

percent_emb <- round(prop.table(table(train_ds$Embarked)),2)*100
percent_emb

bar_emb <- plot(train_ds$Embarked)
text(x=bar_emb, labels = percent_emb, pos=3, cex=1.0, col = "red")

#Insights - 72% passengers embarked from Southampton

#2.g Cabin ----
summary(train_ds$Cabin)
pclass_cabin_ds <- table(train_ds$Pclass, train_ds$Cabin)
str(train_ds$Cabin)

barplot(pclass_cabin_ds)

#2.h Survived ----
summary(train_ds$Survived)
str(train_ds$Survived)

train_ds$Survived <- factor(train_ds$Survived)

table(train_ds$Survived)

percent_survived <- round(prop.table(table(train_ds$Survived)),2)*100
percent_survived

#Insight -- 38% passengers survived and 62% did not survive

#2.i Survival based on Gender ----

surv_gender_ds <- table(train_ds$Survived, train_ds$Sex)

#percent_surv_gend <- round(prop.table(table(train_ds$Survived, train_ds$Sex)),2)*100

bar_surv_gen <- barplot(surv_gender_ds, beside = TRUE)
surv_gender_ds
legend("right",title = "Survived", legend = unique(train_ds$Survived), fill = c("black","grey"))
#text(x=bar_surv_gen, labels = surv_gender_ds, cex=1.0, col = "red")

#Insight -- More females survived than males

#2.j Survival Based on Class ----

surv_class_ds <- table(train_ds$Survived, train_ds$Pclass)
surv_class_ds

bar_surv_class <- barplot(surv_class_ds, beside = TRUE)
legend("top", title = "Survived", legend = unique(train_ds$Survived), fill = c("black","grey"), box.lty = 0)

#Insights -- Upper class passengers survived more than Lower class. 
#Most Lower class passengers drowned
#Almost equal no of Middle class passengers survived as that drowned

#2.k Survival Based on Age ----

train_ds$age_grouping <- cut(train_ds$Age, breaks=c(0,10,20,30,40,50,60,70,80))

View(train_ds)

str(train_ds$age_grouping)

surv_age_ds <- table(train_ds$Survived, train_ds$age_grouping)
surv_age_ds

bar_surv_age <- barplot(surv_age_ds, beside=TRUE)
legend("right", title = "Survived", legend = unique(train_ds$Survived), fill = c("black","grey"), box.lty = 0)

#Insight -- More passsengers in the Age group 20-30 did not survive
# Most infants <10 yeas of age survived
#

#12. Survival based on Emabarkation Port 

surv_emb_ds <- table(train_ds$Survived, train_ds$Embarked)
surv_emb_ds

barplot(surv_emb_ds, beside=TRUE)
legend("top", title = "Survived", legend = unique(train_ds$Survived), fill = c("black","grey"), box.lty = 0)

#Insights -- Most people from Cherbourg port survived
#Most people from Southampton port did not survive


#2.l Survival Based on Sibsp i.e. no of Sibling and Spouse----

str(train_ds$SibSp)

surv_sibsp_ds <- table(train_ds$Survived,train_ds$SibSp)

barplot(surv_sibsp_ds, beside=TRUE)
legend("top", title = "Survived", legend = unique((train_ds$Survived)), fill=c("black","grey"), box.lty = 0)

#Insights -- Most passengers with NO Siblings or spouse did not survive i.e. single passengers drowned
#More passengers with 1 Sibling  / Spouse survived
#Passengers with >1 Sibling  / Spouse did not survive

#2.m Survival Based on Parch i.e. no of Parents and children----

str(train_ds$Parch)

surv_parch_ds <- table(train_ds$Survived, train_ds$Parch)

surv_parch_ds

barplot(surv_parch_ds, beside = TRUE)

#Insights -- Most Passengers with no parents or children drowned
# More Passengers with 1 parent/child survived



------------------------------------------------------------------------------------------------
#3 Using Feature Engineering to simplify the data for the classifier ----
#3.a For Name field, extract Title i.e. Mr., Mrs, etc ----

#Combining the training and testing data
train_test_data <- merge(train_ds, test_ds, all = TRUE)

View(train_test_data)

extractTitle <- function(x) {
  for(i in 1:nrow(x)) {
    string <- x[i,"Name"]
    x[i,"Title"] <- gsub('(.*, )|(\\..*)', '', string)
  }
  return(x)
}

train_test_data <- extractTitle(train_test_data)
str(train_test_data$Title)
table(train_test_data$Title)

train_test_data$Title <- factor(train_test_data$Title)
levels(train_test_data$Title)

#Combine the factors to 4 levels Mr - 3, Mrs - 4, Miss - 2, Others - 1
#used for combining factors
install.packages("forcats")
library(forcats)
train_test_data$Title <- train_test_data$Title %>%
                        fct_collapse(Others = c("Capt","Col","Don","Dona","Mme","Ms","Rev","Sir",
                                "Dr","Jonkheer","Lady","Major","Master","Mlle","the Countess"))

#Above step is not reqd i.e. fct_collapse...
#in the below step "Others" can be replaced with "Capt","Col","Don", etc....
train_test_data$Title <- factor(train_test_data$Title,
                                levels = c("Others","Miss","Mr","Mrs"),
                                labels = c("3", "2", "0", "1"))

train_ds <- extractTitle(train_ds)

train_ds$Title <- train_ds$Title %>%
  fct_collapse(Others = c("Capt","Col","Don","Dona","Mme","Ms","Rev","Sir",
                          "Dr","Jonkheer","Lady","Major","Master","Mlle","the Countess"))

train_ds$Title <- factor(train_ds$Title,
                                levels = c("Others","Miss","Mr","Mrs"),
                                labels = c("3", "2", "0", "1"))



# train_ds$Title <- factor(train_ds$Title, 
#                                 levels = c("Miss","Mr","Mrs","Capt","Col","Don","Dona","Mme","Ms","Rev","Sir",
#                                            "Dr","Jonkheer","Lady","Major","Master","Mlle","the Countess"), 
#                                 labels = c("2", "0", "1","3","3","3","3","3","3","3","3","3","3","3","3","3","3","3"))

View(train_ds)

test_ds <- extractTitle(test_ds)

test_ds$Title <- test_ds$Title %>%
  fct_collapse(Others = c("Capt","Col","Don","Dona","Mme","Ms","Rev","Sir",
                          "Dr","Jonkheer","Lady","Major","Master","Mlle","the Countess"))

test_ds$Title <- factor(test_ds$Title,
                         levels = c("Others","Miss","Mr","Mrs"),
                         labels = c("3", "2", "0", "1"))
View(test_ds)

#drop the Name column
install.packages("dplyr")
library(dplyr)
train_ds <- select(train_ds, -Name)
test_ds <- select(test_ds, -Name)
train_test_data <- select(train_test_data, -Name)
View(train_test_data)

#3.b Change Sex factors to numeric 'Male' - 0, 'Female' - 1 ----

train_test_data$Sex <- factor(train_test_data$Sex, levels = c("male","female"), labels = c("0","1"))

train_ds$Sex <- factor(train_ds$Sex, levels = c("male","female"), labels = c("0","1"))

test_ds$Sex <- factor(test_ds$Sex, levels = c("male","female"), labels = c("0","1"))



#3.c Fill missing values of Age with the median values of Title ----

fillNAwithMedian <- function(x){
  ds_new <- group_by(x,Title)

  new_age <- summarize(ds_new, newAge = median(Age, na.rm = TRUE))

  index <- which(is.na(x$Age ))
  for (i in index){
    x[i,]$Age = new_age$newAge[new_age$Title==x[i,]$Title]
  }
  
  return(x)
}

train_test_data <- fillNAwithMedian(train_test_data)
train_ds <- fillNAwithMedian(train_ds)
test_ds <- fillNAwithMedian(test_ds)

colSums(is.na(train_test_data))


#3.d Binning age to a categorical value----


train_ds$Age_bin <-cut(train_ds$Age, breaks=c(0,16,26,36,62,80), right=TRUE, labels = c(0,1,2,3,4))
test_ds$Age_bin <-cut(test_ds$Age, breaks=c(0,16,26,36,62,80), right=TRUE, labels = c(0,1,2,3,4))
train_test_data$Age_bin <-cut(train_test_data$Age, breaks=c(0,16,26,36,62,80), right=TRUE, labels = c(0,1,2,3,4))
View(train_test_data)

#3.e Fill missing values of Embarked ----

#Since majority of people i.e. 72% embarked from S port, filling the missing values with 'S'

require(stringi)    

table(train_test_data$Embarked)
for (i in 1:nrow(train_ds)) {
  if (stri_isempty(train_ds[i,]$Embarked))
  {
    train_ds[i,]$Embarked <- 'S'
  }
}
str(train_ds$Embarked)
train_ds$Embarked <- factor(train_ds$Embarked, levels = c('C','Q','S'), labels = c("1","2","0"))
------------------------------
for (i in 1:nrow(train_test_data)) {
  if (stri_isempty(train_test_data[i,]$Embarked))
  {
    train_test_data[i,]$Embarked <- 'S'
  }
}
train_test_data$Embarked <- factor(train_test_data$Embarked, levels = c('C','Q','S'), labels = c("1","2","0"))
#test_ds does not have any empty values
test_ds$Embarked <- factor(test_ds$Embarked, levels = c('C','Q','S'), labels = c("1","2","0"))
View(train_test_data)

#3.f Filling missing values in Fare ----

fillFareWithMedian <- function(x){
  ds_new <- group_by(x,Pclass)
  
  val <- summarize(ds_new, MedClass = median(Fare, na.rm = TRUE))
  
  for (i in 1:nrow(x)) {
    if (stri_cmp_eq(x[i,]$Fare, 0) || is.na(x[i,]$Fare))
    {
      x[i,]$Fare <- val$MedClass[val$Pclass == x[i,]$Pclass]
    }
  }
  
  return(x)
}

train_ds <- fillFareWithMedian(train_ds)

View(train_ds)

train_test_data <- fillFareWithMedian(train_test_data)

View(train_test_data)

test_ds <- fillFareWithMedian(test_ds)

View(test_ds)

#Binning fare into 4 categories
summary(train_ds$Fare)

#Based on Quartile values we bin Fare into 4 values viz 
#0 - ranging from 0 to 8
#1 - from 9 to 14
#2 - from 15 to 31
#3 - > 31

train_ds$Fare_bin <- cut(train_ds$Fare, breaks=c(0,8,14,31,513), right=TRUE, labels = c(0,1,2,3))
View(train_ds)

test_ds$Fare_bin <- cut(test_ds$Fare, breaks=c(0,8,14,31,513), right=TRUE, labels = c(0,1,2,3))
View(test_ds)

train_test_data$Fare_bin <- cut(train_test_data$Fare, breaks=c(0,8,14,31,513), right=TRUE, labels = c(0,1,2,3))
View(train_test_data)


#3.g Finding out family size ----

train_ds$FamilyMembers <- train_ds$SibSp + train_ds$Parch + 1

View(train_ds)

table(train_ds$Survived, train_ds$FamilyMembers)

#Insights -- from the above we see that those who are single have less chances of survival 
#than those with family

#Convert into IsAlone flag where 1 = alone, 0 -  with family
train_ds$IsAlone <- if_else(train_ds$FamilyMembers == 1, 1, 0)
train_ds$IsAlone <- factor(train_ds$IsAlone)

table(train_ds$IsAlone)

test_ds$FamilyMembers <- test_ds$SibSp + test_ds$Parch + 1
test_ds$IsAlone <- if_else(test_ds$FamilyMembers == 1, 1, 0)
test_ds$IsAlone <- factor(test_ds$IsAlone)
View(test_ds)

train_test_data$FamilyMembers <- train_test_data$SibSp + train_test_data$Parch + 1
train_test_data$IsAlone <- if_else(train_test_data$FamilyMembers == 1, 1, 0)
train_test_data$IsAlone <- factor(train_test_data$IsAlone)
View(train_test_data)

#3.h Removing the unnecessary features like Name, Ticket, Sibsp,Parch, Cabin, etc  ----

train_ds <- train_ds[ , !names(train_ds) %in% c("PassengerId","Age","SibSp","Parch","Ticket",
                                                "Fare","Cabin","Cabin_bin","FamilyMembers")] 
head(train_test_data)

test_ds <- test_ds[ , !names(test_ds) %in% c("PassengerId","Age","SibSp","Parch","Ticket",
                                                "Fare","Cabin","FamilyMembers")] 

train_test_data <- train_test_data[ , !names(train_test_data) %in% c("PassengerId","Age","SibSp","Parch","Ticket",
                                             "Fare","Cabin","FamilyMembers")] 


-----------------------------------------------------------------------------------------------
#3.i Converting Survived, Pclass to factors----
train_ds$Survived <- factor(train_ds$Survived)
train_ds$Pclass <- factor(train_ds$Pclass)

test_ds$Pclass <- factor(test_ds$Pclass)

train_test_data$Survived <- factor(train_test_data$Survived)
train_test_data$Pclass <- factor(train_test_data$Pclass)


#4. Modelling ----

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_ds$Survived, p=0.75, list=FALSE)
trainSet <- train_ds[index,]
testSet <- train_ds[-index,]
dim(train_ds)
dim(trainSet)
dim(testSet)

#Splitting the train data into X(independent) and Y(dependent) variables
x_train <- trainSet[ , !names(trainSet) %in% c("Survived")]
y_train <- trainSet$Survived

x_test <- testSet[ , !names(testSet) %in% c("Survived")]
y_test <- testSet$Survived
dim(x_test)

#Splitting data using k-Fold repeated cross validation

set.seed(100)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#Logistic Regression
#modelLogReg <- train(Survived ~ ., method = "glm", data = train_ds, trControl = train.control)
modelLogReg <- train(x_train, y_train, method = "glm", trControl = train.control, tuneLength = 3)
predLogReg <- predict(object=modelLogReg, x_test)
confusionMatrix(y_test,predLogReg)
#Accuracy -- 81.53%

# logitMd <- glm(Survived ~ ., family = "binomial", data = train_ds)
# predLogitMd <- predict.glm(x_test)
# print(logitMd)
# confusionMatrix(predLogitMd,y_test)

#Naive Bayes
modelNB <- train(x_train, y_train, method = "nb", trControl = train.control)
predNB <- predict(object=modelNB, x_test)
confusionMatrix(y_test,predNB)
#Accuracy -- 77.03%

#Random Forest
modelRF <- train(x_train, y_train, method = "rf",trControl = train.control)
predRF <- predict(object=modelRF, x_test)
confusionMatrix(y_test,predRF)
#Accuracy -- 80.18%

#CART
modelCART <- train(x_train, y_train, method = "rpart2",trControl = train.control)
predCART <- predict(object=modelCART, x_test)
confusionMatrix(y_test,predCART)
#Accuracy -- 79.73%

#Decision Trees
modelDT <- train(x_train, y_train, method = "ctree", trControl = train.control)
predDT <- predict(object=modelDT, x_test)
confusionMatrix(y_test,predDT)
#Accuracy -- 81.08%

#K - Nearest Neighbours
modelKNN <- train(x_train, y_train, method = "knn", trControl = train.control)
predKNN <- predict(object=modelKNN, x_test)
confusionMatrix(y_test,predKNN)
#Accuracy -- 80.63%

#K - Nearest Neighbours using Max #.Neighbours
modelKNN2 <- train(x_train, y_train, method = "kknn", trControl = train.control)
predKNN2 <- predict(object=modelKNN2, x_test)
confusionMatrix(y_test,predKNN2)
#Accuracy -- 79.28%

#5. Prediction ----

#Using Logistic Regression coz it has highest accuracy
predLogReg <- predict(modelLogReg, test_ds)
summary(predLogReg)

write.table(predLogReg, file="Submission.csv", col.names = FALSE, row.names = FALSE)


