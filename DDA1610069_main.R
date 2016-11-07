#set path for the dataset 
setwd("H:\\PGDDA\\Course 3 Predictive Analytics I\\Module_5_Supervised_classification2_Logistic_regression\\Session4_Graded_Assignment")

# Install the required packages for the case study
install.packages("car") 
install.packages("Hmisc")
install.packages("ROCR")
install.packages("Caret")
install.packages("caTools")
install.packages("MASS")

#attach the objects to R environment
library(car)
library(Hmisc)
library(ROCR)
library(caret)
library(caTools)
library(MASS)
# Download the data set as german_credit
#Load the dataset into german_credit data object
german_credit <- read.csv("german.csv", stringsAsFactors = FALSE)

# Exploratory Data Analysis
##### CHECKPOINT 1 : Data Understanding and Data Exploration

#View the dataframe if the data is loaded or not
View(german_credit)

#check the summary of the dataframe
summary(german_credit)

#Check the structure of the dataframe
str(german_credit)

#21 variables in the dataframe
#13 character variables and 8 interger variables
library(ggplot2)
# Categorical variable
# Status.of.existing.checking.account
summary(as.factor(german_credit$Status.of.existing.checking.account))
ggplot(german_credit, aes(x=Status.of.existing.checking.account)) + geom_bar(col="red", fill="blue", alpha = 0.4) + 
  ggtitle("Status of existing accounts")  + coord_flip()

#  other credits existing (not at this bank)
summary(as.factor(german_credit$Credit.history))
ggplot(german_credit, aes(x=Credit.history)) + geom_bar(col="red", fill="green", alpha = 0.4) + 
  ggtitle("Credit History") + coord_flip()

# Purpose
summary(as.factor(german_credit$Purpose))
ggplot(german_credit, aes(x=Purpose)) + geom_bar(col="red", fill="blue", alpha = 0.4) + 
  ggtitle("Purpose of Loan")

# Savings.account.bonds
summary(as.factor(german_credit$Savings.account.bonds))
ggplot(german_credit, aes(x=Savings.account.bonds)) + geom_bar(col="black", fill="pink", alpha = 0.4) + 
  ggtitle("Savings accounts")

# Present.employment.since.
summary(as.factor(german_credit$Present.employment.since.))
ggplot(german_credit, aes(x=Present.employment.since.)) + geom_bar(col="blue", fill="violet", alpha = 0.7) + ggtitle("Present Employment Since")

# Personal.status.and.sex
summary(as.factor(german_credit$Personal.status.and.sex))
ggplot(german_credit, aes(x=Personal.status.and.sex)) + geom_bar(col="red", fill="brown", alpha = 0.4) + 
  ggtitle("personal Status") + coord_flip()

# Other.debtors...guarantors
summary(as.factor(german_credit$Other.debtors...guarantors))
ggplot(german_credit, aes(x=Other.debtors...guarantors)) + geom_bar() + ggtitle("Other.debtors...guarantors")

# Property
summary(as.factor(german_credit$Property))
ggplot(german_credit, aes(x=Property)) + geom_bar() + ggtitle("Property")

# Other.installment.plans
summary(as.factor(german_credit$Other.installment.plans))
ggplot(german_credit, aes(x=Other.installment.plans)) + geom_bar(col="red", fill="chocolate", alpha = 0.4) + 
  ggtitle("other Installment Plans") + coord_flip()

# Housing.
summary(as.factor(german_credit$Housing.))
ggplot(german_credit, aes(x=Housing.)) + geom_bar() + ggtitle("housing")

# Job_status
summary(as.factor(german_credit$Job_status))
ggplot(german_credit, aes(x=Job_status)) + geom_bar() + ggtitle("Job Status")

# Telephone.
summary(as.factor(german_credit$Telephone.))
ggplot(german_credit, aes(x=Telephone.)) + geom_bar() + ggtitle("Telephone")

# foreign.worker
summary(as.factor(german_credit$foreign.worker))
ggplot(german_credit, aes(x=foreign.worker)) + geom_bar() + ggtitle("Foreign Worker")

### Integer Variables

#Duration.in.month
quantile(german_credit$Duration.in.month, seq(0,1, 0.01))
boxplot(german_credit$Duration.in.month)

#Credit.amount
quantile(german_credit$Credit.amount, seq(0,1, 0.01))
boxplot(german_credit$Credit.amount)

#Installment.rate.in.percentage.of.disposable.income 
quantile(german_credit$Installment.rate.in.percentage.of.disposable.income, seq(0,1, 0.01))
boxplot(german_credit$Installment.rate.in.percentage.of.disposable.income)
summary(as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income))

#Present.residence.since 
quantile(german_credit$Present.residence.since, seq(0,1, 0.01))
boxplot(german_credit$Present.residence.since)
summary(as.factor(german_credit$Present.residence.since))

#Age.in.Years 
quantile(german_credit$Age.in.Years, seq(0,1, 0.01))
boxplot(german_credit$Age.in.Years)

#Number.of.existing.credits.at.this.bank.
quantile(german_credit$Number.of.existing.credits.at.this.bank., seq(0,1, 0.01))
boxplot(german_credit$Number.of.existing.credits.at.this.bank.)
summary(as.factor(german_credit$Number.of.existing.credits.at.this.bank.))

# Number.of.people.being.liable.to.provide.maintenance.for. 
summary(as.factor(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.))

# Data preparation and feature transformation
##### CHECKPOINT 2 : Data Cleaning and Transformation

# missing value
sapply(german_credit, function(x) sum(is.na(x)))
# fortunately, there are no missing or NA values in the dataset

# outlier treatment

#Age.in.Years 
age_out <- boxplot.stats(german_credit$Age.in.Years)
age_out
german_credit11 <- subset(german_credit, !Age.in.Years %in% age_out)
german_credit$Age.in.Years[which(german_credit$Age.in.Years > max(german_credit11$Age.in.Years))] <- max(german_credit11$Age.in.Years)

# Scale the values
#Credit.amount
german_credit[,5] <- scale(german_credit[,5])

#Duration.in.month
german_credit[,2] <- scale(german_credit[,2])

#Age.in.Years
german_credit[,13] <- scale(german_credit[,13])

# Converting all categorical variables to factors

# Status.of.existing.checking.account
german_credit$Status.of.existing.checking.account <- as.factor(german_credit$Status.of.existing.checking.account)
# Credit.history
german_credit$Credit.history <- as.factor(german_credit$Credit.history)
# Purpose
german_credit$Purpose <- as.factor(german_credit$Purpose)
# Savings.account.bonds
german_credit$Savings.account.bonds <- as.factor(german_credit$Savings.account.bonds)
# Present.employment.since.
german_credit$Present.employment.since. <- as.factor(german_credit$Present.employment.since.)
# Personal.status.and.sex
german_credit$Personal.status.and.sex <- as.factor(german_credit$Personal.status.and.sex)
# Other.debtors...guarantors
german_credit$Other.debtors...guarantors <- as.factor(german_credit$Other.debtors...guarantors)
# Property
german_credit$Property <- as.factor(german_credit$Property)
# Other.installment.plans
german_credit$Other.installment.plans <- as.factor(german_credit$Other.installment.plans)
# Housing.
german_credit$Housing. <- as.factor(german_credit$Housing.)
# Job_status
german_credit$Job_status <- as.factor(german_credit$Job_status)
# Telephone.
german_credit$Telephone. <- as.factor(german_credit$Telephone.)
# foreign.worker
german_credit$foreign.worker <- as.factor(german_credit$foreign.worker)

### Check the structure of german_credit data set
str(german_credit)

## Create dummy variables for factor type variables.

# Status.of.existing.checking.account

stschkacct.dummy <- data.frame(model.matrix(~Status.of.existing.checking.account, data = german_credit))
stschkacct.dummy <- stschkacct.dummy[,-1]

# Credit.history
Credit.history.dummy <- data.frame(model.matrix(~Credit.history, data = german_credit))
Credit.history.dummy <- Credit.history.dummy[,-1]

# Purpose
Purpose.dummy <- data.frame(model.matrix(~Purpose, data = german_credit))
Purpose.dummy <- Purpose.dummy[,-1]

# Savings.account.bonds
Savings.account.bonds.dummy <- data.frame(model.matrix(~Savings.account.bonds, data = german_credit))
Savings.account.bonds.dummy <- Savings.account.bonds.dummy[,-1]

# Present.employment.since.
Present.employment.since.dummy <- data.frame(model.matrix(~Present.employment.since., data = german_credit))
Present.employment.since.dummy <- Present.employment.since.dummy[,-1]

# Personal.status.and.sex
Personal.status.and.sex.dummy <- data.frame(model.matrix(~Personal.status.and.sex, data = german_credit))
Personal.status.and.sex.dummy <- Personal.status.and.sex.dummy[,-1]

# Other.debtors...guarantors
Other.debtors...guarantors.dummy <- data.frame(model.matrix(~Other.debtors...guarantors, data = german_credit))
Other.debtors...guarantors.dummy <- Other.debtors...guarantors.dummy[,-1]

# Property
Property.dummy <- data.frame(model.matrix(~Property, data = german_credit))
Property.dummy <- Property.dummy[,-1]

# Other.installment.plans
Other.installment.plans.dummy <- data.frame(model.matrix(~Other.installment.plans, data = german_credit))
Other.installment.plans.dummy <- Other.installment.plans.dummy[,-1]
str(Other.installment.plans.dummy)

# Housing.
Housing.dummy <- data.frame(model.matrix(~Housing., data = german_credit))
Housing.dummy <- Housing.dummy[,-1]

# Job_status
Job_status.dummy <- data.frame(model.matrix(~Job_status, data = german_credit))
Job_status.dummy <- Job_status.dummy[,-1]

# Telephone.
Telephone.dummy <- as.data.frame(model.matrix(~Telephone., data = german_credit))
Telephone.dummy <- Telephone.dummy[,-1]
Telephone.dummy <- data.frame(Telephone.dummy)

# foreign.worker
foreign.worker.dummy <- data.frame(model.matrix(~foreign.worker, data = german_credit))
foreign.worker.dummy <- foreign.worker.dummy[,-1]
foreign.worker.dummy <- data.frame(foreign.worker.dummy)

# Final dataset with dummy variables
german_credit_new <- cbind(german_credit[, c(2,5,8,11,13,16,18,21)], stschkacct.dummy, Credit.history.dummy, 
                           Purpose.dummy, Savings.account.bonds.dummy, 
                           Present.employment.since.dummy, Personal.status.and.sex.dummy, 
                           Other.debtors...guarantors.dummy, Property.dummy, Other.installment.plans.dummy, 
                           Housing.dummy, Job_status.dummy, Telephone.dummy, foreign.worker.dummy)

# Check the structure of the new prepared dataframe for german credit
str(german_credit_new)

#### CHECKPOINT 3 : Splitting the dataset into test and train

library(caTools)
# Set seed value #seed = 100
set.seed(100)
# divide the dataset into Train and Test
split_indices <- sample.split(german_credit_new$Default_status , SplitRatio = 0.70)
train <- german_credit_new[split_indices==T,]
test <- german_credit_new[split_indices==F,]

# Initial Model with all variables
#### CHECKPOINT 4 : Modelling

# prepare the initial model
model_1 <- glm(Default_status~., family = binomial, data = train)
summary(model_1)

# use step to find the ideal model
step <- step(model_1, direction = "both")
# View step object to get the equation for optimised equation
step

# Stepwise selection

library(car)
# use the equation for model 2
model_2 <- glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA410 + PurposeA42 + PurposeA43 + PurposeA49 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

# check the summary for high p-values i.e. low significant variables
summary(model_2)
# check the vif 
sort(vif(model_2))

# PurposeA49

model_3 <- glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA410 + PurposeA42 + PurposeA43 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_3)
sort(vif(model_3))

# PurposeA410
model_4 <- glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_4)
sort(vif(model_4))

# Credit.amount
model_5 <- glm(formula = Default_status ~ Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 + Other.debtors...guarantorsA103 + 
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_5)
sort(vif(model_5))

# Other.debtors...guarantorsA103

model_6 <- glm(formula = Default_status ~ Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA42 + PurposeA43 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 +
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_6)
sort(vif(model_6))

# PurposeA42

model_7 <- glm(formula = Default_status ~ Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + PurposeA43 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 +
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_7)
sort(vif(model_7))

# PurposeA43

model_8 <- glm(formula = Default_status ~ Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income + Age.in.Years + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 +
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_8)

# Age.in.Years

model_9 <- glm(formula = Default_status ~ Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income + 
                 Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                 Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                 PurposeA41 + 
                 Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                 Personal.status.and.sexA93 +
                 Other.installment.plansA143 + Housing.A152, family = binomial, 
               data = train)

summary(model_9)

# Personal.status.and.sexA93
model_10 <- glm(formula = Default_status ~ Duration.in.month +
                  Installment.rate.in.percentage.of.disposable.income + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  PurposeA41 + 
                  Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74 + 
                  Other.installment.plansA143 + Housing.A152, family = binomial, 
                data = train)

summary(model_10)

# Housing.A152

model_11 <- glm(formula = Default_status ~ Duration.in.month +
                  Installment.rate.in.percentage.of.disposable.income + 
                  Status.of.existing.checking.accountA13 + Status.of.existing.checking.accountA14 + 
                  Credit.historyA32 + Credit.historyA33 + Credit.historyA34 + 
                  PurposeA41 + Other.installment.plansA143 +
                  Savings.account.bondsA64 + Savings.account.bondsA65 + Present.employment.since.A74,
                family = binomial, 
                data = train)

summary(model_11)
sort(vif(model_11))
# ----------------
# take model 11
model_final <- model_11
#-----------------------
# c-statistic and KS -statistic
#### CHECKPOINT 5 : Model Evaluation
library(Hmisc)
#C- statistics--Training dataset ---------------------------------------------------
train$predicted_prob = predict(model_final,  type = "response")
rcorr.cens(train$predicted_prob,train$Default_status) # 1st argument is your vector of predicted probabilities, 2nd observed values of outcome variable
# C Index : 8.095675e-01
# C- statistics-----test dataset--------------------------
test$predicted_prob <- predict(model_final, newdata = test ,type = "response")
rcorr.cens(test$predicted_prob,test$Default_status)
# C Index : 7.475661e-01

# K S Statistic --Training dataset ---------------------------------------------------
library(ROCR)

model_score <- prediction(train$predicted_prob ,train$Default_status)
model_perf <-  performance(model_score, "tpr", "fpr")
#plot(model_perf)
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)
ks # 0.4836735
which(ks_table == ks) # 202

ks_decile_train <- 202/700
ks_decile_train # 0.2885714

#--------KS Statistic -----test dataset--------------------------

test_model_score <- prediction(test$predicted_prob ,test$Default_status)
test_model_perf <-  performance(test_model_score, "tpr", "fpr")
test_ks_table <- attr(test_model_perf, "y.values")[[1]] - (attr(test_model_perf, "x.values")[[1]])
test_ks = max(test_ks_table)
test_ks # 0.415873
which(test_ks_table == test_ks) #120

ks_decile_test <- 120/300 
ks_decile_test # 0.4
plot(model_final)

# Selecting threshold value
#### CHECKPOINT 6 : Threshold value

# Predict probabilities of test data
predictions_data <- predict(model_final, newdata = test[, -8 ], type="response")
#Create a 'predictions' object using the predicted probabilities and the test labels
pred_object <- prediction(predictions_data, test$Default_status)
# creating the ROC curve - creating a 'performance' object
performance_measures <- performance(pred_object, measure = "tpr", x.measure = "fpr")
plot(performance_measures)

# Area under the curve
auc <- performance(pred_object, measure = "auc")
auc <- unlist(auc@y.values)
auc # 0.7475661
# according to theory auc value 0.70 is a good sign

# ROC curve
plot(model_perf,col = "red", lab = c(10,10,10))
# Threshold value taken as 0.5
# since we want to minimise the False negative value, low threshold value has to be selected

library(caret)
library(e1071)

# Train
confusionMatrix(as.numeric(train$predicted_prob > 0.35), train$Default_status, positive = "1")


# Accuracy : 0.75 (75%)
# Sensitivity : 0.70 (70%)
# specificity : 0.7714 (77%)

# Test
confusionMatrix(as.numeric(test$predicted_prob > 0.35), test$Default_status, positive = "1")


# Accuracy : 0.7067 (71%)
# Sensitivity : 0.6222 (62%)
# Specificity : 0.7429 (74%)