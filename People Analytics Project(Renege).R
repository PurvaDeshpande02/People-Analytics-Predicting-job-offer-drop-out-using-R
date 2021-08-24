

install.packages("fastDummies")
install.packages("Information")
install.packages("car")
install.packages("pscl")
install.packages("generalhoslem")
install.packages("ROCR") 

library(lubridate)
library(car)
library(Information)
library(fastDummies)
library(pscl)
library(generalhoslem)
library(ROCR)
# _______________________________________________________________________________________________________________________________________
# Step 01 - Importing Data
# _______________________________________________________________________________________________________________________________________

# Importing CSV data set
Renege<-read.csv("E:\\HR Analytics Project\\Renege.csv")

# _______________________________________________________________________________________________________________________________________
# Step 02 - Validate for correctness
# _______________________________________________________________________________________________________________________________________

# Count of Rows and Columns
dim(Renege)

# head and tail data 
head(Renege,10)
tail(Renege,8)

# Understand type of variables
str(Renege)

# count of yes/no of the dependent variable
table(Renege$offer_dropped)

# Offer dropped percentage
1024*100/1999

# Insight: 51% of candidates have dropped the offers


#____________________________________________________________________________________________________________________________________________________________
# Step 03 - Data Preparation
#___________________________________________________________________________________________________________________________________________________________
# Univariate Analysis
# ____________________________________________________________________________________________________________________________________________________________________________________

# Obtain descriptive statistics of the data 
summary(Renege)

# We will start with categorical variables

# visual approach for marital status
Renege$marital_status<-as.factor(Renege$marital_status)
plot(Renege$marital_status, xlab = "marital_status", ylab = "Count", main = "Visual plot of Marital status" )

#metric approach for marital_status
tab_marital <- table(Renege$marital_status)
tab_marital
prop.table(tab_marital)*100
# Insight: Married candidates have 69% of the total

#3)Visual & Metric approach of education_level
Renege$education_level <- as.factor(Renege$education_level)
plot(Renege$education_level, xlab = "Education" , ylab = "Count", main = "Visual plot of education_level")
tab_education <- table(Renege$education_level)
prop.table(tab_education)*100
#Insight (Visual) - Associate degree is highest frequency followed by associate certification
#Insight (Metric) - Associate degree & certifcation is 60%

#change of chr to factor in group
str(Renege)
var<- c("gender","marital_status", "education_level", "distance_from_home" , "sourcing_channel" , "career_growth", "flexi_work", "timely_communication" , "offer_dropped")
Renege [,var]<-lapply(Renege [,var],factor)

#change of chr to date
myfunction <- function(x) mdy(x)
var1<- c("date_1st_contact", "date_offered")
Renege [,var1]<-lapply(Renege [,var1], myfunction )

#factor colms analysis (var list)
for (i in var){
  tab_temp <- table(Renege[i])
  print(prop.table(tab_temp)*100)
  plot(Renege[i], xlab = i, ylab = "Count", main = "Visual plot" )
  
}

# We will start with Numerical  variables

# age analysis
hist(Renege$age, xlab = "Age" , ylab = "Frequency", main = "Visual plot of Age", breaks = 30)
tab_age <- table(Renege$age)
tab_age
prop.table(tab_age)*100
#Insight - Almost all the count is same from 25-45, although 46 age people are least

#int colms analysis
var2 <- c("age", "percent_hike", "total_rounds", "satisfaction_index","no_companies_worked","total_experience")
for (j in var2) {
  tab_temp1 <-table(Renege[j])
  print(prop.table(tab_temp1)*100)
 # hist(Renege[j], xlab = j, ylab = "count", main = "Visual plot")
}

# _______________________________________________________________________________________________________________________________________
# Step 04 - Missing Value
# _______________________________________________________________________________________________________________________________________   


#check for missing values
sum(is.na(Renege))

#remove na values
data_missing <- na.omit(Renege)
dim(data_missing)

# _______________________________________________________________________________________________________________________________________
# Step 05 - Feature Engineering - New Variable Creation
# _______________________________________________________________________________________________________________________________________

Renege$jhi <- Renege$total_experience/Renege$no_companies_worked
str(Renege)
# Metric approach
summary(Renege$jhi)
# Insight: Higher job hoping index at 7.3 and lower at 0.5

# Visual approach
hist(Renege$jhi)
# Insight: Maximum job hoping index ranges from 0.5 to 3

# create days to offer: days_offered
Renege$days_offered <- as.Date(Renege$date_offered,"%m/%d/%Y") - as.Date(Renege$date_1st_contact,"%m/%d/%Y")

# Convert days offered to numeric
Renege$days_offered<-as.numeric(Renege$days_offered)
# Insight: days_offered is converted in numeric from difftime format
class(Renege$days_offered)

# Visual approach for days_offered
hist(Renege$days_offered)
#par(mar=c(1,1,1,1))
# Insight: Candidates were offered job as less as 1 day to maximum 180 days

# Metric approach 
tab_days_offered_num <- table(Renege$days_offered)
tab_days_offered_num
prop.table(tab_days_offered_num)
# Insight: Approx 35% of candidates were offered job after 100 days

# Create Renege_new and remove "date_1st_contact", "date_offered", "no_companies_worked", "total_experience"  
dim(Renege)
Renege<- subset(Renege,select = -c(date_1st_contact,date_offered,no_companies_worked,total_experience))

# ___________________________________________________________________________________________________________________________________

# Step 06 - Bi-Variate Analysis - Independent vs. Dependent and Hypothesis testing
# ___________________________________________________________________________________________________________________________________

#Visual and metric approach for marital_status (independent) & offer_dropped (dependant)
plot(Renege$marital_status, Renege$offer_dropped, xlab = "marital_status" , ylab = "offer dropped")
tab_maritalbi<- table(Renege$offer_dropped, Renege$marital_status)
tab_maritalbi
round(prop.table(tab_maritalbi,2)*100,0)
#Insight - Almost Equal proportion of accepting/rejecting offer across the different levels of marital_status

#Visual and metric approach for education_level (independent) & offer_dropped (dependant)
plot(Renege$education_level, Renege$offer_dropped, xlab = "Ed_level" , ylab = "offer dropped")
tab_edbi<- table(Renege$offer_dropped, Renege$education_level)
tab_edbi
round(prop.table(tab_edbi,2)*100,0)
#Insight - Almost Equal proportion of accepting/rejecting offer across the different levels of ed_level

#Visual and metric approach for gender (independent) & offer_dropped (dependant)
plot(Renege$gender, Renege$offer_dropped, xlab = "Gender" , ylab = "offer dropped")
tab_genderbi<- table(Renege$offer_dropped, Renege$gender)
tab_genderbi
round(prop.table(tab_genderbi,2)*100,0)
#Insight - There 3 times more males than females in dataset while the proportion is almost equal

#Visual and metric approach for distance from home (independent) & offer_dropped (dependant)
plot(Renege$distance_from_home, Renege$offer_dropped, xlab = "Distance from home" , ylab = "offer dropped")
tab_dfhbi<- table(Renege$offer_dropped, Renege$distance_from_home)
tab_dfhbi
round(prop.table(tab_dfhbi,2)*100,0)
#Insight - 59% of people are not accepting the offers because dist from home is >20kms

#Visual and metric approach for sourcing channel (independent) & offer_dropped (dependant)
plot(Renege$sourcing_channel, Renege$offer_dropped, xlab = "Sourcing Channel" , ylab = "offer dropped")
tab_scbi<- table(Renege$offer_dropped, Renege$sourcing_channel)
tab_scbi
round(prop.table(tab_scbi,2)*100,0)
#Insight - out of all the internal referrals 70% dropped the offer, hence it may not be a good source

#Visual and metric approach for career growth (independent) & offer_dropped (dependant)
plot(Renege$career_growth, Renege$offer_dropped, xlab = "career growth" , ylab = "offer dropped")
tab_cgbi<- table(Renege$offer_dropped, Renege$career_growth)
tab_cgbi
round(prop.table(tab_cgbi,2)*100,0)
#Insight - More than 60% candidates who dropped offer were in later growth category

#Visual and metric approach for flexi_work (independent) & offer_dropped (dependant)
plot(Renege$flexi_work, Renege$offer_dropped, xlab = "Flexi work" , ylab = "offer dropped")
tab_fwbi<- table(Renege$offer_dropped, Renege$flexi_work)
tab_fwbi
round(prop.table(tab_fwbi,2)*100,0)
#Insight - there is 6:4 ratio of offer drop between candidates who are flexible work timing or do not have flexible work timing

#Visual and metric approach for Timely communication(independent) & offer_dropped (defendant)
plot(Renege$timely_communication, Renege$offer_dropped, xlab = "Timely Communication" , ylab = "offer dropped")
tab_tcbi<- table(Renege$offer_dropped, Renege$timely_communication)
tab_tcbi
round(prop.table(tab_tcbi,2)*100,0)
#Insight - 60% candidates drop offer when they are not timely communicated

#bi- variate analysis - independent vs defendant, Numeric variables

#Visual and metric approach for Age(independent) & offer_dropped (defendant)
boxplot(Renege$age ~ Renege$offer_dropped, xlab = "Offer dropped", ylab = "Age" , range = 95)
#metric approach 
#cal mean
mean_age <- by(Renege$age, Renege$offer_dropped, mean)
mean_age
#cal median
med_age <- by(Renege$age, Renege$offer_dropped, median)
med_age
#Insight - Age does not seem to be a reason of offer drop

#Visual and metric approach for percent_hike(independent) & offer_dropped (defendant)
boxplot(Renege$percent_hike ~ Renege$offer_dropped, xlab = "Offer dropped", ylab = "% hike" , range = 95)
#metric approach 
#cal mean
mean_percenthike <- by(Renege$percent_hike, Renege$offer_dropped, mean)
mean_percenthike
#cal median
med_percenthike <- by(Renege$percent_hike, Renege$offer_dropped, median)
med_percenthike
#cal std deviation
sd_percenthike <- by(Renege$percent_hike, Renege$offer_dropped, sd)
sd_percenthike
#Insight - The median percent hike for candidates who dropped the offer is less than who accepted

#Visual and metric approach for satisfactory index (independent) & offer_dropped (defendant)
boxplot(Renege$satisfaction_index ~ Renege$offer_dropped, xlab = "Offer dropped", ylab = "Satisfaction Index" , range = 95)
#metric approach 
#cal mean
mean_satisfaction_index <- by(Renege$satisfaction_index, Renege$offer_dropped, mean)
mean_satisfaction_index
#cal median
med_satisfaction_index <- by(Renege$satisfaction_index, Renege$offer_dropped, median)
med_satisfaction_index
#cal std deviation
sd_satisfaction_index <- by(Renege$satisfaction_index, Renege$offer_dropped, sd)
sd_satisfaction_index
#Insight - the median satisfaction index for candidates who accept is much more than who drop

#Visual and metric approach for Total Rounds (independent) & offer_dropped (defendant)
boxplot(Renege$total_rounds ~ Renege$offer_dropped, xlab = "Offer dropped", ylab = "Total Rounds" , range = 95)
#metric approach 
#cal mean
mean_totalrounds <- by(Renege$total_rounds, Renege$offer_dropped, mean)
mean_totalrounds
#cal median
med_totalrounds <- by(Renege$total_rounds, Renege$offer_dropped, median)
med_totalrounds
#cal std deviation
sd_totalrounds <- by(Renege$total_rounds, Renege$offer_dropped, sd)
sd_totalrounds
#Insight more rounds of interview more is the drop off percent

#Visual and metric approach for jhi (independent) & offer_dropped (defendant)
boxplot(Renege$jhi ~ Renege$offer_dropped, xlab = "Offer dropped", ylab = "JHI" , range = 95)
#metric approach 
#cal mean
mean_jhi <- by(Renege$jhi, Renege$offer_dropped, mean)
mean_jhi
#cal median
med_jhi <- by(Renege$jhi, Renege$offer_dropped, median)
med_jhi
#cal std deviation
sd_jhi <- by(Renege$jhi, Renege$offer_dropped, sd)
sd_jhi
## Insight: jhi indicates stability of a candidate; higher the jhi, more stable the candidate 

#Visual and metric approach for days_offered (independent) & offer_dropped (defendant)
boxplot(Renege$days_offered ~ Renege$offer_dropped, xlab = "Offer dropped", ylab = "days_offered" , range = 95)
#metric approach 
#cal mean
mean_days_offered <- by(Renege$days_offered, Renege$offer_dropped, mean)
mean_days_offered
#cal median
med_days_offered <- by(Renege$days_offered, Renege$offer_dropped, median)
med_days_offered
#cal std deviation
sd_days_offered <- by(Renege$days_offered, Renege$offer_dropped, sd)
sd_days_offered
# Insight: This calls for interview process optimization as delayed process may lead candidates to apply for jobs elsewhere due to lack of clarity

#_______________________________________________________________________________________________________________________________________
# Step 07 - Hypothesis Testing - Categorical Variables
#_______________________________________________________________________________________________________________________________________

# The chi-square test of independence is used to determine if there is a significant relationship between two 
# nominal (categorical) variables.
# The null hypothesis for this test is that there is no relationship between the variables and the alternate hypothesis that 
# there is a relationship

#H0= The 2 variables are independent of each other
#H1= They are dependent.


var<- c("gender","marital_status", "education_level", "distance_from_home" , "sourcing_channel" , "career_growth", "flexi_work", "timely_communication")

for (i in var){
  tab_temp <- table(Renege[[i]],Renege[["offer_dropped"]])
  tab_temp
  print(i)
  print(chisq.test(tab_temp))
}
#Insight- "distance_from_home" , "sourcing_channel" , "career_growth", "flexi_work", "timely_communication" - these are significant and will be considered for the further analysis

#creating new data frame
Renege_new <- subset(Renege,select = -c(gender,marital_status, education_level))

#_______________________________________________________________________________________________________________________________________
# Step 08 - Dummy variables creation
#_______________________________________________________________________________________________________________________________________

#1)dist from home
Renege_new$dfh_15 <- ifelse (Renege_new$distance_from_home == "<15 kms",1,0)
Renege_new$dfh_15to20 <- ifelse (Renege_new$distance_from_home == "15-20 kms",1,0)

#2)others
var<- c("sourcing_channel" , "career_growth", "flexi_work", "timely_communication")

Renege_new<- dummy_cols(Renege_new,select_columns = var)

#removing above var list
Renege_new <- subset(Renege_new, select = - c(distance_from_home,sourcing_channel , career_growth, flexi_work, timely_communication))

str(Renege_new)

#_______________________________________________________________________________________________________________________________________
# Step 09 -  Dimension Reduction
#_______________________________________________________________________________________________________________________________________
# Step 09 - A - Information value
#_______________________________________________________________________________________________________________________________________

#Dimension reduction
class(Renege_new$offer_dropped)
Renege_new$offer_dropped_num <- ifelse (Renege_new$offer_dropped == "Yes",1,0)
summary(Renege_new$offer_dropped_num)

Renege_new$offer_dropped<-NULL
Renege_new$flexi_work_No<-NULL
Renege_new$timely_communication_No<-NULL
Renege_new$`sourcing_channel_Job Portals`<-NULL
Renege_new$career_growth_Vertical<-NULL

# Calculate Information Value for all numeric variables
# install.packages("Information")

infotable<- create_infotables(Renege_new,y="offer_dropped_num")
infotable
Renege_new<-Renege_new[c(-10,-7,-12)]

#_______________________________________________________________________________________________________________________________________
# Step 09 - B - Detecting and Dealing with Multicollinearity 
#_______________________________________________________________________________________________________________________________________

# The basic problem is multicollinearity results in unstable parameter estimates which makes it very 
# difficult to assess the effect of independent variables on dependent variables

# variance inflation factors (VIF) helps us to detect multicollinearity
# It quantifies how much the variance is inflated
# VIF of 4 or more suggests the presence of multicollinearity
# install.packages("car")


vif(glm(offer_dropped_num~.,family = binomial,data = Renege_new))
# Insights: There are no variables which are multicollinear, no action required

#_______________________________________________________________________________________________________________________________________
# Step 10 - Data Partitioning - Training & Testing Data sets
#_______________________________________________________________________________________________________________________________________

# Shuffeling the data before partitioning
Renege_new_shuffle <-Renege_new[sample(nrow(Renege_new)),]

# Splitting data into train data and test data
set.seed(567)
index<- sample(2,nrow(Renege_new_shuffle),replace = TRUE, prob = c(0.7,0.3))
View (index)
# Training data set
data_train <-Renege_new_shuffle[index == 1,]
# Testing data set
data_test <-Renege_new_shuffle[index == 2,]

dim(data_train)
dim(data_test)

#_______________________________________________________________________________________________________________________________________
# Step 11 - Model Building 
#_______________________________________________________________________________________________________________________________________


# Iteration 01 - Model with all variables
model_0 <-glm(offer_dropped_num~., family = binomial , data = data_train)
options(scipen = 9999)
summary(model_0)


model_1 <-glm(offer_dropped_num~. -jhi, family = binomial , data = data_train)
summary(model_1)

model_2 <-glm(offer_dropped_num~. -jhi -total_rounds, family = binomial , data = data_train)
summary(model_2)

model_3 <-glm(offer_dropped_num~. -jhi -total_rounds -`sourcing_channel_Company Website`, family = binomial , data = data_train)
summary(model_3)

#_______________________________________________________________________________________________________________________________________
# Step 12 - Predicting Training Data 
#_______________________________________________________________________________________________________________________________________
# 

# Predicting on training data
data_train$pred<- predict(model_3, newdata=data_train, type = "response")
data_train$pred
class(data_train)


#_______________________________________________________________________________________________________________________________________
# Step 13 - Evaluating the Model - Part 1 - Training Data 
#_______________________________________________________________________________________________________________________________________

# Pseudo R^2
pR2(model_3)

# Hosmer-Lemeshow Test
logitgof(obs = data_train$offer_dropped_num, exp = fitted(model_3))

# ROC Curve
pred2 <- prediction(data_train$pred,labels = data_train$offer_dropped_num)
class(pred2)

slotNames(pred2)

roc.perf = performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

abline(a = 0, b = 1)

auc <- performance(pred2, measure = "auc")
auc
auc <- auc@y.values[[1]]
auc

# Extract the maximum accuracy(classification rate) and the corresponding cut-off
acc.perf = performance(pred2, measure = "acc")

slotNames(acc.perf)

ind = which.max(slot(acc.perf, "y.values")[[1]] )

ind

acc = slot(acc.perf, "y.values")[[1]][ind]

cutoff = slot(acc.perf, "x.values")[[1]][ind]



print(c(accuracy = acc, cutoff = cutoff))

# Insight: Accuracy of the model is 95%


# Testing the accuracy with the obtained cut-off on training data
predcat47<- ifelse(data_train$pred >0.5114, 1, 0)


accuracy<- table(predcat47, data_train[,"offer_dropped_num"])
diag(accuracy)
sum(diag(accuracy)) / sum(accuracy)
table(data_train$offer_dropped_num)

#_______________________________________________________________________________________________________________________________________
# Step 14 - Evaluating the Model - Part 2 - Testing Data 
#_______________________________________________________________________________________________________________________________________


# Now Lets do some prediction using the model on the test data set. We have created a new new variable by the name pred 
# which has the predicted probability values 

# Predicting on testing data
data_test$pred = predict(model_3, newdata=data_test, type = "response")

# Testing the accuracy with the obtained cut-off on testing data
predtest47<- ifelse(data_test$pred >0.5114, 1, 0)
accuracy<- table(predtest47, data_test[,"offer_dropped_num"])
sum(diag(accuracy)) / sum(accuracy)
# Insight: an accuracy of 94% represents a good model that is ready to be used to predict the offer drop out on a 
# brand new data set


names(Renege)


