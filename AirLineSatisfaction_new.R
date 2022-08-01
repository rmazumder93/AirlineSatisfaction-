#setwd("D:/Master Degree/CIS 9660/Group Project")
setwd("G:/My Drive/College/Fall Semester 2021/CIS 9660 Data Mining/Project")
# Import data and obtain basic information
Satisfaction=read.csv("satisfaction.csv",header=T,na.strings="?")
dim(Satisfaction)
names(Satisfaction)

# Original numbers of rows
nRows1 = nrow(Satisfaction)

# remove the rows containing missing values
SatisfNew=na.omit(Satisfaction)
dim(SatisfNew)

# numbers of after 393 rows were removed
nRows2 = nrow(SatisfNew)
nRows1 - nRows2

# remove the "ID" column
SatisfNew$id=NULL
dim(SatisfNew)

# There are 14 ordinal variables that reflect customers' satisfaction from scale 1 to 5.
# Value "0" means that this service is not applicable.
# Filter the rows contains value "0" in these 14 ordinal variables.
library(dplyr)
SatisfNew1 = filter(SatisfNew, Seat.comfort>0, Departure.Arrival.time.convenient>0, 
                    Food.and.drink>0, Gate.location>0, Inflight.wifi.service>0, 
                    Inflight.entertainment>0, Online.support>0, 
                    Ease.of.Online.booking>0, On.board.service>0, Leg.room.service>0, 
                    Baggage.handling>0, Checkin.service>0, Cleanliness>0)
#use this data below for analysis 
analysis_data = SatisfNew1
analysis_data$satisfaction_v2 = factor(analysis_data$satisfaction_v2)
dim(SatisfNew1)

# Statistical summary
qulitative.data=c(1:3,5:6,8:21)
SatisfNew1[,qulitative.data]=lapply(SatisfNew1[,qulitative.data],factor)
summary(SatisfNew1)

quantitative.data=c(4,7,22:23)
# Standard Deviation
sapply (SatisfNew1[,quantitative.data],sd)

# Variance
sapply (SatisfNew1[,quantitative.data],var)

# Histograms
par(mfrow=c(1,2))
hist(SatisfNew1$Age,main="Histogram for Age", xlab="Age", ylab="# of Customer")
hist(SatisfNew1$Flight.Distance,main="Histogram for Flight Distance", xlab="Distance", ylab="# of Customer")

## Logistical Regression 
##Removing columns that are not needed for logitical regression 

satisfact = subset(analysis_data, select= -c(Gender, Customer.Type, Age, Type.of.Travel, Class))
glm.fits = glm(satisfaction_v2~., data=satisfact, family=binomial)

summary(glm.fits)


#Based on the summary, seat comfort, food and drink, gate location,inflight entertainment, online support, 
# ease of online booking, on board service, leg room service, baggage handling, check in service, online boarding
# departure delay these independent variable have a positive and significant effect on customer satisfaction. 

# flight distance, inflight wifi service, departure arrival time convenient and arrival delays has negative and
# significant effect on customer satisfaction 

# Predicting the function to be used to predict the probability 

contrasts(satisfact$satisfaction_v2)

glm.probs = predict(glm.fits,type="response")
glm.probs[1:10]

# Creating a vector of class predictions based on predicted probability 

satisfa = satisfact$satisfaction_v2
glm.pred=rep("neutral or dissatisfied",119255)
glm.pred[glm.probs>.5] = "satisfied"
table(glm.pred, satisfact$satisfaction_v2)

#Overall fraction of correct prediction is 
mean(glm.pred==satisfact$satisfaction_v2)

#precision 

precision = 55807/(55807+10526)
precision

#recall 

recall = 55807/(55807+8762)
recall

# The result can be considered misleading since we trained and tested the model on the same set of 119255.
# Training error rate is often underestimates the test error rate. Thus in order to come up with better accuracy
# of logistical regression model using cross validation(?)

#Creating training and testing data sets divided data into 2/3 of training and remaining testing

set.seed(1)
p = .667
n = dim(satisfact)[1]


train = sample(1:nrow(satisfact), nrow(satisfact)*p, replace = F)
test = satisfact[-train,]

Satifa = satisfa[-train]

# Running logistic regression using the subset of the observation that were significant only.
glm.fit = glm(satisfaction_v2~. -Cleanliness,data=satisfact, family=binomial, subset=train)

summary(glm.fit)
glm.probs = predict(glm.fit,test, type="response")
glm.pred=rep("neutral or dissatisfied",39712)
glm.pred[glm.probs>.5] = "satisfied"
table(glm.pred, Satifa)

#Overall fraction of correct prediction is 
mean(glm.pred==Satifa)
#Running logistic regression using the subset of the observation using cross validation shows 
#same number as previously with test error rate as: 
mean(glm.pred!=Satifa)

#precision 

precision = 18613/(18613+3492)
precision

#recall
recall = 18613/(18613+2932)
recall

#Running logistic regression with a smaller model 
glm.fit = glm(satisfaction_v2~Seat.comfort+Leg.room.service+Arrival.Delay.in.Minutes+Food.and.drink+On.board.service,
              data=satisfact, family=binomial, subset=train)

summary(glm.fit)
glm.probs = predict(glm.fit,test, type="response")
glm.pred=rep("neutral or dissatisfied",39712)
glm.pred[glm.probs>.5] = "satisfied"
table(glm.pred, Satifa)
mean(glm.pred==Satifa)
precision = 16663/(16663+5995)
precision
recall = 16663/(16663+4882)
recall
#Prediction drops drastically meaning that all the other variables play significant role in prediction variables

