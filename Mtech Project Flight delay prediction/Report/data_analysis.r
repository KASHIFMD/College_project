```{r setup, include=FALSE, echo=FALSE}
knitr::opts_chunk$set(echo=TRUE)

```
In this notebook, we will work on 2018 flight delays dataset, where we have about 7.2 million flights. 
We will explore the data, conduct a data preparation, and decide variables that could be used for naive Bayes classifier.

##Data Preparation
```{r DP, echo=FALSE,message = FALSE, warning = FALSE}
library(dplyr)
library(ggplot2)
fd.df <- read.csv("2019.csv")

#Reorder the columns in respective of preferred use (e.g. unnamed..27 string variable will most probably not going to be used)
col.order <- c("FL_DATE","OP_CARRIER","ORIGIN","DEST","CRS_DEP_TIME","CRS_ARR_TIME","CRS_ELAPSED_TIME","ACTUAL_ELAPSED_TIME","DEP_TIME","ARR_TIME","DEP_DELAY","ARR_DELAY","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY","CANCELLED","DIVERTED","TAXI_OUT","TAXI_IN","WHEELS_OFF","WHEELS_ON","CANCELLATION_CODE",
"AIR_TIME","DISTANCE","OP_CARRIER_FL_NUM","Unnamed..27")
fd.df <- fd.df[,col.order]
str(fd.df)
```

The structure of dataset indicates that there are 28 variables, that consists of a variety of categorical and continous variables. 
The detailed description of these variables can be found at 
https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time

##Missing Data Analysis
```{r MissingData,echo=FALSE, message = FALSE, warning = FALSE}
MissingData <- data.frame("FL_DATE"=sum(is.na(fd.df$FL_DATE)),"OP_CARRIER"=sum(is.na(fd.df$OP_CARRIER)),"ORIGIN"=sum(is.na(fd.df$ORIGIN)),"DEST"=sum(is.na(fd.df$DEST)),"CRS_DEP_TIME"=sum(is.na(fd.df$CRS_DEP_TIME)),"CRS_ARR_TIME"=sum(is.na(fd.df$CRS_ARR_TIME)),"CRS_ELAPSED_TIME"=sum(is.na(fd.df$CRS_ELAPSED_TIME)),"ACTUAL_ELAPSED_TIME"=sum(is.na(fd.df$ACTUAL_ELAPSED_TIME)),"DEP_TIME"=sum(is.na(fd.df$DEP_TIME)),"ARR_TIME"=sum(is.na(fd.df$ARR_TIME)),
"DEP_DELAY"=sum(is.na(fd.df$DEP_DELAY)),"ARR_DELAY"=sum(is.na(fd.df$ARR_DELAY)),"CARRIER_DELAY"=sum(is.na(fd.df$CARRIER_DELAY)),"WEATHER_DELAY"=sum(is.na(fd.df$WEATHER_DELAY)),"NAS_DELAY"=sum(is.na(fd.df$NAS_DELAY)),
"SECURITY_DELAY"=sum(is.na(fd.df$SECURITY_DELAY)),"LATE_AIRCRAFT_DELAY"=sum(is.na(fd.df$LATE_AIRCRAFT_DELAY)),
"CANCELLED"=sum(is.na(fd.df$CANCELLED)),"DIVERTED"=sum(is.na(fd.df$DIVERTED)),"TAXI_OUT"=sum(is.na(fd.df$TAXI_OUT)),"TAXI_IN"=sum(is.na(fd.df$TAXI_IN)),"WHEELS_OFF"=sum(is.na(fd.df$WHEELS_OFF)),"WHEELS_ON"=sum(is.na(fd.df$WHEELS_ON)),"CANCELLATION_CODE"=sum(is.na(fd.df$CANCELLATION_CODE)),"AIR_TIME"=sum(is.na(fd.df$AIR_TIME)),
"DISTANCE"=sum(is.na(fd.df$DISTANCE)),"OP_CARRIER_FL_NUM"=sum(is.na(fd.df$OP_CARRIER_FL_NUM)),"Unnamed..27"=sum(is.na(fd.df$Unnamed..27)))

#Number of Missing Data Points
MissingData <- t(MissingData)
colnames(MissingData) <- "Number of Missing Data Points"

#Percentage of Missing Data
MissingDataPer <- round((MissingData*100/nrow(fd.df)),digits = 2)
MissingDataPer

#Lets remove the variables with high missing data percentages >50% from our focus of analysis.
#fd.updated <- fd.df[,-c("CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY","LATE_AIRCRAFT_DELAY","Unnamed..27 ")] 
fd.updated <- subset(fd.df, select = -c(CARRIER_DELAY,WEATHER_DELAY,NAS_DELAY,SECURITY_DELAY,LATE_AIRCRAFT_DELAY,Unnamed..27) )
str(fd.updated)  
```

After conducting the missing data analysis,it was found that Carrier Delay, in Minutes (CARRIER_DELAY), Weather Delay, in Minutes (WEATHER_DELAY),National Air System Delay, in Minutes (NAS_DELAY), Security Delay, in Minutes (SECURITY_DELAY),	Late Aircraft Delay, in Minutes (LATE_AIRCRAFT_DELAY) had over 80% missing data.
In addition to these four varibles, the last variable Unnamed..27 will need to be removed from the dataset since the missing data percentage is 100%.
Therefor these variables are removed and the data was updated and defined as fd.update.

As we know that the delays are given in minutes, which could be cumbersome to interpret. The overall objective is the make the data as much understandable and interpretable as we can prior to modeling and experimentation.
In the following steps, we will define the variables that is in char format to categorical type (factor). 


#Data manipulation: Conversion of Minutes to Hours and creation of Output Variable
```{r dm1, echo=FALSE, message = FALSE, warning = FALSE}
#Lets define the carrier, origin and destination as factors
fd.updated$ORIGIN <- as.factor(fd.updated$ORIGIN)
fd.updated$DEST <- as.factor(fd.updated$DEST)
fd.updated$OP_CARRIER <- as.factor(fd.updated$OP_CARRIER)
fd.updated$DIVERTED <- as.factor(fd.updated$DIVERTED)

#Analyzing and Creating the output variable
delay.breaks <- c(-150,0,15,3000)
delay.brackets <- c("Early", "Ontime", "Late")
FlightStatus <- cut(fd.updated$ARR_DELAY, breaks = delay.breaks, labels = delay.brackets)


#Lets create a variable to indicate whether a flight is delayed or not
fd.updated$delayed <- as.factor(ifelse(fd.updated$ARR_DELAY>15,1,0))

#Lets convert the unit of delay from minutes to hours and round it to 1 decimal units
fd.updated$ARR_DELAY_HRS <- round(fd.updated$ARR_DELAY/60,digits = 1)
#Lets do the same for departure delays
fd.updated$DEP_DELAY_HRS <- round(fd.updated$DEP_DELAY/60,digits = 1)

num.cf<- sum(fd.updated$CANCELLED)
per.cf <- 100*num.cf/nrow(fd.updated)

cf.df <- subset(fd.updated, fd.updated$CANCELLED>0)
#head(cf.df)

#Lets create morning, afternoon, evening, and redeye flight groups as dep_slot variable.
fd.updated$dep_slot <- NA
#define break points
flight.times <- c(0,600,1200,1700,2400)
flight.types <- c("Redeye","Morning","Afternoon", "Evening")
fd.updated$dep_slot <- cut(fd.updated$DEP_TIME, breaks = flight.times, labels = flight.types)

#Lets group the arrival of flights by the same time slots
fd.updated$arr_slot <- NA
fd.updated$arr_slot <- cut(fd.updated$ARR_TIME, breaks = flight.times, labels = flight.types)

#convert fl_date character to date class
fd.updated$FL_DATE<-as.Date(fd.updated$FL_DATE)
class(fd.updated$FL_DATE)
#Lets create a variable that will keep the month
fd.updated$month <- months(fd.updated$FL_DATE)
#Lets create a variable that will keep the day of the week
fd.updated$day <- weekdays(fd.updated$FL_DATE)
#define the day and month variables as factors (categorical variable)
fd.updated$month<-as.factor(fd.updated$month)
fd.updated$day<- as.factor(fd.updated$day)
```
#DATA VISUALIZATION
``` {r Data Vis,echo=FALSE, message = FALSE, warning = FALSE}
#Cont. table of flights by flight status (early, ontime, delayed)
prop.table(round(table(FlightStatus), digits=2))

#Contingency table of delayed(1) and nondelayed (0) flights
ct.Delayed <- prop.table(table(fd.updated$delayed))
ct.Delayed <- as.data.frame(100*round(ct.Delayed,digits = 2))
rownames(ct.Delayed) <- c("Ontime or Early", "Delayed")
colnames(ct.Delayed) <- c("Code", "Percentage")
ct.Delayed

#CANCELLED FLIGHTS
"Percent of Cancelled Flights:" 
round(per.cf,digits=2)

#Bar plot of cancelled flights 
ggplot(fd.updated,) + geom_bar(aes(x = CANCELLED, fill = delayed)) + labs(title = "Flight Status by Cancellation of Flight", y="Relative frequencies", x= "Flight Type (Cancelled:1, Not Cancelled: 0)")+ geom_text(aes( label = scales::percent(..prop..),y= ..prop..,x = CANCELLED ), stat= "count", vjust = -.5) +theme(legend.position="bottom") 
#Cancelled flights are not delayed flights.

#Boxplot of delays (in hrs)
boxplot(fd.updated$ARR_DELAY_HRS, main="Arriving Flight Delay (Hrs)")

#Bxplot of delays (in hrs)
boxplot(fd.updated$DEP_DELAY_HRS, main="Departure Flight Delay (Hrs)")

#histogram of flights by class of flight by time
cont.table.per <-prop.table(table(fd.updated$dep_slot))
barplot(cont.table.per, main = "Departing Flights",col = "Light blue")
#histogram of flights by class of flight by time
cont.table.per <-prop.table(table(fd.updated$arr_slot))
barplot(cont.table.per, main = "Arriving Flights", col="Light green")

#Bar plot of flights by day
ggplot(fd.updated) + geom_bar(aes(x = day, fill = delayed), position = "dodge")+labs(title = "Flight Status by Week Day", ylab="Number of Flights") 

#Bar plot of flights by month
ggplot(fd.updated) + geom_bar(aes(x = month, fill = delayed), position = "dodge")+labs(title = "Flight Status by Month", ylab="Number of Flights") 

#Bar plot of departing flights by time-slot
ggplot(fd.updated) + geom_bar(aes(x = dep_slot, fill = delayed), position = "dodge")+labs(title = "Flight Status by Departure Time Period", ylab="Number of Flights") 

#Bar plot of arriving flights by time-slot
ggplot(fd.updated,) + geom_bar(aes(x = arr_slot, fill = delayed), position = "dodge")+labs(title = "Flight Status by Arrival Time Period", ylab="Number of Flights") 

#Bar plot of diverted flights 
ggplot(fd.updated,) + geom_bar(aes(x = DIVERTED, fill = delayed), position = "dodge")+labs(title = "Flight Status by Divertion of Flight (Route Change)",ylab="Number of Flights") 

```
#Feature selection
Now,lets narrow down the focus of analysis. Yes, we have a number of variables and in this modeling exercise, we will keep:
    Carrier Type (Airline Company), 
    Origin Airport, 
    Destination Airport, 
    Departure and arrival time slots (Redeye, morning, afternoon, evening), 
    Divertion (whether the flight is a direct flight (0) or connected (1), 
    Month and day of the week 
    and the output: delay (1:Delayed, 0:Not delayed - in other words, no later than 15 mins in terms of arrival time)
               
We exclude the rest of the variables either due to having a huge % of missing data or being irrelevant for classification purposes. 
We know that cancelled fligths take up of roughly 2% of the data thus not considered. Since we are using naive Bayes, an exahustive algorithm to build a classification model, it will be wise to focus on the mosst critical factors from passenger perspective.

#Data Partition
In this section, we will look at the final structure of the data and partition it into train and test subsets.
```{r data split,echo=FALSE, message = FALSE, warning = FALSE}
fd.expdata <- fd.updated[,c("OP_CARRIER","ORIGIN","DEST","DIVERTED","month", "day","dep_slot","arr_slot","delayed")]

#Lets tabularize the structure of fd.expdata
library(knitr)
library(magrittr)
data.frame(variable = names(fd.expdata),
           class = sapply(fd.expdata, typeof),
           First_values = sapply(fd.expdata, function(x) paste0(head(x),  collapse = ", ")),
           row.names = NULL) %>% 
  kable()
               
# Create training and validation sets.
set.seed(1905)
split <- sample(nrow(fd.expdata),nrow(fd.expdata)*0.75) #75-25 split is used.
fd.data.train <- as.data.frame(fd.expdata[split,])
fd.data.test <- as.data.frame(fd.expdata[-split,])
```
           
#Building Naive Bayes: naive Bayes classifier model will be developed with the train data.
```{r nb model,echo=FALSE, message = FALSE, warning = FALSE}
library(e1071) 
library(FNN)
library(caret)

fd.naive <- naiveBayes(delayed~.,data=fd.data.train)
fd.naive
#Below is a memory exhaustive and would take a good amount of time to check the conf. matrix of train data.Left for the future experimentation. 
#pred.class.train <- predict(fd.naive, newdata = fd.data.train, type = "class")
#train.cf <- confusionMatrix(pred.class.train, fd.data.train$delayed)
#train.cf

#Following (caret approach with trainControl includes cross validation)
#However, this training with 10 fold CV takes exponentially higher time
#Therefore, its left here for future experimentation
#train_control <- trainControl(method="cv", number=10)
#fd.naive.model = train(x=fd.data.train[, c(1:9)],y=fd.data.train[, 10],method ='nb',trControl=train_control)
#str(fd.data.train)
```
               
#Pivot tables of classifications on train data
```{r pvt,echo=FALSE, message = FALSE, warning = FALSE}
#install.packages("expss")
library(expss)
library(xtable)
library(tidyverse)
library(dplyr)
# use prop.table() with margin = 1 to convert a count table to a proportion table, 
#Contingency table for delayed % by destination
dest.cont.table<- round((prop.table(table(fd.data.train$DEST,fd.data.train$delayed),margin = 1)),digits = 2)
colnames(dest.cont.table) <- c("Non-delayed", "Delayed") 
print("#Contingency table for delayed % by destination")
#print(dest.cont.table)

#The worst 10 dest in terms of delay probability
ten.dest.cont.table<-as.data.frame(dest.cont.table) 
colnames(ten.dest.cont.table) <- c("Destination","Flight_Status", "Delay_Probability") 
ten.dest.cont.table<-filter(ten.dest.cont.table,Flight_Status=="Delayed")

tenworstdest<-ten.dest.cont.table%>% 
arrange(desc(Delay_Probability)) 

tenworstdest<-head(tenworstdest[,c(1,3)],10)
print("The Worst 10 destination with highest delay probability")
print(tenworstdest)

#Top 10 dest with lowest delay probability
tenbestdest<-as.data.frame(dest.cont.table)
colnames(tenbestdest) <- c("Destination","Flight_Status", "Ontime_Probability") 
tenbestdest<-filter(tenbestdest ,Flight_Status=="Non-delayed")

tenbestdest<-tenbestdest%>% 
arrange(desc(Ontime_Probability)) 
tenbestdest<-head(tenbestdest[,c(1,3)],10)
print("Top 10 origin with lowest delay probability")
print(tenbestdest)

#Contingency table for delayed % by origin
origin.cont.table <- round(prop.table(table(fd.data.train$ORIGIN,fd.data.train$delayed), margin = 1),digits = 2)
colnames(origin.cont.table) <- c("Non-delayed", "Delayed") 
#print("#Contingency table for delayed % by origin")
#print(origin.cont.table)

#The worst 10 origin in terms of delay probability
tenworstorg<-as.data.frame(origin.cont.table) 
colnames(tenworstorg) <- c("Origin","Flight_Status", "Delay_Probability") 
tenworstorg<-filter(tenworstorg,Flight_Status=="Delayed")

tenworstorg<-tenworstorg%>% 
arrange(desc(Delay_Probability)) 

tenworstorg<-head(tenworstorg[,c(1,3)],10)
print("The Worst 10 origin with highest delay probability")
print(tenworstorg)

#Top 10 origin with lowest delay probability
tenbestorg<-as.data.frame(origin.cont.table)
colnames(tenbestorg) <- c("Origin","Flight_Status", "Ontime_Probability") 
tenbestorg<-filter(tenbestorg ,Flight_Status=="Non-delayed")

tenbestorg<-tenbestorg%>% 
arrange(desc(Ontime_Probability)) 
tenbestorg<-head(tenbestorg[,c(1,3)],10)
print("Top 10 origin with lowest delay probability")
print(tenbestorg)

#Contingency Probability Table by Month
month.cont.table <- round(prop.table(table(fd.data.train$month,fd.data.train$delayed),margin = 1),digits = 2)
colnames(month.cont.table) <- c("Non-delayed", "Delayed") 
print("#Contingency Probability Table by Month")
print(month.cont.table)

#Contingency Probability Table by Day
day.cont.table <-round(prop.table(table(fd.data.train$day,fd.data.train$delayed),margin = 1),digits = 2)
colnames(day.cont.table) <- c("Non-delayed", "Delayed") 
print("#Contingency Probability Table by Day")
print(day.cont.table)

```
Lets take a look at the naive Bayes classification model performance with the test data

```{r prob and class mem,echo=FALSE, message = FALSE, warning = FALSE}
library(caret)
#probabilities
pred.prob.fd <- predict(fd.naive, newdata = fd.data.test, type = "raw")
## predict class membership
pred.class <- predict(fd.naive, newdata = fd.data.test, type = "class")
test.cf <- confusionMatrix(pred.class, fd.data.test$delayed)
test.cf

```
Lets assume that we are flyign with American Airlines (AA),from ABQ to JFK,no diversion, on a Wednesday, October 28, 2020, scheduled to be at 1:00AM (Redeye) and arriving at 6:30AM (Morning)
           
```{r predicting a new flight,echo=FALSE, message = FALSE, warning = FALSE}
#Define the new passenger data
new.pass.data <- c("AA", "ABQ","JFK",0,"October", "Wednesday","Redeye","Morning","")
#Get the first row from the experiment data to have the column names and structure
new.passenger <- fd.expdata[1,]
#Update the passenger data with the new passenger data
new.passenger[,c(1:9)] <- new.pass.data
#Predict the probability of delay
pred.prob.fd <- predict(fd.naive, newdata = new.passenger, type = "raw")
pred.prob.fd
## predict class membership
pred.class <- predict(fd.naive, newdata = new.passenger, type = "class")
pred.class
#Prediction of new passenger
print(pred.class)
```

           
Results indicate that 81% accuracy in classification of delayed flights is obtained with the Naive Bayes model. Kapp statistic was found to be 0.12 and reasonable. Cross validation and confusion matrix of training models were not generated due to being computationally exhaustive but the specific lines of codes were left for self-study. 
Lastly, a prediction was made for a new passenger who is flying from Albuquerque (ABQ) to New York (JFK) on a Wednesday Redeye flight October 28, 2020 and scheduled to be arriving in the morning. The predicted probability of delay was found to be 4.09% and the flight was classified as ontime. Thanks. Feel free to comment, and reach out to me for collaborative work.