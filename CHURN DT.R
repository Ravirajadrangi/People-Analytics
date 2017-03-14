ChurnData <- read.csv("~EmployeeChurn/EmployeeChurn/ChurnData.csv")
MYdataset <- ChurnData
str(MYdataset)
library(plyr)
library(dplyr)
StatusCount<- as.data.frame.matrix(MYdataset %>%
                                     group_by(STATUS_YEAR) %>%
                                     select(STATUS) %>%
                                     table())
StatusCount$TOTAL<-StatusCount$ACTIVE + StatusCount$TERMINATED
StatusCount$PercentTerminated <-StatusCount$TERMINATED/(StatusCount$TOTAL)*100
StatusCount

##YEAR    ACTIVE    TERMED TOTAL     PercentTerminated
## 2005   4445        271  4579          5.926403
## 2007   4521        295  4683          6.459321
## 2008   4603        304  4767          6.440319
## 2009   4710        294  4852          5.926628
## 2010   4840        271  4963          5.478340
## 2011   4972        260  5082          5.164502
## 2012   5101        280  5231          5.485184
## 2013   5215        255  5320          4.973684
## 2014   4962        551  5215          7.851390
## 2015   4799        312  4961          6.265471

mean(StatusCount$PercentTerminated)

## [1] 5.997124
library(ggplot2)
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(BUSINESS_UNIT),fill = as.factor(STATUS)),data=MYdataset,position = position_stack())
#Just Terminates By Termination Type And Status Year
TerminatesData<- as.data.frame(MYdataset %>%
filter(STATUS=="TERMINATED"))
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termtype_desc)),data=TerminatesData,position = position_stack())
##Density Plots
library(caret)
## Loading required package: lattice
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="density",auto.key = list(columns = 2))
#Box plot
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="box",auto.key = list(columns = 2))

##MOdel Buildingusing Rattle
library(rattle)
library(magrittr)
crv$seed 42

#Create training and testing datasets

set.seed(crv$seed) 
MYnobs nrow(MYdataset) # 52692 observations 
MYsample subset(MYdataset,STATUS_YEAR<=2014)
MYvalidate NULL
MYtest subset(MYdataset,STATUS_YEAR== 2015)

MYinput c("age", "length_of_service",    "gender_full", "STATUS_YEAR", "BUSINESS_UNIT")

MYnumeric c("age", "length_of_service", "STATUS_YEAR")

MYcategoric c("gender_full", "BUSINESS_UNIT")

MYtarget  "STATUS"
MYrisk    NULL
MYident   "EmployeeID"
MYignore  c("recorddate_key", "birthdate_key", "orighiredate_key", "terminationdate_key", "city_name", "gender_short", "termreason_desc", "termtype_desc","department_name","job_title", "store_name")
MYweights NULL

MYTrainingData<-MYtrain[c(MYinput, MYtarget)]
MYTestingData<-MYtest[c(MYinput, MYtarget)]


# Build the Decision Tree model.

MYrpart rpart(STATUS ~ .,
              data=MYtrain[, c(MYinput, MYtarget)],
              method="class",
              parms=list(split="information"),
              control=rpart.control(usesurrogate=0, 
                                    maxsurrogate=0))
print(MYrpart)
fancyRpartPlot(MYrpart, main="Decision Tree ChurnData $ STATUS")
