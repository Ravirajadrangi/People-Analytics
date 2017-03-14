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


# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:ggplot2':
## 
##     margin
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
# Build the Random Forest model.

set.seed(crv$seed)
MYrf randomForest(STATUS ~ .,
                  data=MYtrain[c(MYinput, MYtarget)],
                  ntree=500,
                  mtry=2,
                  importance=TRUE,
                  na.action=randomForest::na.roughfix,
                  replace=FALSE)

# Generate textual output of 'Random Forest' model.

MYrf
## 
## Call:
##  randomForest(formula = STATUS ~ ., data = MYtrain[c(MYinput,      MYtarget)], ntree = 500, mtry = 2, importance = TRUE, replace = FALSE,      na.action = randomForest::na.roughfix) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 1.13%
## Confusion matrix:
##            ACTIVE TERMINATED  class.error
## ACTIVE      43366          3 6.917383e-05
## TERMINATED    501        822 3.786848e-01
# The `pROC' package implements various AUC functions.

# Calculate the Area Under the Curve (AUC).

pROC::roc(MYrf$y, as.numeric(MYrf$predicted))
## 
## Call:
## roc.default(response = MYrf$y, predictor = as.numeric(MYrf$predicted))
## 
## Data: as.numeric(MYrf$predicted) in 43369 controls (MYrf$y ACTIVE) < 1323 cases (MYrf$y TERMINATED).
## Area under the curve: 0.8106
# Calculate the AUC Confidence Interval.

pROC::ci.auc(MYrf$y, as.numeric(MYrf$predicted))
## 95% CI: 0.7975-0.8237 (DeLong)
# List the importance of the variables.

rn round(randomForest::importance(MYrf), 2)
rn[order(rn[,3], decreasing=TRUE),]
##                   ACTIVE TERMINATED MeanDecreaseAccuracy MeanDecreaseGini
## age                36.51     139.70                52.45           743.27
## STATUS_YEAR        35.46      34.13                41.50            64.65
## gender_full        28.02      40.03                37.08            76.80
## length_of_service  18.37      18.43                21.38            91.71
## BUSINESS_UNIT       6.06       7.64                 8.09             3.58
# Time taken: 18.66 secs

