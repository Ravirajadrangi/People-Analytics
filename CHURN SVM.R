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


# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)
## 
## Attaching package: 'kernlab'
## 
## The following object is masked from 'package:ggplot2':
## 
##     alpha
# Build a Support Vector Machine model.

set.seed(crv$seed)
MYksvm ksvm(as.factor(STATUS) ~ .,
            data=MYtrain[c(MYinput, MYtarget)],
            kernel="rbfdot",
            prob.model=TRUE)

# Generate a textual view of the SVM model.

MYksvm
## Support Vector Machine object of class "ksvm" 
## 
## SV type: C-svc  (classification) 
##  parameter : cost C = 1 
## 
## Gaussian Radial Basis kernel function. 
##  Hyperparameter : sigma =  0.365136817631195 
## 
## Number of Support Vectors : 2407 
## 
## Objective Function Value : -2004.306 
## Training error : 0.017811 
## Probability model included.
# Time taken: 42.91 secs
MYpr predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))

# Generate the confusion matrix showing counts.

table(na.omit(MYtest[c(MYinput, MYtarget])]$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))
##             Predicted
## Actual       ACTIVE TERMINATED
##   ACTIVE       4799          0
##   TERMINATED    150         12
# Generate the confusion matrix showing proportions.

pcme table(actual, cl)
  nc nrow(x)
  tbl cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) c("Actual", "Predicted")
  return(tbl)
}
per pcme(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr)
round(per, 2)
##             Predicted
## Actual       ACTIVE TERMINATED Error
##   ACTIVE       0.97          0  0.00
##   TERMINATED   0.03          0  0.93
# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
## 3
# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
## 46

