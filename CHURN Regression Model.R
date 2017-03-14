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

# Build a Regression model.

MYglm glm(STATUS ~ .,
          data=MYtrain[c(MYinput, MYtarget)],
          family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(MYglm))
## 
## Call:
## glm(formula = STATUS ~ ., family = binomial(link = "logit"), 
##     data = MYtrain[c(MYinput, MYtarget)])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3245  -0.2076  -0.1564  -0.1184   3.4080  
## 
## Coefficients:
##                       Estimate Std. Error z value Pr(>|z|)    
## (Intercept)         -893.51883   33.96609 -26.306  < 2e-16 ***
## age                    0.21944    0.00438  50.095  < 2e-16 ***
## length_of_service     -0.43146    0.01086 -39.738  < 2e-16 ***
## gender_fullMale        0.51900    0.06766   7.671  1.7e-14 ***
## STATUS_YEAR            0.44122    0.01687  26.148  < 2e-16 ***
## BUSINESS_UNITSTORES   -2.73943    0.16616 -16.486  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 11920.1  on 44691  degrees of freedom
## Residual deviance:  9053.3  on 44686  degrees of freedom
## AIC: 9065.3
## 
## Number of Fisher Scoring iterations: 7
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(MYglm)[1],
            attr(logLik(MYglm), "df")))
## Log likelihood: -4526.633 (6 df)
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            MYglm$null.deviance-MYglm$deviance,
            MYglm$df.null-MYglm$df.residual))
## Null/Residual deviance difference: 2866.813 (5 df)
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(MYglm$null.deviance-MYglm$deviance,
                   MYglm$df.null-MYglm$df.residual)))
## Chi-square p-value: 0.00000000
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(MYglm$y, MYglm$fitted.values)))
## Pseudo R-Square (optimistic): 0.38428451
cat('\n==== ANOVA ====\n\n')
## 
## ==== ANOVA ====
print(anova(MYglm, test="Chisq"))
## Analysis of Deviance Table
## 
## Model: binomial, link: logit
## 
## Response: STATUS
## 
## Terms added sequentially (first to last)
## 
## 
##                   Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
## NULL                              44691    11920.1              
## age                1   861.75     44690    11058.3 < 2.2e-16 ***
## length_of_service  1  1094.72     44689     9963.6 < 2.2e-16 ***
## gender_full        1    14.38     44688     9949.2 0.0001494 ***
## STATUS_YEAR        1   716.39     44687     9232.8 < 2.2e-16 ***
## BUSINESS_UNIT      1   179.57     44686     9053.3 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
cat("\n")
# Time taken: 1.62 secs
# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

MYpr as.vector(ifelse(predict(MYglm, type=green: "response", newdata=MYtest[c(MYinput, MYtarget)]) > 0.5, green: "TERMINATED", "ACTIVE"))

# Generate the confusion matrix showing counts.

table(na.omit(MYtest[c(MYinput, MYtarget])]$STATUS, MYpr,
dnn=c("Actual", "Predicted"))
##             Predicted
## Actual       ACTIVE
##   ACTIVE       4799
##   TERMINATED    162
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
## Actual       ACTIVE Error
##   ACTIVE       0.97     0
##   TERMINATED   0.03     1
# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))
## -97
# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))
