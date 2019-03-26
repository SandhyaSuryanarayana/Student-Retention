
getwd()
setwd('F:\\SEM-2\\Machine learning\\New\\Inputfiles')
##----Load packages----

library(Hmisc) #Datacleaning
library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(stringr)
library(leaps)
library(e1071)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)





Financial_Aid <- read.csv('Financial_Aid.csv',na.strings = "")
Progress <- read.csv('Progress.csv',na.strings = "")
static <- read.csv('Static.csv',na.strings = "")

names(Financial_Aid)[1] <- "StudentID"
names(Financial_Aid)[3] <-"cohortterm"
names(Financial_Aid)[4] <- "MaritalStatus"
names(Financial_Aid)[5] <- "AdjustedGrossIncome"
names(Financial_Aid)[6] <- "ParentAdjustedGrossIncome"
names(Financial_Aid)[7] <- "FatherHighestGradeLevel"
names(Financial_Aid)[8] <- "MotherHighestGradeLevel"
names(Financial_Aid)[10] <- "Loan2012"
names(Financial_Aid)[11] <- "Scholarship2012"
names(Financial_Aid)[12] <- "WorkStudy2012"
names(Financial_Aid)[13] <- "Grant2012"
names(Financial_Aid)[14] <- "Loan2013"
names(Financial_Aid)[15] <- "Scholarship2013"
names(Financial_Aid)[16] <- "WorkStudy2013"
names(Financial_Aid)[17] <- "Grant2013"
names(Financial_Aid)[18] <- "Loan2014"
names(Financial_Aid)[19] <- "Scholarship2014"
names(Financial_Aid)[20] <- "WorkStudy2014"
names(Financial_Aid)[21] <- "Grant2014"
names(Financial_Aid)[22] <- "Loan2015"
names(Financial_Aid)[23] <- "Scholarship2015"
names(Financial_Aid)[24] <- "WorkStudy2015"
names(Financial_Aid)[25] <- "Grant2015"
names(Financial_Aid)[26] <- "Loan2016"
names(Financial_Aid)[27] <- "Scholarship2016"
names(Financial_Aid)[28] <- "WorkStudy2016"
names(Financial_Aid)[29] <- "Grant2016"
names(Financial_Aid)[30] <- "Loan2017"
names(Financial_Aid)[31] <- "Scholarship2017"
names(Financial_Aid)[32] <- "WorkStudy2017"
names(Financial_Aid)[33] <- "Grant2017"

##----4.Data Manipulation----
Merge1 <- merge(Progress,static,by='StudentID')
Merge2 <- merge(Merge1,Financial_Aid,by='StudentID')

Merge2$StudentID<- as.numeric(Merge2$StudentID)

Merge2$Term <- as.factor(Merge2$Term)

Merge2$CompleteDevMath <- as.factor(Merge2$CompleteDevMath)

Merge2$CompleteDevEnglish <- as.factor(Merge2$CompleteDevEnglish)

Merge2$Complete1 <- as.factor(Merge2$Complete1)

Merge2$Gender <- as.factor(Merge2$Gender)

Merge2$BirthYear <- as.numeric(Merge2$BirthYear)

Merge2$BirthYear <- replace(Merge2$BirthYear,is.na(Merge2$BirthYear),1989)

Merge2$Race <- ifelse(Merge2$Hispanic==1,1,
                     ifelse(Merge2$AmericanIndian==1,2,
                     ifelse(Merge2$Asian==1,3,
                     ifelse(Merge2$Black==1,4,
                     ifelse(Merge2$NativeHawaiian==1,5,
                     ifelse(Merge2$White==1,6,
                     ifelse(Merge2$TwoOrMoreRace==1,7
                            )))))))
Merge2$Race <- replace(Merge2$Race,is.na(Merge2$Race),-1)

Merge2$Race <- as.factor(Merge2$Race)

sum(is.na(Merge2$State))

Merge2$State <- replace(Merge2$State,is.na(Merge2$State),'NJ')

Merge2$HsDip <- as.factor(Merge2$HsDip)

Merge2$HSDipYr<-ifelse(Merge2$HSDipYr != -1,year(as.Date(as.character(Merge2$HSDipYr),"%Y")),-1)

Merge2$Enrollmentstatus<- as.factor(Merge2$Enrollmentstatus)

Merge2$HighDeg <- as.factor(Merge2$HighDeg)

Merge2$MathPlacement <- as.factor(Merge2$MathPlacement)

Merge2$EngPlacement <- as.factor(Merge2$EngPlacement)

Merge2$GatewayMathStatus <-as.factor(Merge2$GatewayMathStatus)

Merge2$GatewayEnglishStatus<-as.factor(Merge2$GatewayEnglishStatus)


Merge2$MaritalStatus <- as.factor(Merge2$MaritalStatus)

Merge2$MaritalStatusNew <- impute(Merge2$MaritalStatus,'Single')

Merge2$MaritalStatusNew <- as.factor(Merge2$MaritalStatusNew)

Merge2$FatherHighestGradeLevel <- impute(Merge2$FatherHighestGradeLevel,'Unknown')
Merge2$MotherHighestGradeLevel <- impute(Merge2$MotherHighestGradeLevel,'Unknown')

Merge2$AdjustedGrossIncome <- impute(Merge2$AdjustedGrossIncome,0)
Merge2$ParentAdjustedGrossIncome <- impute(Merge2$ParentAdjustedGrossIncome,0)

Merge2$Loan2012 <- impute(Merge2$Loan2012,0)
Merge2$Scholarship2012 <- impute(Merge2$Scholarship2012,0)
Merge2$WorkStudy2012 <- impute(Merge2$WorkStudy2012,0)
Merge2$Grant2012 <- impute(Merge2$Grant2012,0)

Merge2$Loan2013 <- impute(Merge2$Loan2013,0)
Merge2$Scholarship2013 <- impute(Merge2$Scholarship2013,0)
Merge2$WorkStudy2013 <- impute(Merge2$WorkStudy2013,0)
Merge2$Grant2013 <- impute(Merge2$Grant2013,0)

Merge2$Loan2014 <- impute(Merge2$Loan2014,0)
Merge2$Scholarship2014 <- impute(Merge2$Scholarship2014,0)
Merge2$WorkStudy2014 <- impute(Merge2$WorkStudy2014,0)
Merge2$Grant2014 <- impute(Merge2$Grant2014,0)


Merge2$Loan2015 <- impute(Merge2$Loan2015,0)
Merge2$Scholarship2015 <- impute(Merge2$Scholarship2015,0)
Merge2$WorkStudy2015 <- impute(Merge2$WorkStudy2015,0)
Merge2$Grant2015 <- impute(Merge2$Grant2015,0)

Merge2$Loan2016 <- impute(Merge2$Loan2016,0)
Merge2$Scholarship2016 <- impute(Merge2$Scholarship2016,0)
Merge2$WorkStudy2016 <- impute(Merge2$WorkStudy2016,0)
Merge2$Grant2016 <- impute(Merge2$Grant2016,0)

Merge2$Loan2017 <- impute(Merge2$Loan2017,0)
Merge2$Scholarship2017 <- impute(Merge2$Scholarship2017,0)
Merge2$WorkStudy2017 <- impute(Merge2$WorkStudy2017,0)
Merge2$Grant2017 <- impute(Merge2$Grant2017,0)

Merge2$Funds12 <- ifelse(Merge2$Scholarship2012 > 0 | Merge2$WorkStudy2012 > 0 | 
                               Merge2$Grant2012 >0 | Merge2$Loan2012>0,1,0 )
Merge2$Funds13 <- ifelse(Merge2$Scholarship2013 > 0 | Merge2$WorkStudy2013 > 0 | 
                               Merge2$Grant2013 >0|Merge2$Loan2013 > 0,1,0 )

Merge2$Funds14 <- ifelse(Merge2$Scholarship2014 >0 | Merge2$WorkStudy2014 > 0 | 
                               Merge2$Grant2014 >0 | Merge2$Loan2014>0,1,0 )
Merge2$Funds15 <- ifelse(Merge2$Scholarship2015 >0 | Merge2$WorkStudy2015 > 0 | 
                               Merge2$Grant2015 >0 | Merge2$Loan2015>0,1,0 )
Merge2$Funds16 <- ifelse(Merge2$Scholarship2016 >0 | Merge2$WorkStudy2016 > 0 | 
                               Merge2$Grant2016 >0 | Merge2$Loan2016>0,1,0 )
Merge2$Funds17 <- ifelse(Merge2$Scholarship2017 >0 | Merge2$WorkStudy2017 > 0 | 
                               Merge2$Grant2017 >0 | Merge2$Loan2017>0,1,0 )


Merge2$Funds12<- as.factor(Merge2$Funds12)
Merge2$Funds13  <- as.factor(Merge2$Funds13) 
Merge2$Funds14 <- as.factor(Merge2$Funds14)
Merge2$Funds15 <- as.factor(Merge2$Funds15)
Merge2$Funds16 <- as.factor(Merge2$Funds16)
Merge2$Funds17 <- as.factor(Merge2$Funds17)


TrainLabel <- read.csv('DropoutTrainLabels.csv')
TrainLabel$StudentID <- as.numeric(TrainLabel$StudentID)
TrainLabel$Dropout <- as.factor(TrainLabel$Dropout)

TrainData <- merge(Merge2,TrainLabel,by='StudentID')

#colSums(is.na(TrainData))
#getwd()
##----Test table----
TestID <- read.csv('TestIDs.csv')
TestID$StudentID <- as.numeric(TestID$StudentID)
TestData <- merge(Merge2,TestID,by='StudentID')

##----Exploratory Data Analysis----

plot(TrainData$MaritalStatusNew)

box(TrainData$Race,TrainData$Dropout)


boxplot(CumGPA~ Dropout,
        data=TrainData,
        main="BoxPlot for Dropouts for different GPA",
        xlab="DropOut",
        ylab="Cumulative GPA",
        col="Sky blue",
        border="brown")


boxplot(NumColCredAttemptTransfer~ Dropout,
        data=TrainData,
        main="BoxPlot for Dropouts for different GPA",
        xlab="DropOut",
        ylab="NumColCredAttemptTransfer",
        col="Sky blue",
        border="brown")



summary(TrainData)

plot(TrainData$Funds12)
plot(TrainData$Funds13)
plot(TrainData$Funds14)
plot(TrainData$Funds15)
plot(TrainData$Funds16)
plot(TrainData$Funds17)


table(TrainData$AcademicYear,TrainData$Dropout)

table(TrainData$Race,TrainData$Dropout)
plot(CumGPA~Dropout,data=TrainData)

featurePlot(x=TrainData[,c(8,5)], y=iris[,2], plot="pairs", auto.key=list(columns=3))

colnames(TrainData)

Feature_1 <- TrainData[c("StudentID","Term","CompleteDevMath","CompleteDevEnglish",
                         "Major1","Complete1","CumGPA","State","Gender" ,"BirthYear",
                         "HsDip","HSDipYr","Enrollmentstatus","NumColCredAttemptTransfer",
                         "GatewayMathStatus","GatewayEnglishStatus","FatherHighestGradeLevel",
                         "MotherHighestGradeLevel","Race","Funds12" ,"Funds13","Funds14","Funds15","Funds16",
                         "Funds17", "MaritalStatusNew","Dropout")]
#TermGPA,#HSGPAUnwtd,37,37

##----5.Correlation matrix----
Feature_cor <- TrainData[c(1,2,5,6,7,8,11,13,14,21,22,24,25,
                            30,31,40,41,42,43,44,45,46,47,48,49,50,51,
                            52,53,54,55,56,57,58,59,60,61,62,63,64,66,67.68,69,70
                            ,71)]##12,37,38,65,66
correlation_1 <- as.matrix(Feature_cor,use="pairwise.complete.obs")
cor_mat<-rcorr(correlation_1)
cor_mat
corrplot(cor_mat$r, type="upper", order="hclust", 
         p.mat = cor_mat$P, sig.level = 0.01, insig = "blank")

write.csv(cbind.data.frame(cor_mat$r),'cor.mat.csv')

Feature_co_2r <- TrainData[c(5,28,29,10,11,24,25,26,40,41,42,43,44,45,46)]
correlation_2 <- as.matrix(Feature_co_2r,use="pairwise.complete.obs")
cor_mat<-rcorr(correlation_2)
cor_mat$r
corrplot(cor_mat$r, type="upper", order="hclust", 
         p.mat = cor_mat$P, sig.level = 0.01, insig = "blank")

colnames(corr(correlation_2))

corrplot(correlation_2)

summary(TrainData)

##----6.BestSubset selection----
colnames(MergeInput)
Feature_subset <- Feature_1[c(1,2,7,8,11,13,14,22,24,27,28,37,38
                               ,64,65,66,67,68,69,70,71,72)]

Feature_subset <- Feature_1[c("StudentID","Term","CompleteDevMath",
                              "Major1","Complete1","CumGPA","Gender" ,"BirthYear",
                              "HsDip","Enrollmentstatus","FatherHighestGradeLevel",
                              "MotherHighestGradeLevel",
                              "Race","Funds12" ,"Funds13","Funds14","Funds15","Funds16",
                              "Funds17", "MaritalStatusNew","Dropout")]
bstsubset <- regsubsets(Dropout ~.-StudentID, data=Feature_subset,nvmax=20)
reg.summary <-summary(bstsubset)
reg.summary$adjr2#20
reg.summary$bic#16
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)

plot(reg.summary$bic,xlab="Number of Variables",ylab="bic",type="l")
points(16,reg.summary$bic[16], col="red",cex=2,pch=20)
coef(bstsubset,16)

plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted R^2",type="l")
points(20,reg.summary$adjr2[20], col="red",cex=2,pch=20)
coef(bstsubset,20)

##----Model 2 SVm Radial Kernal----

intrain <- createDataPartition(Feature_1$Dropout,p=0.80,list = FALSE)
train1 <- Feature_1[intrain,]
test1 <- Feature_1[-intrain,]
#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(C=c(.1,0.5,1,2,5,10), sigma=c(0.03,0.04,0.05,0.07,0.09,0.1))
#Fit the SVM model to training data
#svmRadial uses the Radial Kernel.  
#You can explicitly specify the cost parameter by using the C = option 
#and the gamma (sigma) parameter using the sigma = option
DropSVMRad_train <- train(Dropout ~ Term+CompleteDevMath+Major1+Complete1+CumGPA 
                   + Gender+Race+ BirthYear+Funds12+Funds13
                   +Funds16+Funds17
                   ,method="svmRadial",
                   trControl=trctrl,data=train1)
DropSVMRad
#See model fit details
DropSVMRad$finalModel
DropSVMRad$bestTune
#See the results details by each optimization run
DropSVMRad$results
#Predict test dataset
SVMpredict <- predict(DropSVMRad,test1)
confusionMatrix(SVMpredict,test1$Dropout)

##For Test data
trctrl <- trainControl(method = "cv", number = 10)
DropSVMRad <- train(Dropout ~ Term+CompleteDevMath+Major1+Complete1+CumGPA 
                    + Gender+Race+ BirthYear+Funds12+Funds13
                    +Funds16+Funds17
                    ,method="svmRadial",trControl=trctrl,data=TrainData)
DropSVMRad
#See model fit details
DropSVMRad$finalModel
#See the tuning parametrs used (cost C, and sigma of the radial kernel function)
DropSVMRad$bestTune
#See the results details by each optimization run
DropSVMRad$results
#Predict test dataset
SVMpredict_Test <- predict(DropSVMRad,TestData)
SVMpredict_Test_bind <- cbind.data.frame(TestData$StudentID,SVMpredict_Test)
colnames(SVMpredict_Test_bind) <- c('StudentID','Dropout')
write.csv(SVMpredict_Test_bind,'SVMpredict_Test_bind.csv')

##----Model 3.bagging----
intrain <- createDataPartition(Feature_1$Dropout,p=0.80,list = FALSE)
train1 <- Feature_1[intrain,]
test1 <- Feature_1[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
BagFit <- train(Dropout ~Term+CompleteDevMath+Major1+Complete1+CumGPA 
                + Gender+Race+ BirthYear+Funds12+Funds13
                +Funds16+Funds17, data = train1, method = "treebag",
                 trControl=trctrl)
BagFit
BegFitPredict <- predict(BagFit, newdata = test1)
confusionMatrix(BegFitPredict,test1$Dropout)
#To see the importance of the variables
bagImp <- varImp(BagFit, scale=TRUE)
bagImp
plot(bagImp)

##TestData
trctrl <- trainControl(method = "cv", number = 10)
BagFitTest <- train(Dropout ~Term+CompleteDevMath+Major1+Complete1+CumGPA 
                + Gender+Race+ BirthYear+Funds12+Funds13
                +Funds16+Funds17, data = TrainData, method = "treebag",
                trControl=trctrl)
BagFitTest
bagImp <- varImp(BagFit, scale=TRUE)
bagImp
plot(bagImp)

BegFitPredictTest <- predict(BagFitTest, newdata = TestData)

BegFitPredictTestbind <- cbind.data.frame(TestData$StudentID,BegFitPredictTest)

#names(BegFitPredictTestbind) <- c('StudentID','Dropout')
#write.csv(BegFitPredictTestbind,'BegFitPredictTestbind.csv')

##
##----Model 4- Random Forest----
#Random Forest
SfSplit <- createDataPartition(Feature_1$Dropout,p=0.80,list = FALSE)
train1 <- Feature_1[SfSplit,]
test1 <- Feature_1[-SfSplit,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter
trctrl <- trainControl(method = "cv", number = 10)
#Fit the random forest (method = "rf"). Set importance = TRUE to have the variable importance calculated.
#Parameter mtry in the train function lets you set how many variables are considered at each split
RfTrain <- train(Dropout ~Term+CompleteDevMath+Major1+Complete1+CumGPA 
                    + Gender+Race+ BirthYear+Funds12+Funds13+Funds14+Funds15
                    +Funds16+Funds17, data = train1, method = "rf",importance = T,
                    trControl=trctrl)
##To see model details##
RfTrain#92.5
#To see the tuned mtry parameter.  Mtry is the number of randomly selected predictors
RfTrain$bestTune
#To see the the % variance explained
RfTrain$finalModel
#Plot complexity parameter tuning runs
plot(RfTrain)
#Predict
RfTrainPredict<- predict(RfTrain, newdata = test1)
#Calculate MSE
confusionMatrix(RfTrainPredict,test1$Dropout)
#To see the importance of the variables
forestImp <- varImp(RfTrain)
forestImp
plot(forestImp)


trctrl <- trainControl(method = "cv", number = 10)
#Fit the random forest (method = "rf"). Set importance = TRUE to have the variable importance calculated.
#Parameter mtry in the train function lets you set how many variables are considered at each split
RfTest <- train(Dropout ~Term+CompleteDevMath+Major1+Complete1+CumGPA 
                 + Gender+Race+ BirthYear+Funds12+Funds13+Funds14+Funds15
                 +Funds16+Funds17, data = TrainData, method = "rf",importance = T,
                 trControl=trctrl)
RfTest#92.5
#To see the tuned mtry parameter.  Mtry is the number of randomly selected predictors
RfTest$bestTune
#To see the the % variance explained
RfTest$finalModel
#Plot complexity parameter tuning runs
plot(RfTest)
#Predict
RfTestPredict<- predict(RfTest, newdata = TestData)
RfTestPredictbind <- cbind.data.frame(TestData$StudentID,RfTestPredict)
names(RfTestPredictbind) <- c('StudentID','Dropout')
write.csv(RfTestPredictbind,'RfTestPredictbind.csv')

##----Model1----
colnames(TrainData)
split <- createDataPartition(TrainData$Dropout,p=0.80,list = FALSE)
train1 <- TrainData[split,]
test1 <- TrainData[-split,]
trctrl <- trainControl(method = "cv", number = 10)

##Decision tree:TrainData
tree_fit <- train(Dropout ~ Term+CompleteDevMath+Major1+Complete1+ CumGPA+BirthYear
                  +HSGPAUnwtd+Enrollmentstatus+NumColCredAttemptTransfer+HighDeg
                  +GatewayEnglishStatus+MaritalStatusNew+
                    Funds17+Funds13+Funds15,
                  data = train1, method = "rpart",
                  trControl=trctrl)

tree_fit
plot(tree_fit)
tree_fit$bestTune
tree_fit$results
tree_fit$finalModel
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree_fit$finalModel)
predict_tree <- predict(tree_fit,test1)
confusionMatrix(test1$Dropout,predict_tree)


####
### Decision Tree: Test Data
tree_fit <- train(Dropout ~ Term+CompleteDevMath+Major1+Complete1+ CumGPA+BirthYear
                  +HSGPAUnwtd+Enrollmentstatus+NumColCredAttemptTransfer+HighDeg
                  +GatewayEnglishStatus+MaritalStatusNew+
                    Funds17+Funds13+Funds15,
                  data = TrainData, method = "rpart",
                  trControl=trctrl)

tree_fit
tree_fit
tree_fit$bestTune
tree_fit$finalModel
tree_fit$results
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree_fit$finalModel)
treeImp <- varImp(tree_fit, scale = TRUE)
treeImp
plot(treeImp)

predict_tree <- predict(tree_fit,TestData)
predict_New <- cbind.data.frame(TestData$StudentID,predict_tree)
names(predict_New) <- c('StudentID','DropOuts')
#write.csv(predict_New,'Predict.csv')
plot(tree_fit)

##----ROC curve---
library(pROC)
# ROC Curve
roccurve <- roc(test1$Dropout ~ as.numeric(predict_tree))
roccurve$auc
roccurve$sensitivities
roccurve$specificities
plot(roccurve)

roccurve <- roc(test1$Dropout ~ as.numeric(SVMpredict))
roccurve$auc
roccurve$sensitivities
roccurve$specificities
plot(roccurve)

library(ROCR)
ROCRpred1 <- prediction(as.numeric(predict_tree), as.numeric(test1$Dropout))
ROCRpred2 <- prediction(as.numeric(SVMpredict), as.numeric(test1$Dropout))
ROCRpred3 <- prediction(as.numeric(BegFitPredict), as.numeric(test1$Dropout))
ROCRpred4 <- prediction(as.numeric(RfTrainPredict), as.numeric(test1$Dropout))
#ROC Curve
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')
ROCRperf4 <- performance(ROCRpred4, 'tpr','fpr')
plot(ROCRperf1,col = 'Red')
abline(0, 1)
plot(ROCRperf2, add = TRUE, col = 'Yellow')
plot(ROCRperf3, add = TRUE, col='black')
plot(ROCRperf4, add = TRUE, Col='Orange')
legend(x=-0.1,y=1.3,legend=paste(rep(c("Red","Yellow","Pink","Blue")),
                               rep(c("Decision Tree","SVM predict","Bagging","Random Forest"))
                               ,sep=":"),pch=rep(c(16,18),each=4),bty="n",
                                ncol=2,cex=0.7,pt.cex=0.7,xpd=TRUE)

#Gains curve
ROCRgains <- performance(ROCRpred, 'tpr','rpp')
plot(ROCRgains, colorize = TRUE)
abline(0, 1)
#Lift Curve
ROCRlift <- performance(ROCRpred, 'lift','rpp')
plot(ROCRlift, colorize = TRUE)



##------------------

##----Model 5 Logistic regression----
split <- createDataPartition(TrainData$Dropout,p=0.80,list = FALSE)
train1 <- TrainData[split,]
test1 <- TrainData[-split,]
trctrl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
#str(feature2)

Model_glm <- train(Dropout ~ Term+CompleteDevMath+Major1+Complete1+ CumGPA+BirthYear
                   +HSGPAUnwtd+Enrollmentstatus+NumColCredAttemptTransfer+HighDeg
                   +GatewayEnglishStatus+MaritalStatusNew+Race+
                     Funds17+Funds13+Funds15,
                   data = train1, method='glm',family= 'binomial',
                   trControl = trctrl)
Model_glm
Model_glm$results
predict_glm <- predict(Model_glm,newdata = testT)
predict_glm
confusionMatrix(predict_glm,testT$Dropout)



