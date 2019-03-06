str(ProgressStatic_Merge)

getwd()
##Fall 2011 static data 
nrow(ProgressStatic_Merge) #13126
sum(is.na(ProgressStatic_Merge))
summary(ProgressStatic_Merge)
str(Fall_2011_ST)

ProgressStatic_Merge$studentid<- as.numeric(ProgressStatic_Merge$studentid)

ProgressStatic_Merge$term <- as.factor(ProgressStatic_Merge$term)

ProgressStatic_Merge$CompleteDevMath <- as.factor(ProgressStatic_Merge$CompleteDevMath)

ProgressStatic_Merge$CompleteDevEnglish <- as.factor(ProgressStatic_Merge$CompleteDevEnglish)

ProgressStatic_Merge$complete1 <- as.factor(ProgressStatic_Merge$complete1)

ProgressStatic_Merge$complete2 <- as.factor(ProgressStatic_Merge$complete2)

ProgressStatic_Merge$TransferIntent <- as.factor(ProgressStatic_Merge$TransferIntent)

ProgressStatic_Merge$DegreeTypeSought <- as.factor(ProgressStatic_Merge$DegreeTypeSought)
##-----------End of Progress data update

ProgressStatic_Merge$RegistrationDate<-as.Date(as.character(ProgressStatic_Merge$RegistrationDate),"%Y%m%d")

ProgressStatic_Merge$Gender <- as.factor(ProgressStatic_Merge$Gender)

ProgressStatic_Merge$BirthYear<- format(as.Date(as.character(ProgressStatic_Merge$BirthYear),"%Y"),"%Y")


ProgressStatic_Merge$Hispanic <- as.factor(ProgressStatic_Merge$Hispanic)

ProgressStatic_Merge$AmericanIndian <- as.factor(ProgressStatic_Merge$AmericanIndian )          

ProgressStatic_Merge$Asian<- as.factor(ProgressStatic_Merge$Asian)

ProgressStatic_Merge$Black<- as.factor(ProgressStatic_Merge$Black)

ProgressStatic_Merge$NativeHawaiian<- as.factor(ProgressStatic_Merge$NativeHawaiian)

ProgressStatic_Merge$White<- as.factor(ProgressStatic_Merge$White)

ProgressStatic_Merge$TwoOrMoreRace<- as.factor(ProgressStatic_Merge$TwoOrMoreRace)

ProgressStatic_Merge$HSDip <- as.factor(ProgressStatic_Merge$HSDip)

ProgressStatic_Merge$FirstGen<- as.factor(ProgressStatic_Merge$FirstGen)

ProgressStatic_Merge$DualHSSummerEnroll<- as.factor(ProgressStatic_Merge$DualHSSummerEnroll)

ProgressStatic_Merge$EnrollmentStatus<- as.factor(ProgressStatic_Merge$EnrollmentStatus)

ProgressStatic_Merge$HighDeg <- as.factor(ProgressStatic_Merge$HighDeg)

ProgressStatic_Merge$MathPlacement <- as.factor(ProgressStatic_Merge$MathPlacement)

ProgressStatic_Merge$EngPlacement <- as.factor(ProgressStatic_Merge$EngPlacement)

ProgressStatic_Merge$GatewayMathStatus <-as.factor(ProgressStatic_Merge$GatewayMathStatus)

ProgressStatic_Merge$GatewayEnglishStatus<-as.factor(ProgressStatic_Merge$GatewayEnglishStatus)


#Financial Aid table
View(Financial_Aid)
nrow(Financial_Aid) #13769
colSums(is.na(Financial_Aid)) #305867
str(Financial_Aid)
summary(Financial_Aid)
#View(Financial_Aid_1)

names(Financial_Aid)[1] <- "studentid"
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


Financial_Aid$StudentID <- as.numeric(Financial_Aid$StudentID)
Financial_Aid$cohortterm <- as.factor(Financial_Aid$cohortterm)
Financial_Aid$MaritalStatus <- as.factor(Financial_Aid$MaritalStatus)

#Data cleaning
library(Hmisc)

table(Financial_Aid$MaritalStatus) #Single =10155 

Financial_Aid$MaritalStatusNew <- impute(Financial_Aid$MaritalStatus,"Single")
summary(Financial_Aid$MaritalStatusNew)

Financial_Aid$AdjustedGrossIncome <- impute(Financial_Aid$AdjustedGrossIncome,0)
Financial_Aid$ParentAdjustedGrossIncome <- impute(Financial_Aid$ParentAdjustedGrossIncome,0)
Financial_Aid$FatherHighestGradeLevel <- impute(Financial_Aid$FatherHighestGradeLevel,0)
Financial_Aid$MotherHighestGradeLevel <- impute(Financial_Aid$MotherHighestGradeLevel,0)
Financial_Aid$Loan2012 <- impute(Financial_Aid$Loan2012,0)
Financial_Aid$Scholarship2012 <- impute(Financial_Aid$Scholarship2012,0)
Financial_Aid$WorkStudy2012 <- impute(Financial_Aid$WorkStudy2012,0)
Financial_Aid$Grant2012 <- impute(Financial_Aid$Grant2012,0)

Financial_Aid$Loan2013 <- impute(Financial_Aid$Loan2013,0)
Financial_Aid$Scholarship2013 <- impute(Financial_Aid$Scholarship2013,0)
Financial_Aid$WorkStudy2013 <- impute(Financial_Aid$WorkStudy2013,0)
Financial_Aid$Grant2013 <- impute(Financial_Aid$Grant2013,0)

Financial_Aid$Loan2014 <- impute(Financial_Aid$Loan2014,0)
Financial_Aid$Scholarship2014 <- impute(Financial_Aid$Scholarship2014,0)
Financial_Aid$WorkStudy2014 <- impute(Financial_Aid$WorkStudy2014,0)
Financial_Aid$Grant2014 <- impute(Financial_Aid$Grant2014,0)

Financial_Aid$Loan2015 <- impute(Financial_Aid$Loan2015,0)
Financial_Aid$Scholarship2015 <- impute(Financial_Aid$Scholarship2015,0)
Financial_Aid$WorkStudy2015 <- impute(Financial_Aid$WorkStudy2015,0)
Financial_Aid$Grant2015 <- impute(Financial_Aid$Grant2015,0)


Financial_Aid$Loan2016 <- impute(Financial_Aid$Loan2016,0)
Financial_Aid$Scholarship2012 <- impute(Financial_Aid$Scholarship2016,0)
Financial_Aid$WorkStudy2016 <- impute(Financial_Aid$WorkStudy2016,0)
Financial_Aid$Grant2016 <- impute(Financial_Aid$Grant2016,0)

Financial_Aid$Loan2017 <- impute(Financial_Aid$Loan2017,0)
Financial_Aid$Scholarship2017 <- impute(Financial_Aid$Scholarship2017,0)
Financial_Aid$WorkStudy2017 <- impute(Financial_Aid$WorkStudy2017,0)
Financial_Aid$Grant2017 <- impute(Financial_Aid$Grant2017,0)


View(Financial_Aid)

colnames(Financial_Aid)



##Merge st_sp with financial aid data

MergeAll <- merge(ProgressStatic_Merge,Financial_Aid,by="studentid")
nrow(MergeAll) #13126
View(MergeAll)#1759
colnames(MergeAll)

##Merge labelData

str(DropoutTrainLabels) #12261
DropoutTrainLabels$StudentID <- as.numeric(DropoutTrainLabels$StudentID)
DropoutTrainLabels$Dropout <- as.factor(DropoutTrainLabels$Dropout)
names(DropoutTrainLabels)[1] <- "studentid"


TrainData <- merge(MergeAll,DropoutTrainLabels,by="studentid") #12139
colnames(Merge_Label)
colSums(is.na(Merge_Label))

##Test data temp
View(TestIDs)#1000
TestIDs$StudentID <- as.numeric(TestIDs$StudentID)
names(TestIDs)[1] <- "studentid"
Testdata <- merge(MergeAll,TestIDs,by="studentid")

##--------------------------------------##
##Best subset selection




library(leaps)

bstsubset <- regsubsets(Dropout ~ .,data=Merge_Label,nvmax = 20)
reg.summary <-summary(bstsubset)
reg.summary$adjr2
reg.summary$bic
which.max(reg.summary$adjr2)
which.min(reg.summary$bic)
#PLot
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(13,reg.summary$adjr2[13], col="red",cex=2,pch=20)

plot(bstsubset,scale="adjr2")
coef(bstsubset,13)


par("mar")
par(mar=c(1,1,1,1))

##Decision tree
library(caret)
library(e1071)

colnames(Merge_Label)

intrain <- createDataPartition(Merge_Label$Dropout,p=0.80,list = FALSE)
train1 <- Merge_Label[intrain,]
test1 <- Merge_Label[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
tree_fit <- train(Dropout ~Gender+HSDip+NumColCredAttemptTransfer+NumColCredAcceptTransfer
                  +MathPlacement+GatewayMathStatus+GatewayEnglishStatus+MaritalStatusNew
                  +HSGPAUnwtd
                  , data = train1, method = "rpart",
                  trControl=trctrl)
tree_fit$bestTune
tree_fit$finalModel
plot(tree_fit)

library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(tree_fit$finalModel)
#Predict
predictions <- predict(tree_fit, newdata = test1)
#Performance metrics
confusionMatrix(predictions,test1$Dropout)
#To see the importance of the variables
treeImp <- varImp(tree_fit, scale = TRUE)
treeImp
plot(treeImp)
