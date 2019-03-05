getwd()
##Fall 2011 static data 
nrow(Fall_2011_ST)
sum(is.na(Fall_2011_ST$Address1))
summary(Fall_2011_ST)
str(Fall_2011_ST)

Fall_2011_ST$StudentID<- as.numeric(Fall_2011_ST$StudentID)

Fall_2011_ST$RegistrationDate<-as.Date(as.character(Fall_2011_ST$RegistrationDate),"%Y%m%d")

Fall_2011_ST$Gender <- as.factor(Fall_2011_ST$Gender)

Fall_2011_ST$BirthYear <- format(as.Date(as.character(Fall_2011_ST$BirthYear),"%Y"),"%Y")


Fall_2011_ST$Hispanic <- as.factor(Fall_2011_ST$Hispanic)

Fall_2011_ST$AmericanIndian <- as.factor(Fall_2011_ST$AmericanIndian )          

Fall_2011_ST$Asian<- as.factor(Fall_2011_ST$Asian)

Fall_2011_ST$Black<- as.factor(Fall_2011_ST$Black)

Fall_2011_ST$NativeHawaiian<- as.factor(Fall_2011_ST$NativeHawaiian)

Fall_2011_ST$White<- as.factor(Fall_2011_ST$White)

Fall_2011_ST$TwoOrMoreRace<- as.factor(Fall_2011_ST$TwoOrMoreRace)

Fall_2011_ST$HSDip <- as.factor(Fall_2011_ST$HSDip)

Fall_2011_ST$FirstGen<- as.factor(Fall_2011_ST$FirstGen)

Fall_2011_ST$DualHSSummerEnroll<- as.factor(Fall_2011_ST$DualHSSummerEnroll)

Fall_2011_ST$EnrollmentStatus<- as.factor(Fall_2011_ST$EnrollmentStatus)

Fall_2011_ST$HighDeg <- as.factor(Fall_2011_ST$HighDeg)

Fall_2011_ST$MathPlacement <- as.factor(Fall_2011_ST$MathPlacement)

Fall_2011_ST$EngPlacement <- as.factor(Fall_2011_ST$EngPlacement)

Fall_2011_ST$GatewayMathStatus <-as.factor(Fall_2011_ST$GatewayMathStatus)

Fall_2011_ST$GatewayEnglishStatus<-as.factor(Fall_2011_ST$GatewayEnglishStatus)

#Fall 2011 progress data
nrow(Fall_2011_SP)
sum(is.na(Fall_2011_SP))
summary(Fall_2011_SP)
str(Fall_2011_SP)

str(Fall_2011_SP)

Fall_2011_SP$Term <- as.factor(Fall_2011_SP$Term)

Fall_2011_SP$CompleteDevMath <- as.factor(Fall_2011_SP$CompleteDevMath)

Fall_2011_SP$CompleteDevEnglish <- as.factor(Fall_2011_SP$CompleteDevEnglish)

Fall_2011_SP$Complete1 <- as.factor(Fall_2011_SP$Complete1)

Fall_2011_SP$Complete2 <- as.factor(Fall_2011_SP$Complete2)

Fall_2011_SP$TransferIntent <- as.factor(Fall_2011_SP$TransferIntent)

Fall_2011_SP$DegreeTypeSought <- as.factor(Fall_2011_SP$DegreeTypeSought)

summary(Fall_2011_SP)
nrow(Fall_2011_SP)


#Financial Aid table
View(Financial_Aid)
nrow(Financial_Aid) #13769
colSums(is.na(Financial_Aid)) #305867
str(Financial_Aid)
summary(Financial_Aid)
View(Financial_Aid_1)

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

View(Financial_Aid)

colnames(Financial_Aid)

Financial_Aid_1 <- Financial_Aid[c(1,2,3,34,5,6,7,8,10,11,12,13)]
View(Financial_Aid_1)
colSums(is.na(Financial_Aid_1))

##Merge static and progress data
st_sp <- merge(Fall_2011_ST,Fall_2011_SP,by="StudentID")
View(st_sp)
colnames(st_sp)
st_sp <- st_sp[c(-5,-6,-36,-37)]

nrow(st_sp) ##1759

##Merge st_sp with financial aid data

MergeAll <- merge(st_sp,Financial_Aid_1,by="StudentID")

View(MergeAll)

colnames(MergeAll)

##Merge labelData

Merge_Label <- merge(MergeAll,TrainLabels,by="StudentID")
View(Merge_Label)



