getwd()
##Fall 2011 static data 
nrow(Fall_2011_ST)
sum(is.na(Fall_2011_ST$Address1))
summary(Fall_2011_ST)
str(Fall_2011_ST)

#Fall 2011 progress data
nrow(Fall_2011_SP)
sum(is.na(Fall_2011_SP))
summary(Fall_2011_SP)
str(Fall_2011_SP)

#Financial Aid table
View(Financial_Aid)
nrow(Financial_Aid) #13769
sum(is.na(Financial_Aid)) #305867
str(Financial_Aid)
View(Financial_Aid_1)


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

##Student progress data

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

summary(Fall_2011_ST)
nrow(Fall_2011_ST)

sum(is.na(Fall_2011_ST$GatewayMathStatus))

st_sp <- merge(Fall_2011_ST,Fall_2011_SP,by="StudentID")

View(st_sp)

nrow(st_sp) ##1759

a<-st_sp[c(1,11,12,25,26,27,28,29,30,31,32,33,34,35,38,39,40,41,42,43,44,45,46,47,48,49,50,51)]

View(a)

colnames(st_sp)

names(Financial_Aid)[4] <- "MaritalStatus"
names(Financial_Aid)[6] <- "ParentAdjustedGrossIncome"
names(Financial_Aid)[7] <- "FatherHighestGradeLevel"
names(Financial_Aid)[8] <- "MotherHighestGradeLevel"
names(Financial_Aid)[9] <- "2012Loan"
names(Financial_Aid)[10] <- "2012Scholarship"
names(Financial_Aid)[11] <- "2012Work/Study"
names(Financial_Aid)[11] <- "2012Grant"
summary(Financial_Aid)

Financial_Aid_1 <- Financial_Aid[c(1,4,5,6,7,8,9,10,11,12,13)]

View(Financial_Aid_1)
str(Financial_Aid)

Financial_Aid$`Marital Status` <- as.factor(Financial_Aid$`Marital Status`)

Financial_Aid$StudentID <- as.numeric(Financial_Aid$StudentID)

names(Financial_Aid)[1] <- "StudentID"



a_b_c <- merge(Financial_Aid,a,by="StudentID")

View(a_b_c)

nrow(a_b_c)

names(a_b_c)

str(TrainLabels)

TrainLabels$StudentID <- as.numeric(TrainLabels$StudentID)

TrainLabels$Dropout <- as.factor(TrainLabels$Dropout)

withtrain <- merge(a_b_c,TrainLabels,by = "StudentID")

View(withtrain)

names(withtrain)


Final_data <- withtrain[c(1,)]


