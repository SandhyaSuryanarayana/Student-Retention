getwd()

nrow(Fall_2011_ST)
sum(is.na(Fall_2011_ST))
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

##Student progress data

str(Fall_2011_SP)

Fall_2011_SP$CompleteDevMath <- as.factor(Fall_2011_SP$CompleteDevMath)

Fall_2011_SP$CompleteDevEnglish <- as.factor(Fall_2011_SP$CompleteDevEnglish)

Fall_2011_SP$Complete1 <- as.factor(Fall_2011_SP$Complete1)

Fall_2011_SP$Complete2 <- as.factor(Fall_2011_SP$Complete2)

Fall_2011_SP$TransferIntent <- as.factor(Fall_2011_SP$TransferIntent)

Fall_2011_SP$DegreeTypeSought <- as.factor(Fall_2011_SP$DegreeTypeSought)

summary(Fall_2011_SP)

summary(Fall_2011_ST)


sum(is.na(Fall_2011_ST$GatewayMathStatus))

st_sp <- merge(Fall_2011_ST,Fall_2011_SP,by="StudentID")

View(st_sp)


a<-st_sp[c(1,11,12,25,26,27,28,29,30,31,32,33,34,35,38,39,40,41,42,43,44,45,46,47,48,49,50,51)]

View(a)

colnames(st_sp)

View(Financial_Aid)
