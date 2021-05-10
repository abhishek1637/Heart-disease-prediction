library(ggplot2)
library(ggpubr)
library(dplyr)
theme_set(theme_pubr())
#choosing the dataset
data<- read.csv(file.choose(),header=T)  
#viewing the dataset
str(data)
#viewing the dimension of the dataset
dim(data)
summary(data)
table(is.na(data))
#removing null values by inserting mean of that column
#education attribute
education = mean(data$education,na.rm=TRUE)
data$education[is.na(data$education)] = education
#cigsperday
cigsPerDay = mean(data$cigsPerDay,na.rm=TRUE)
data$cigsPerDay[is.na(data$cigsPerDay)] = cigsPerDay
#bpmeds
BPMeds = mean(data$BPMeds,na.rm=TRUE)
data$BPMeds[is.na(data$BPMeds)] = BPMeds
#totchol
totChol = mean(data$totChol,na.rm=TRUE)
data$totChol[is.na(data$totChol)] = totChol
#BMI
BMI = mean(data$BMI,na.rm=TRUE)
data$BMI[is.na(data$BMI)] = BMI
#heartrate
hr = mean(data$heartRate,na.rm=TRUE)
data$heartRate[is.na(data$heartRate)] =hr
#glucose
glucose = mean(data$glucose,na.rm=TRUE)
data$glucose[is.na(data$glucose)] =glucose
#summary after first step of preprocessing
summary(data)
View(data)
#correlation matrix



cor(catdata)
install.packages("corrplot")
library(corrplot)
corrplot(cor(data),method="circle")
#---------correlation end-----
#boxplot------
numdata=select(data,age,cigsPerDay,totChol,sysBP,diaBP,BMI,heartRate,glucose)
boxplot(numdata)
boxplot(age~CVD,data=data, main="Age",
        xlab="CVD", ylab="age")
boxplot(cigsPerDay~CVD,data=data, main="Cigsperday",
        xlab="CVD", ylab="cigsPerDay")
boxplot(totChol~CVD,data=data, main="cholesterol",
        xlab="CVD", ylab="cholesterol")
boxplot(sysBP~CVD,data=data, main="sysBP",
        xlab="CVD", ylab="sysBP")
boxplot(diaBP~CVD,data=data, main="diaBP",
        xlab="CVD", ylab="diaBP")
boxplot(BMI~CVD,data=data, main="BMI",
        xlab="CVD", ylab="BMI")
boxplot(heartRate~CVD,data=data, main="heartRate",
        xlab="CVD", ylab="heartRate")
boxplot(glucose~CVD,data=data, main="glucose",
        xlab="CVD", ylab="glucose")
summary(data$cigsPerDay)
summary(data$sysBP)
summary(data$diaBP)
summary(data$BMI)
summary(data$heartRate)
summary(data$glucose)
summary(data$totChol)
#Removing outliers
up_chol=262+1.5*IQR(data$totChol)
up_chol
low_chol = 206-1.5*IQR(data$totChol)
low_chol

up_cigs=20+1.5*IQR(data$cigsPerDay)
up_cigs

up_sysbp=144+1.5*IQR(data$sysBP)
up_sysbp

up_diabp=90+1.5*IQR(data$diaBP)
up_diabp
low_diabp = 75-1.5*IQR(data$diaBP)
low_diabp

up_bmi=28.03+1.5*IQR(data$BMI)
up_bmi
low_bmi = 23.08-1.5*IQR(data$BMI)
low_bmi

up_heart=83+1.5*IQR(data$heartRate)
up_heart
low_heart= 68-1.5*IQR(data$heartRate)
low_heart

up_gluc=85+1.5*IQR(data$glucose)
up_gluc
low_gluc = 72-1.5*IQR(data$glucose)
low_gluc

mydata = subset(data,(glucose<100 & glucose>55) & 
                  (heartRate<100 & heartRate>47.5) &
                  (BMI<34 & BMI >15.6475) &
                  (diaBP<108 & diaBP >55) &
                 (sysBP<168) & (cigsPerDay<50 & cigsPerDay>-30) &
                  (totChol>125 & totChol<342))
View(mydata)
boxplot(mydata)
boxplot(age~CVD,data=mydata, main="Age",
        xlab="CVD", ylab="age")
boxplot(cigsPerDay~CVD,data=mydata, main="Cigsperday",
        xlab="CVD", ylab="cigsPerDay")
boxplot(totChol~CVD,data=mydata, main="cholesterol",
        xlab="CVD", ylab="cholesterol")
boxplot(sysBP~CVD,data=mydata, main="sysBP",
        xlab="CVD", ylab="sysBP")
boxplot(diaBP~CVD,data=mydata, main="diaBP",
        xlab="CVD", ylab="diaBP")
boxplot(BMI~CVD,data=mydata, main="BMI",
        xlab="CVD", ylab="BMI")
boxplot(heartRate~CVD,data=mydata, main="heartRate",
        xlab="CVD", ylab="heartRate")
boxplot(glucose~CVD,data=mydata, main="glucose",
        xlab="CVD", ylab="glucose")
#-------------boxplot ended
#correlation matrix
corrplot(cor(mydata),method="circle")
#Histogram------------------
hist(mydata$age, main = "Age", col = "lightblue")
hist(mydata$cigsPerDay, main = "Cigarrates", col = "lightblue")
hist(mydata$totChol, main = "cholesterol", col = "lightblue")
hist(mydata$BMI, main = "BMI", col = "lightblue")
hist(mydata$glucose, main = "Glucose", col = "lightblue")
hist(mydata$sysBP, main = "Systolic bp", col = "lightblue")
hist(mydata$diaBP, main = "Diastolic bp", col = "lightblue")
hist(mydata$heartRate, main = "Heart Rate", col = "lightblue")
skewness(mydata$age)
skewness(mydata$cigsPerDay)

#barplot

plot(mydata$sysBP, mydata$diaBP, main="Scatterplot Example",
     xlab="Systolic BP ", ylab="Diastolic BP",pch=18, col=c("red", "blue4"))
plot(mydata$sysBP, mydata$totChol, main="Scatterplot Example",
     xlab="systolic bp ", ylab="cholesterol",pch=20)


counts <- table(mydata$education)
barplot(counts, main="Education",xlab="edu",col=c("chartreuse", "blue4"))

counts1 <- table(mydata$male)
barplot(counts1, main="Gender",xlab="Male", names.arg=c("Female", "Male"))

counts2 <- table(mydata$currentSmoker)
barplot(counts2, main="Current Smoker",xlab="smoker", names.arg=c("No", "Yes"))

counts3 <- table(mydata$CVD)
barplot(counts3, main="heart Disease",xlab="disese", names.arg=c("No", "Yes"))

counts4 <- table(mydata$diabetes)
barplot(counts4, main="Diabetes",xlab="diabetes", names.arg=c("No", "Yes"))
#----------------------------------------------------
#-------------------------------------------------------
#data<- data[1:4240,-16]
set.seed(1234)

id<-sample(2, nrow(mydata), prob= c(0.8, 0.2), replace=T)
datatrain<-mydata[id==1,]
datatest<-mydata[id==2,]

#naive bayes

library(e1071)
library(caret)
library(naivebayes)

colnames(datatrain)
data_nb<-naiveBayes(CVD ~ male+age+education+currentSmoker+ BPMeds+prevalentStroke + prevalentHyp + diabetes+ totChol + sysBP + BMI+ heartRate+ glucose, data= datatrain)
data_nb

pree3<-predict(data_nb, datatest)
confusionMatrix(table(pree3, datatest$CVD))





