Sys.setlocale(locale="Thai")
library(readxl)
library(epiDisplay)

setwd("D:\\6520310203")
setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Lab 3")
dt <- read_excel("child140_practice.xlsx")
dt <- read_excel("teacherstress_practice.xlsx")
dt <- data.frame(dt)
str(dt)

summary(dt)
tab1(dt$teachdur)




dt <- dt[,c(30,1:29)]
str(dt)

tab1(dt$sex)


dt$sex <- ifelse(dt$sex=="ชาย",1,
		ifelse(dt$sex=="หญิง",2,dt$sex))
tab1(dt$sex)

dt$sex <- factor(dt$sex)
levels(dt$sex) <- c("Male","Female")
tab1(dt$sex)



tab1(dt$age)
summary(dt$age)


dt$age <- ifelse(dt$age==14,NA,dt$age)
summary(dt$age)


tab1(dt$teachdur)

dt$teachdur <- sub("ปี","",dt$teachdur)
tab1(dt$teachdur)

dt$teachdur <- as.numeric(dt$teachdur)
summary(dt$teachdur)


dt$teachdur <- ifelse(dt$teachdur=="7เดือน",0.7,dt$teachdur)
summary(dt$teachdur)

tab1(dt$edu)
dt$edu <- ifelse(dt$edu=="ม.ต้น",NA,dt$edu)
dt$edu <- ifelse(dt$edu %in% c("ป.บัณฑิต","ปริญญาตรี","อนุปริญญา"),1,
			ifelse(dt$edu %in% c("ปริญญาโท","สูงกว่าปริญญาโท"),2,dt$edu))
dt$edu <- factor(dt$edu)
levels(dt$edu) <- c("bsc","master+")
tab1(dt$edu)


tapply(dt$teachdur, dt$edu, mean, na.rm=T)
tapply(dt$teachdur, dt$edu, min, na.rm=T)
tapply(dt$teachdur, dt$edu, max, na.rm=T)

summ(dt$teachdur, by=dt$edu, graph=F)    #epiDisplay


tab1(dt$status)

dt$status <- ifelse(dt$status=="โสด",1,2)
tab1(dt$status)


dt$status <- factor(dt$status)
levels(dt$status) <- c("Single","Married")
tab1(dt$status)




tab1(dt$children)
dt$children <- sub("คน","",dt$children)
tab1(dt$children)

dt$children <- ifelse(dt$children %in% c("-","ไม่มี"),0,dt$children)
tab1(dt$children)


dt$children <- as.numeric(dt$children)
summary(dt$children)




dt$children <- ifelse(dt$children==0,1,
			ifelse(dt$children <3,2,3))
tab1(dt$children)

dt$children<- factor(dt$children)
levels(dt$children) <- c("No","1-2","3+")
tab1(dt$children)



table(dt$status,dt$children)
tabpct(dt$status,dt$children,percent="row") 

dt$inc<-  sub("บาท","",dt$inc)
tab1(dt$inc)

dt$inc <- sub("[?]","<=",dt$inc)

dt$inc <- sub("[,]","",dt$inc)
dt$inc <- sub(" ","",dt$inc)
dt$inc <- sub(" ","",dt$inc)
tab1(dt$inc)

dt$inc <- factor(dt$inc)
tab1(dt$inc)



des(dt)

for(i in c(9:20)){
	dt[,i] <- ifelse(dt[,i]=="มากที่สุด",5,
			ifelse(dt[,i]=="มาก",4,
			ifelse(dt[,i]=="ปานกลาง",3,
			ifelse(dt[,i]=="น้อย",2,
			ifelse(dt[,i]=="น้อยที่สุด",1,dt[,i]
			)))))
	dt[,i] <- as.numeric(dt[,i])
}
str(dt)

tab1(dt[,21])
des(dt)


for(i in c(21:30)){
	dt[,i] <- ifelse(dt[,i]=="ไม่เลย",0,
			ifelse(dt[,i]=="แทบจะไม่มี",1,
			ifelse(dt[,i]=="มีบางครั้ง",2,
			ifelse(dt[,i]=="ค่อนข้างบ่อย",3,
			ifelse(dt[,i]=="บ่อยมาก",4,dt[,i]
			)))))
	dt[,i] <- as.numeric(dt[,i])
}
str(dt)

dt$tavg <- rowMeans(dt[,c(9:18)],na.rm=T)
str(dt)

dt$mavg <- rowMeans(dt[,c(19:20)],na.rm=T)
str(dt)

dt$stavg <- rowMeans(dt[,c(21:30)],na.rm=T)
str(dt)



tab1(dt[,18])

dt$behAvg <- factor(dt$behAvg)
dt$behAvg <- as.numeric(dt$behAvg)
summary(dt$behAvg)


dt[, 18:30] <- lapply(dt[, 18:30], function(x) as.numeric(as.character(x)))
str(dt)
dt$behAvg <- rowMeans(dt[,c(18:30)],na.rm=T)
str(dt)

tab1(dt$behAvg)
table(dt$behAvg)



dt$stu_birth_weight <- as.numeric(as.character(dt$stu_birth_weight))
dt$bwgrp <- ifelse(dt$stu_birth_weight < 3000, 1, 2)
table(dt$bwgrp)
tab1(dt$bwgrp)



avg<- aggregate(behAvg ~ bwgrp, data = dt, FUN = mean, na.rm = TRUE)
avg





dt[, 18:30] <- lapply(dt[, 18:30], function(x) as.numeric(as.character(x)))
dt$behSum <- rowSums(dt[, 18:30], na.rm = TRUE)
summary(dt$behSum)
tab1(dt$behSum)


dt$behgrp<- ifelse(dt$behSum <3,0,1)
behgrp <- table(dt$behgrp)
tab1(dt$behgrp)


dt$stu_sex <- factor(dt$stu_sex)
levels(dt$stu_sex) <- c("1","2")
labels(dt$stu_sex) <- c("Male","Female")


dt$stu_sex <- factor(dt$stu_sex, levels = c("1", "2"), labels = c("Male", "Female"))
sex_behgrp <- table(dt$stu_sex, dt$behgrp)
tab1(sex_behgrp)


dt$stu_stature <- as.numeric(dt$stu_stature)/100
dt$BMI <- as.numeric(dt$stu_weight)/(dt$stu_stature^2)
dt$bmigrp <- cut(dt$BMI, breaks=c(-Inf,18.5,24.9,Inf), labels=c("ผอม","สุขภาพดี","น้ำหนักเกิน"))
bmi_bmigrp <- table(dt$bmigrp)
tab1(bmi_bmigrp)



dt$stu_sex <- factor(dt$stu_sex, levels = c("1", "2"), labels = c("Male", "Female"))
sex_bmigrp <- table(dt$stu_sex, dt$bmigrp)
tab1(sex_bmigrp)


dt$behgrp <- factor(dt$behgrp, levels =c(0,1), labels = c("<3", "3>="))
bmigrp_behgrp <- table(dt$bmigrp, dt$behgrp)
tab1(bmigrp_behgrp)










dt <- dt[,c(30,1:29)]
str(dt)
















