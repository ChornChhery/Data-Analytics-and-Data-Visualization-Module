Sys.setlocale("LC_ALL", locale="Thai")
install.packages("readxl")
library(readxl)
install.packages("epiDisplay")
library(epiDisplay)
install.packages("reshape2")
library(reshape2)

setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Lab 4") 
dt <- read_excel("unemployment.xlsx")
dt <- as.data.frame(dt)
str(dt)



# Reshape
d <- melt(dt, id=c("State","Area_name"))

colnames(d) <- c("st","area","yr","unemp")
d$yr <- sub("y","",d$yr)
d <- d[order(d$st,d$area,d$yr),]
View(d)

hist(d$unemp)
str(d)

d$unemp_1 <- log10(d$unemp)
hist(d$unemp_1)

tapply(d$unemp , d$st, mean)

zz <- tapply(d$unemp , d$st, mean)
zz <- as.data.frame(zz)
zz
zz$st <- row.names(zz)
mn <- min(zz[,1])
mn
zz[zz[,1]==mn,]

mn <- max(zz[,1])
mn
zz[zz[,1]==mn,]

summ(d$unemp, by=d$st)

write.csv(d, file="unemploy2017.csv", row.names=F)


setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Lab 4") 
dt <- read_excel("child140_practice.xlsx")
dt <- as.data.frame(dt)
str(dt)

dt$stubrtdte <- as.Date(dt$stu_brtdte, "%Y%m%d")
today <- as.Date("25670707","%Y%m%d")
dt$stuage <- as.numeric(today - dt$stubrtdte)/365.25
str(dt)

summary(dt$stuage)

dt$fabrtdte <- as.Date(dt$brtdte, "%Y%m%d")
dt$faage <- as.numeric(today - dt$fabrtdte)/365.25
summary(dt$faage)

dt$mombrtdte <- as.Date(dt$brtdte2, "%Y%m%d")
dt$momage <- as.numeric(today - dt$mombrtdte)/365.25
summary(dt$momage)

dt[which(dt$momage<0),]
dt$momage <- ifelse (dt$momage <0 ,NA,dt$momage)
summary(dt$momage)


dt$faagegrp <- ifelse(is.na(dt$faage),4,
				ifelse(dt$faage<=30,1,
				ifelse(dt$faage<=40,2,3)))
tab1(dt$faagegrp)

dt$faagegrp <- factor(dt$faagegrp)
levels(dt$faagegrp) <- c("1:<30","2:31-40","3:>40","4:Underfined")
tab1(dt$faagegrp)



dt$momagegrp <- factor(dt$momagegrp)
levels(dt$momagegrp) <- c("1:<30","2:31-40","3:>40","4:Underfined")
tab1(dt$momagegrp)

dt$momagegrp <- dt$momage
dt$momagegrp <-ifelse(dt$momage<=30,1,
			ifelse(dt$momage<=40,2,3))
tab1(dt$momagegrp)

# 2variable categorical vs continous======
tapply(dt$sal, dt$faagegrp, mean, na.rm=T)
summ(dt$sal, by=dt$faagegrp)

# 2variable categorical vs continous======
table(dt$occnme, dt$faagegrp)
tabpct(dt$occnme, dt$faagegrp, percent="row")

tapply(dt$sal, dt$momagegrp, mean, na.rm=T)
summ(dt$sal, by=dt$momagegrp)

mom_sal <- tapply(dt$sal, dt$momagegrp, mean, na.rm = TRUE)
mom_sal
summ(dt$sal, by=dt$mom_sal)

str(dt)

table(dt$edulevnme1 , dt$momagegrp)
tabpct(dt$edulevnme1 , dt$momagegrp, percent="row")

#=============================================
nr_1 <- read_excel("CBR64-Nara1_practice.xlsx")
nr_1 <- as.data.frame(nr_1)
str(nr_1)

nr_2 <- read_excel("CBR64-Nara2_practice.xlsx")
nr_2 <- as.data.frame(nr_2)
str(nr_2)

nr_3 <- rbind(nr_1,nr_2)

des(nr_1)
des(nr_2)

nr_11 <- nr_1[,c("tit","sex","stt",
"brtdte","tmb","prv","edu","occ","mem",
"inc53m","inc55y")]

nr_22 <- nr_2[,c("tit","sex","stt",
"brtdte","tmb","prv","edu","occ","mem",
"inc53m","inc55y")]

des(nr_11)
des(nr_22)

nr_3 <- rbind(nr_11,nr_22)
des(nr_3)
str(nr_3)
nr_3$bd<- as.Date(nr_3$brtdte,"%d%m%Y")
today <- as.Date("16072567","%d%m%Y")
nr_3$age <- as.numeric(today-nr_3$bd)/365.25
summ(nr_3$age)

nr_3[which(nr_3$age>100),]
nr_3$age <- ifelse (nr_3$age>100,NA,nr_3$age)
summary(nr_3$age)

nr_3$inc55m <- nr_3$inc55y/12
nr_3$inc <- rowSums(nr_3[,c("inc53m","inc55m")], na.rm=T)
summ(nr_3$inc)

str(nr_3)


nr_3[which(nr_3$mem>20),]
nr_3$mem <- ifelse(nr_3$mem>20,NA,nr_3$mem)
summary(nr_3$mem)
tab1(nr_3$mem)





setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Lab 4")
dt <- read.csv("thai_road_accident_practice.csv")
str(dt)



 dt$incy <- format(as.Date(dt$incident_datetime,"%d/%m/%Y"),"%Y")
 dt$incy <- as.numeric(dt$incy)
 summary(dt$incy)
 table(dt$incy)

 dt$incm <- format(as.Date(dt$incident_datetime,"%d/%m/%Y"),"%m")
 dt$incm <- as.numeric(dt$incm)
 summary(dt$incm)
 table(dt$incm)


 dt$inctime <- as.POSIXct(dt$incident_datetime, format = "%d/%m/%Y %H:%M")
 dt$inctime <- format(as.POSIXct(dt$inctime),format = "%H:%M")
 dt$inctime <- gsub("[:]", ".",dt$inctime)
 dt$inctime <- as.numeric(dt$inctime)
 summary(dt$inctime)





 dt$incdate <- as.Date(dt$incident_datetime,"%d/%m/%Y")
 dt$rptdate <- as.Date(dt$report_datetime,"%d/%m/%Y")
 dt$peri <- dt$rptdate- dt$incdate
 dt$days_between <- as.numeric(dt$rptdate - dt$incdate)
 summary(dt$days_between)
 tab1(dt$days_between)



dt$incident_date <- as.Date(dt$incident_datetime, "%d/%m/%Y")
dt$y <- format(dt$incident_date, "%Y")
dt$m <- format(dt$incident_date, "%m")
event <- table(dt$year, dt$month)
max_months <- apply(event, 1, function(x) {
  names(x)[which.max(x)]
})
re_table <- data.frame(year = names(max_months), month = max_months)
re_table





dt$incident_date <- as.Date(dt$incident_datetime, "%d/%m/%Y %H:%M")
dt$report_date <- as.Date(dt$report_datetime, "%d/%m/%Y %H:%M")
dt$days_between <- as.numeric(dt$report_date - dt$incident_date)
un_info <- dt[, c("year", "province_en", "vehicle_type", 
                       "number_of_fatalities", "number_of_injuries", "days_between")]
un_case <- un_info[un_info$number_of_fatalities >= 5 | un_info$number_of_injuries >= 10, ]
head(un_case,5)





avg_stat <-aggregate(cbind(number_of_fatalities,number_of_injuries)~year,data=dt,FUN=mean)
avg_stat


avg_stat <-aggregate(cbind(number_of_fatalities,number_of_injuries)~year+month,data=dt,FUN=mean)
avg_stat




dt <- data.frame(
  year = c(2019, 2019, 2020, 2020, 2020, 2021, 2021),
  vehicle_type = c("motorcycle", "car", "car", "motorcycle", "car", "motorcycle", "motorcycle")
)
accidents_by_type <- table(dt$year, dt$vehicle_type)
most_frequent_types <- apply(accidents_by_type,1,function(x){
  names(x)[which.max(x)]
})
re_tab<- data.frame(year=names(most_frequent_types),most_frequent_vehicle_type=most_frequent_types)
re_tab









