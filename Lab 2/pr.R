x <- c(3, 12, 5, 89, 90, 147, 123, 4, 35, 44, 30, 25, 93, 78, 65, 63, 43, 98, 87)
d<- length(x)
d
if(d<100){
	print("Number of d is less than 100")
}else{
	print("Number of d is more than 100")
} 

if(d<25){
	print("Number of x is less than 25")
}
if(d<10){
	print("Number of x is less than 10")
}
d

if(d<22){
	print("Number of x is less than 22")
}
d <- rep(c(seq(1,76,2), seq(1,96,3)),2)
d<- length(d)
d

if(d<50){
	print("Number of d is less than 50")
}else{
	print("Number of d is more than 50")
}

if(d<50){
	print("Number of d is less than 50")
}else if(d<100){
	print("Number of d is less than 100")
}
if(d<50){
	print("Number of d is less than 50")
}else if(d<100){
	print("Number of d is less than 100")
}else if(d<150){
	print("Number of d is less than 150")
}else{
	print("Number of d is more than 150")
}


k <- c(145, 63.2, 58, 132.65, 87, 154, 132.50)
k
ifelse(k<100,1,0)

ifelse(k<100,1,0)->m
m

mean(k)
avg.k<- mean(k)
avg.k

ifelse(k<100,k-avg.k,k+avg.k)

sum <- 0
for(i in c(1:100)){
	sum <- sum+i
}
sum

num<- c(92, 33, 14, 38, 71, 32, 2, 35, 86, 82, 91, 23, 22, 39, 38, 57, 42, 9, 90, 96, 53, 91, 17, 81, 6, 49, 43)	
h<- length(num)
h

sum<- 0
for(i in c(1:h)){
	if(num[i]%%2==0){
		sum <- sum+1
	}
}
sum
sum<- 0
for(i in c(1:h)){
	if(num[i]%%2==1){
		sum <- sum+1
	}
}

myFunction <- function(num){
	if(num%%2==0){
		print("Even Number")
	}else{
		print("Odd Number")
	}
}
myFunction(10)

df <- data.frame(y=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 5, 6, 7, 8),
                 x1=c(1, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9),
                 x2=c(3, 3, 6, 6, 8, 9, 9, 8, 8, 7, 4, 3, 3, 2, 2, 1, 1))
df


setwd("C:\\Users\\user\\Downloads")
dir()

library(readxl)

dt<- read_excel("CBR64-3Prv_Practice (1).xlsx")
dt<- data.frame(dt)
str(dt)

colnames(dt) <- c("stt",
"tit",
"sex",
"age",
"brtdte",
"mar",
"prv",
"edu",
"occ",
"sal",
"q53",
"q54",
"q57",
"q58",
"q61",
"q62",
"q63",
"q64",
"q65",
"q66",
"q67",
"q68"
)
str(dt)



hist(dt$sal)

df <- data.frame(y=c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 5, 6, 7, 8),
                 x1=c(1, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9),
                 x2=c(3, 3, 6, 6, 8, 9, 9, 8, 8, 7, 4, 3, 3, 2, 2, 1, 1))


hist(df$x2)
hist(df$x1)
hist(df$y)

dt$sal_1<- sqrt(dt$sal)
hist(dt$sal_1)

dt$sal_2<- log10(dt$sal+0.001)
hist(dt$sal_2)

dt$sal_3<- 1/(dt$sal)
hist(dt$sal_3)

log10
log10(0)

dt$q5ro_total <- dt$q57*dt$q58
str(dt)

dt$qinc_mtotal <- rowSums(dt[,c("sal","q53","q61", "q62", "q63",
"q64", "q65", "q66", "q68")],na.rm=T)
str(dt)

dt$qinc_ytotal<- dt$qinc_mtotal*12
str(dt)


dt$inctotal<- rowSums(dt[,c("q5ro_total","qinc_ytotal","q54","q67" )],na.rm=T)
str(dt)

dt$inctotal <- sqrt(dt$inctotal)
hist(dt$sal)

dt$inctotal_1 <- log10(dt$inctotal+0.001)
hist(dt$sal_1)

dt$inctotal_2 <- l/(dt$inctotal)
hist(dt$sal_2)

dt$incmon<- dt$inctotal/12 #income Salary
str(dt)

dt$incmon_grp <- dt$incmon 
dt$incmon_grp<- ifelse(dt$incmon<2686,1,
		    ifelse(dt$incmon<3173,2,
		    ifelse(dt$incmon<5346,3,4)))

library(epiDisplay)
tab1(dt$incmon_grp)

table(dt$incmon_grp)


< 2,686 --> ต่ำกว่าเส้นยากจน
	2. < 3,173 --> ต่ำกว่า 40% ล่าง
	3. < 5,346 --> ต่ำกว่าเส้นมัธยฐาน
	4. >= 5,346 --> สูงกว่ามัธยฐาน


tabpct(dt$prv,dt$incmon_grp)   #epiDisplay
table(dt$prv,dt$incmon_grp)  #Rbase











