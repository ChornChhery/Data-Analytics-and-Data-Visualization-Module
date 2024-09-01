a <- c(2,3,4,5,6,7)
b <- c(2,4,5,6,8,1)
plot(a,pch=23)


plot(a, pch=12, col="green")

barplot(1:10, col=rainbow(5))

plot(a,pch=16, col="blue", type="o", lty=1)

plot(b, pch=15, col="green", type="o", lty=4, font=9)


plot(a, pch=13, col="blue", type="o", lty=6, font=7,
	xlim=c(2,5), ylim=c(0,8), main="Effect of Number",
	, cex=2, cex.main=0.9)


c <- c(0,1,2,3,4,5,6)
d <- c(7,8,9,10,11,12,13)
plot(d, pch=15, col="blue", type="s", lty=3,
	font=10, xlim= c(0,5), ylim=c(0,9),	
	xlab="Days", ylab="Weight gain (kg)")

points(c,col="orange", type="o", lty=10, font=8)

lines(d,col="blue", type="o", lty=50, font=10)

legend(0,9,lty=c(2,5), c("Treatment A", "Treatment B"),
	 col=c("blue","green"))



legend("topleft", lty=c(4,2), c("Treatment A", "Treatment B"),
	 col=c("yellow","red"))


setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\week 10")
b <- read.csv("dexabia.csv")
str(b)


b$agenew <- round(b$age,0)
table(b$agenew)

summary(b)

qqnorm(b$age)
qqline(b$age,col="blue",lwd=2)

hist(b$age)
hist(b$age,breaks=20,col=rainbow(5),main="Age")
mtext(side=1,"Age",padj=3)
mtext(side=3,"Frequency",adj=0.3,padj=4)

hist(b$age)
hist(b$age,breaks=20,col="yellow",main="")
mtext(side=1,"Age",padj=4,las=1)


boxplot(b$age)
boxplot(b$age,col=rainbow(5),las=1, horizontal=T)
mtext(side=3,"Weight (grams)", adj=-0.1,padj=-1.5,las=1)
mtext(side=3,"Age (Years)", adj=-0.1,padj=-1.5)

stem(b$age)


stripchart (b$age)
stripchart (b$age,method="stack")
stripchart (b$age,method="stack",pch=2)
stripchart (b$age,method="stack",pch=1,col="red",frame.plot=F)
stripchart (b$age,method="stack",pch=1,col="blue",frame.plot=F,at=0.05)


boxplot(b$wt~b$sex)
boxplot(b$wt~b$sex,las=1,col=c("blue","red"))
boxplot(b$wt~b$sex,las=1,col=c("blue","pink"),las=1,xlab="Number", ylab="Weight")


b$sex <- as.factor(b$sex)
levels(b$sex) <- c("male","female")
boxplot(b$wt~b$sex,las=1,col=c("blue","pink"),las=1,xlab="",ylab="")
mtext(side=3,"Weight (Kg)", adj=-0.1,padj=-1.5)



b$agegrp<- cut(b$age,breaks=c(0,39,59,100))
b$agegrp<- factor(b$agegr)
levels(b$agegr) <- c("<40","40-59","60+")
b$sex<- as.factor(b$sex)
levels(b$sex)<- c("male","female")
mtext(side=3,"Weight (Kg)", adj=-0.1,padj=-1.5)
mtext(side=1,"Gender",padj=3)
boxplot(b$wt~b$sex,las=1,col=c("blue","pink"),las=1,xlab="",ylab="")
mtext(side=3,"Weight (Kg)", adj=-0.1,padj=-1.5)
mtext(side=1,"Gender",padj=3)
title(main="Comparing Weight by Gender")



plot(b$ht,b$wt,xlab="Height",ylab="Weight",las=1,col="blue")
title(main="Comparing Weight and Height")

plot(b$sex)
sexfreq <- table(b$sex)
barplot(sexfreq)
par(mfrow=c(1,1), oma=c(3,4,3,2),mar=c(3,4,3,2),
mgp=c(0.5,0.5,0.5),tcl=-0.1,las=1)
barp <- barplot(table(b$sex), xlim=c(0,150),
las=1, cex.names=1.0, horiz=F, col="pink", width=0.8)
text(val$Freq[c(1:nrow(val))]+5,barp[c(1:nrow(val)),1],val$Freq[c(1:nrow(val))])
par(mfrow=c(1,1), oma=c(3,4,3,2),mar=c(3,4,3,2),
mgp=c(0.5,0.5,0.5),tcl=-0.1,las=1)
barp <- barplot(table(b$sex),las=1, cex.names=1.0, col="pink", width=0.8)
barp <- barplot(table(b$sex),las=1, cex.names=1.0, col="pink", width=0.8)

cor(b$ht,b$wt)


plot(b$sex)
sexfreq <- table(b$sex)
barplot(sexfreq)
barp <- barplot(table(b$sex),las=1,cex.names=1.0,
	  col= rainbow(4), width=0.9, ylim=c(0,250))

text(x=barp, y=sexfreq, label=sexfreq, pos=3, cex=3, col="blue")


sexfreq <- table(b$sex)
pie(sexfreq)
pct <- as.data.frame(proportions(sexfreq)*100)
pie(sexfreq,labels=paste(pct$Var1,round(pct$Freq,2),
    "%"),col=heat.colors(3),cex=1)

pie(sexfreq,labels=paste(pct$Var1,round(pct$Freq,2),
    "%"),col=c("yellow","green"),cex=1)

b$dexagr <- ifelse(b$dexa<40,"normal","fat")
ctab<- table(b$dexagr,b$sex)
barplot(ctab,beside=T, xlab="Gender",ylab="Number",
	  main="Gender Value", col=c("blue","orange"))
legend("topleft",inset=c(0.03,0.04),
	 c("Fat","Normal"),fill=c("blue","orange"), adj=c(0.02,0.1))

mtext(side=3,adj=0.06,"Percents")





b$bmi <- b$wt/(b$ht/100)^2
summary(b$bmi)
b$bmigr <- ifelse(b$bmi<25,"Normal","Overweight")

t3 <- table(b$bmigr,b$agegr,b$dexagr)
t3t1 <- prop.table(t3[,,1],2)*100
t3t1
t3t2 <- prop.table(t3[,,2],2)*100
t3t2

barplot(t3t1,beside=T, legend=c("Normal","Overweight"),xlab="Fat")
barplot(t3t2,beside=T, legend=c("Normal","Overweight"),xlab="Normal")


dt <- read.csv("StudentsAdaptability.csv")
str(dt)



plot(dt$Gender)
sex <- table(dt$Gender)
barplot(sex)
barp <- barplot(table(dt$Gender),las=1,cex.names=1.0,
	  col=c("blue","yellow"), width=0.8,ylim=c(0,800),
	  main="Gender Type")
text(x=barp,y=sex,label=sex,pos=3,cex=1) 


plot(dt$Age)
agegrp <- table(dt$Age)
barplot(agegrp)
barp <-barplot(table(dt$Age),main="Age range of student",
	col=c("red","blue","green","yellow","skyblue","purple"),ylim=c(0,400))
text(x=barp, y=agegrp,label=agegrp,pos=3,cex=1)



plot(dt$Education.Level)
edugrp <- table(dt$Education.Level)
barplot(edugrp)
barp <- barplot(table(dt$Education.Level),main="Education institution level",
	  col=c("pink","purple","yellow"),ylim=c(0,600))
text(x=barp, y=edugrp,label=edugrp,pos=3,cex=1)


plot(dt$Adaptivity.Level)
adagrp <- table(dt$Adaptivity.Level)
barplot(adagrp)
barp <- barplot(table(dt$Adaptivity.Level),main="Adaptability level",
	  col=c("green","blue","red"),ylim=c(0,700))
text(x=barp, y=adagrp,label=adagrp,pos=3,cex=1)



high_adaptability <- sum(dt$Adaptivity.Level == "High")
total_samples <- nrow(dt)
proportion_high_adaptability <- (high_adaptability/total_samples)*100
proportion_high_adaptability



ada_fin<-table(dt$Adaptivity.Level,dt$Financial.Condition)
barp <- barplot(ada_fin, beside=TRUE, col=c("green", "blue", "red"),
                legend = rownames(ada_fin), ylim=c(0,600),
                main="Adaptability Level by Financial Condition", xlab="Financial Condition", ylab="Count")
text(x=barp, y=ada_fin, label=ada_fin, pos=3, cex=1)



adap_internet<-table(dt$Adaptivity.Level,dt$Internet.Type)
barp <- barplot(adap_internet, beside=TRUE, col=c("green", "blue", "yellow"),
                legend = rownames(adap_internet), ylim=c(0,500),
                main="Adaptability Level by Internet Type", xlab="Internet Type", ylab="Count")
text(x=barp, y=adap_internet, label=adap_internet, pos=3, cex=1)





