
setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Week5")

iq <- read.csv("iqsize.csv", header=T,sep=",")
library(epiDisplay)
str(iq) # structure data
names(iq) <- tolower(names(iq))
des(iq)
summ(iq) # for continuous variables
# data table
#View(iq)
# 1. การแจกแจงของตัวแปรตาม outcome distribution
summ(iq$piq)
hist(iq$piq, col="green",main="Histogram of iq size")


# 2. การแจกแจงของตัวแปรอิสระ determinants distribution and management
#continuous variables based on descriptive results
summ(iq$brain)
hist(iq$brain, col="pink")
shapiro.test(iq$brain)
summ(iq$height)
hist(iq$height, col="pink")
shapiro.test(iq$height)
summ(iq$weight)
hist(iq$weight, col="pink")
shapiro.test(iq$weight)
quantile(iq$weight) # การจัดกลุ่มใหม่ของตัวแปร น้้าหนัก
iq$weight1 <- cut(iq$weight, c(105.00,140.00,160.00,192.00))
tab1(iq$weight1, col=c("green","pink","yellow"))


# 2.1 การแปลงตัวแปร อิสระที่เป็น integer ให้เป็น factor Categorical variable based on 
descriptive
tab1(iq$gender)
iq$gender1 <- factor(iq$gender)
levels(iq$gender1) <- c("male","female")
tab1(iq$gender1, col=c("blue","pink"))
# 3. การหาความสัมพันธ์รายคู่ ระหว่างตัวแปรอิสระแต่ละตัวกับตัวแปรตาม preliminary analysis
# relationship between outcome and determinants by t.test and simple linear 
regression and graph
# campare mean, 2 groups
summ(iq$piq,by=iq$gender1)
t.test(iq$piq~iq$gender1, var.equal=T)
boxplot(iq$piq~iq$gender1, col=c("yellow","red"),main="iq size between gender")
summ(iq$piq,by=iq$weight1)
oneway.test(iq$piq~iq$weight1, var.equal=T)
boxplot(iq$piq~iq$weight1, col=c("yellow","red"),main="iq size between weight 
group")

# simple linear regression, full model
cor(iq$piq,iq$brain)
plot(iq$piq,iq$brain)
mod0 <- lm(piq~brain, data=iq)
summary(mod0)
cor(iq$piq,iq$height)
plot(iq$piq,iq$height)
12
mod1 <- lm(piq~height, data=iq)
summary(mod1)


# 4. การสร้างตัวแบบการถดถอยเชิงเส้นพหุคูณ multiple linear regression, full model
mod <- lm(piq~brain+height+weight1+gender1, data=iq)
summary(mod)
# 5. การคัดเลือกตัวแบบที่เหมาะสมที่สุด
step(mod,direction = "backward")
# 6. ตัวแบบที่เหมาะสมที่สุด best model
mod4 <- lm(piq~brain+height, data=iq)
summary(mod4)
step(mod4,direction = "backward")
anova(mod4) # assess whether at least one predictor variable has a non-zero 
coefficient
#Assess model
plot(mod4)





























# 3. การหาความสัมพันธ์รายคู่ ระหว่างตัวแปรอิสระแต่ละตัวกับตัวแปรตาม preliminary analysis
# relationship between outcome and determinants by t.test and simple linear 
regression and graph
# campare mean, 2 groups
summ(iq$piq,by=iq$gender1)
t.test(iq$piq~iq$gender1, var.equal=T)
boxplot(iq$piq~iq$gender1, col=c("yellow","red"),main="iq size between gender")

summ(iq$piq,by=iq$weight1)
oneway.test(iq$piq~iq$weight1, var.equal=T)
boxplot(iq$piq~iq$weight1, col=c("yellow","red"),main="iq size between weight 
group")
# simple linear regression, full model
cor(iq$piq,iq$brain)
plot(iq$piq,iq$brain)
mod0 <- lm(piq~brain, data=iq)
summary(mod0)
cor(iq$piq,iq$height)
plot(iq$piq,iq$height)
12
mod1 <- lm(piq~height, data=iq)
summary(mod1)






# 6. ตัวแบบที่เหมาะสมที่สุด best model
mod4 <- lm(piq~brain+height, data=iq)
summary(mod4)
step(mod4,direction = "backward")

# 4. การสร้างตัวแบบการถดถอยเชิงเส้นพหุคูณ multiple linear regression, full model
mod <- lm(piq~brain+height+weight1+gender1, data=iq)
summary(mod)


summ(iq$piq)
hist(iq$piq, col="green",main="Histogram of iq size")
shapiro.test(iq$piq)

summ(iq$brain)
hist(iq$brain, col="pink")
shapiro.test(iq$brain)

summ(iq$brain)
hist(iq$brain, col="pink")
shapiro.test(iq$brain)
summ(iq$height)
hist(iq$height, col="pink")
shapiro.test(iq$height)


summ(iq$weight)
hist(iq$weight, col="pink")
shapiro.test(iq$weight)


quantile(iq$weight) # การจัดกลุ่มใหม่ของตัวแปร น้้าหนัก
iq$weight1 <- cut(iq$weight, c(105.00,140.00,160.00,192.00))
tab1(iq$weight1, col=c("green","pink","yellow"))

# campare mean, 2 groups
summ(iq$piq,by=iq$gender1)
t.test(iq$piq~iq$gender1, var.equal=T)
boxplot(iq$piq~iq$gender1, col=c("yellow","red"),main="iq size between gender")


summ(iq$piq,by=iq$weight1)
oneway.test(iq$piq~iq$weight1, var.equal=T)
boxplot(iq$piq~iq$weight1, col=c("yellow","red"),main="iq size between weight 
group")



cor(iq$piq,iq$brain)
plot(iq$piq,iq$brain)
mod0 <- lm(piq~brain, data=iq)
summary(mod0)
cor(iq$piq,iq$height)
plot(iq$piq,iq$height)
12
mod1 <- lm(piq~height, data=iq)
summary(mod1)

# Calculate means and standard deviations
mean_piq <- mean(iq$piq)
sd_piq <- sd(iq$piq)
mean_brain <- mean(iq$brain)
sd_brain <- sd(iq$brain)

# Print results
cat("IQ Mean ± SD: ", mean_piq - sd_piq, " to ", mean_piq + sd_piq, "\n")
cat("Brain Size Mean ± SD: ", mean_brain - sd_brain, " to ", mean_brain + sd_brain, "\n")




