setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Case Study")

install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate) 

# Read the dataset
df <- read_excel("Netflix Userbase.xlsx")
df <- data.frame(df)
str(df)


# สรุปข้อมูลเบื้องต้น (เช่น การหาค่าเฉลี่ย, ค่าสูงสุด/ต่ำสุด)
summary(df)

# # ตรวจสอบค่า Missing Values
missing<-colSums(is.na(df))
missing

# แปลงคอลัมน์ 'Join Date' และ 'Last Payment Date' ให้อยู่ในรูปแบบวันที่
df$`Join.Date` <- as.Date(df$`Join.Date`, format="%Y-%m-%d")
df$`Last.Payment.Date` <- as.Date(df$`Last.Payment.Date`, format="%Y-%m-%d")
head(df$`Join.Date`)
head(df$`Last.Payment.Date`)

summary(df)
# ตรวจสอบชื่อตัวแปร (คอลัมน์)
colnames(df)


# 4.1: Gender distribution by Device
ggplot(df, aes(x = Device, fill = Gender)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender Distribution by Device") +
  xlab("Device") +
  ylab("Total") +
  theme_minimal()


# Subscription Type Distribution in R
ggplot(df, aes(x = `Subscription.Type`, fill = `Subscription.Type`)) +
  geom_bar() +
  ggtitle("Subscription Type Distribution") +
  xlab("Subscription.Type") +
  ylab("Total") +
  theme_minimal()


# 4.4: Revenue Analysis by Subscription Type
ggplot(df, aes(x = `Subscription Type`, y = `Monthly Revenue`, fill = `Subscription Type`)) +
  geom_boxplot() +
  ggtitle("Monthly Revenue by Subscription Type") +
  xlab("Subscription Type") +
  ylab("Monthly Revenue") +
  theme_minimal()


# 4.3: Country-wise User Distribution
ggplot(df, aes(x = Country, fill = Country)) +
  geom_bar() +
  coord_flip() + 
  ggtitle("User Distribution by Country") +
  xlab("Total") +
  ylab("Country") +
  theme_minimal()


# 4.4: Revenue Analysis by Subscription Type
ggplot(df, aes(x = `Subscription.Type`, y = `Monthly.Revenue`, fill = `Subscription.Type`)) +
  geom_boxplot() +
  ggtitle("Monthly Revenue by Subscription Type") +
  xlab("Subscription Type") +
  ylab("Monthly Revenue") +
  theme_minimal()


# 4.5: Revenue Analysis by Country
ggplot(df, aes(x = Country, y = `Monthly.Revenue`, fill = Country)) +
  geom_boxplot() +
  coord_flip() + 
  ggtitle("Monthly Revenue by Country") +
  xlab("Country") +
  ylab("Monthly Revenue") +
  theme_minimal()











