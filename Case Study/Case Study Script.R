setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Case Study 3")

install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("pROC")
library(pROC)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate) 
install.packages("knitr")
library(knitr)
install.packages("epiDisplay")
library(epiDisplay)
install.packages("car")
library(car)

dt <- read.csv("COVID-19 Survey Student Responses.csv", stringsAsFactors = FALSE)
dt <- data.frame(dt)
str(dt)

summary(dt)

# ตรวจสอบค่าขาดหาย (Missing values)
missing <- colSums(is.na(dt))
missing

dt <- dt[!duplicated(dt), ]
head(dt)

# Handle missing values appropriately for each column
# E.g., Replace with mean for numerical columns, or mode for categorical columns
dt$Time.spent.on.social.media[is.na(dt$Time.spent.on.social.media)] <- mean(dt$Time.spent.on.social.media, na.rm = TRUE)
dt$Time.spent.on.Online.Class[is.na(dt$Time.spent.on.Online.Class)] <- mean(dt$Time.spent.on.Online.Class, na.rm = TRUE)# ตรวจสอบอีกครั้งว่าไม่มีค่าที่หายไปแล้ว
sum(is.na(dt$Rating.of.Online.Class.experience))

# Convert categorical variables to factors
dt$Region.of.residence <- as.factor(dt$Region.of.residence)
dt$Health.issue.during.lockdown <- as.factor(dt$Health.issue.during.lockdown)

# Ensure numerical columns are treated correctly
dt$Age.of.Subject <- as.numeric(dt$Age.of.Subject)

# Optionally, remove outliers based on a threshold (e.g., 1.5 times the interquartile range)
Q1 <- quantile(dt$Time.spent.on.social.media, 0.25)
Q3 <- quantile(dt$Time.spent.on.social.media, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter rows that have outliersr
dt <- dt[dt$Time.spent.on.social.media >= lower_bound & dt$Time.spent.on.social.media <= upper_bound, ]
# Ensure the response variable is binary for logistic regression
dt$Health.issue.during.lockdown <- ifelse(dt$Health.issue.during.lockdown == "YES", 1, 0)
dt$Health.issue.during.lockdown <- as.factor(dt$Health.issue.during.lockdown)
# Create a binned version of Time Spent on Social Media
dt$SocialMediaTime_binned <- cut(dt$Time.spent.on.social.media, breaks = c(0, 1, 2, 3, 4, 5, Inf),
                                 labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5+"))

dt$Rating.of.Online.Class.experience[is.na(dt$Rating.of.Online.Class.experience)] <- "Average" # mode imputation
dt$Medium.for.online.class[is.na(dt$Medium.for.online.class)] <- "Smartphone" # Example imputation

# Ensure that the structure of the data is correct
str(dt)

nrow(dt) 

# Define the independent variables (x) and dependent variable (y)
independent_vars <- c("Region.of.residence", "Age.of.Subject", "Time.spent.on.Online.Class", 
                      "Time.spent.on.social.media", "Number.of.meals.per.day")
dependent_var <- "Health.issue.during.lockdown"

# Combine x and y variables into a single list
selected_vars <- c(independent_vars, dependent_var)

# Subset the dataset based on the selected variables
subset_data <- dt[selected_vars]

# Find the number of rows (data points) and columns (variables)
num_rows <- nrow(subset_data)
num_vars <- ncol(subset_data)

# Output the result
cat("1.1 จากผลการวิเคราะห์ จำนวนข้อมูลทั้งหมด", num_rows, "ค่า จำนวนตัวแปร", num_vars, "ตัว\n")


#สถิติเชิงสรุปของตัวแปรเชิงตัวเลข
table(dt$Region.of.residence)  # การกระจายตัวของผู้ตอบในแต่ละภูมิภาค
summary(dt$Time.spent.on.Online.Class)  # สรุปการกระจายของเวลาที่ใช้ในคลาสออนไลน์
summary(dt$Time.spent.on.social.media) # สรุปการกระจายของเวลาที่ใช้บนโซเชียลมีเดีย 

# Visualize outliers
# Graph 1
# Calculate percentages
region_counts <- dt %>%
  group_by(Region.of.residence) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Create the bar plot with counts and percentages
ggplot(dt, aes(x = Region.of.residence, fill = Region.of.residence)) +
  geom_bar(color = "black") +  # Border color for bars
  scale_fill_manual(values = c("lightblue", "yellow")) +  # Two color scheme
  labs(title = "Distribution of Region of Residence",
       x = "Region of Residence",
       y = "Count") +
  theme_minimal(base_size = 14) +
  geom_text(data = region_counts, aes(x = Region.of.residence, y = Count, 
                                      label = paste0(Count, " (", round(Percentage, 1), "%)")),
            vjust = -0.5, color = "black", size = 4)


#Graph 2
# Example Histogram with Count Labels on Top of Each Bar
ggplot(dt, aes(x = Time.spent.on.social.media)) +
  geom_histogram(binwidth = 1, aes(fill = ..count..), color = "black") +
  geom_text(stat='bin', aes(label=..count..), vjust=-0.5, binwidth=1) +  # Adds count values
  scale_fill_gradientn(colors = c("red", "purple", "pink", "yellow", "orange", "blue", "green")) +
  labs(title = "Distribution of Time Spent on Social Media",
       x = "Time Spent on Social Media (hours)",
       y = "Frequency") +
  theme_minimal(base_size = 14)


# Graph 3
# Boxplot: Time Spent on Online Class with statistics
ggplot(dt, aes(x = "", y = Time.spent.on.Online.Class)) +
  geom_boxplot(fill = "green", color = "black", outlier.colour = "red", outlier.shape = 16) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5) +  # Show median
  labs(title = "Boxplot of Time Spent on Online Class",
       x = "",
       y = "Time Spent on Online Class (hours)") +
  theme_minimal(base_size = 14)


#Graph 4
# Bar Chart: Number of Meals Per Day with counts
ggplot(dt, aes(x = factor(Number.of.meals.per.day), fill = factor(Number.of.meals.per.day))) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Add counts on top of bars
  scale_fill_manual(values = c("#FF6666", "#FFCC66", "#66CCFF", "#66FF66", "#FF66CC", "#9966FF", "#FFFF66", "#FF9933")) +
  labs(title = "Distribution of Number of Meals Per Day",
       x = "Number of Meals Per Day",
       y = "Count") +
  theme_minimal(base_size = 14)

# Graph 5
# Histogram: Age of Subject with counts
ggplot(dt, aes(x = Age.of.Subject)) +
  geom_histogram(binwidth = 1, aes(fill = ..count..), color = "black") +
  geom_text(stat = 'bin', aes(label = ..count..), vjust = -0.5, binwidth = 1) +  # Add counts on top of bars
  scale_fill_gradientn(colors = c("red", "orange", "yellow", "green", "blue", "purple", "pink")) +
  labs(title = "Distribution of Age of Subject",
       x = "Age of Subject (years)",
       y = "Frequency") +
  theme_minimal(base_size = 14)


# Graph 6
# Calculate percentages
HealthIssues_counts <- dt %>%
  group_by(Health.issue.during.lockdown) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Create the bar plot with counts and percentages
ggplot(dt, aes(x = Health.issue.during.lockdown, fill = Health.issue.during.lockdown)) +
  geom_bar(color = "black") +  # Border color for bars
  scale_fill_manual(values = c("lightcoral", "blue")) +  # Two color scheme
  labs(title = "Distribution of Health Issues During Lockdown",
       x = "Health Issues During Lockdown",
       y = "Count") +
  theme_minimal(base_size = 14) +
  geom_text(data = HealthIssues_counts, aes(x = Health.issue.during.lockdown, y = Count, 
                                      label = paste0(Count, " (", round(Percentage, 1), "%)")),
            vjust = -0.5, color = "black", size = 4)


#Graph 7
# Heatmap for Health Issues by Region
dt_summary <- dt %>% 
  group_by(Region.of.residence, Health.issue.during.lockdown) %>% 
  summarise(Count = n())

ggplot(dt_summary, aes(x = Region.of.residence, y = Health.issue.during.lockdown, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Health Issues by Region",
       x = "Region of Residence", 
       y = "Health Issue During Lockdown",
       fill = "Count") +
  theme_minimal()

#T-test graph 1
m <- table(dt$Region.of.residence,dt$Health.issue.during.lockdown)
m
chisq.test(m)


# Graph 8
# Bar Plot: Health Issue During Lockdown by Age of Subject with Two Colors
ggplot(dt, aes(x = Age.of.Subject, y = Health.issue.during.lockdown, fill = factor(Health.issue.during.lockdown))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  labs(title = "Health Issue During Lockdown by Age of Subject",
       x = "Age of Subject", 
       y = "Health Issue During Lockdown",
       fill = "Health Issue During Lockdown") +
  theme_minimal()

# T-test graph 2
# Create the contingency table based on Age group and Health Issue
dt$Age.group <- cut(dt$Age.of.Subject, 
                    breaks = c(15, 18, 21, 24, 30, Inf), 
                    labels = c("15-18", "19-21", "22-24", "25-30", "30+"))
# Create the contingency table
m1 <- table(dt$Age.group, dt$Health.issue.during.lockdown)

# Perform chi-squared test
chi_test <- chisq.test(m1)
chi_test



# Graph 9
# Bar Plot: Health Issue During Lockdown by Time Spent on Online Class with Two Colors
ggplot(dt, aes(x = Time.spent.on.Online.Class, y = Health.issue.during.lockdown, fill = factor(Health.issue.during.lockdown))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "cyan")) +
  labs(title = "Health Issue During Lockdown by Time Spent on Online Class",
       x = "Time Spent on Online Class", 
       y = "Health Issue During Lockdown",
       fill = "Health Issue During Lockdown") +
  theme_minimal()

#T-test graph 3
# Contingency table
m2 <- table(dt$Time.spent.on.Online.Class, dt$Health.issue.during.lockdown)
m2
chi_test <- chisq.test(m2)
chi_test


# Graph 10
# Boxplot: Time Spent on Social Media by Health Issue During Lockdown
ggplot(dt, aes(x = Health.issue.during.lockdown, y = Time.spent.on.social.media, fill = Health.issue.during.lockdown)) +
  geom_boxplot() +
  labs(title = "Time Spent on Social Media by Health Issue During Lockdown",
       x = "Health Issue During Lockdown",
       y = "Time Spent on Social Media (hours)") +
  theme_minimal() +
  scale_fill_manual(values = c("purple", "lightgreen"))

#T-test 4
# Create the contingency table
m4 <- table(dt$Time.spent.on.social.media, dt$Health.issue.during.lockdown)
chisq.test(m4)


#Graph 11
# Bar Plot: Health Issue During Lockdown by Number of Meals per Day with Two Colors
ggplot(dt, aes(x = Number.of.meals.per.day, y = Health.issue.during.lockdown, fill = factor(Health.issue.during.lockdown))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("yellow", "orange")) +
  labs(title = "Health Issue During Lockdown by Number of Meals per Day",
       x = "Number of Meals per Day", 
       y = "Health Issue During Lockdown",
       fill = "Health Issue During Lockdown") +
  theme_minimal()

#T-test 5
# Contingency table for Graph 5
m5 <- table(dt$Number.of.meals.per.day, dt$Health.issue.during.lockdown)

# Chi-squared test for independence
chi_test_5 <- chisq.test(m5)
chi_test_5


# Calculate frequency and percentage for "Health.issue.during.lockdown"
health_issue_summary <- dt %>%
  group_by(Health.issue.during.lockdown) %>%
  summarise(Frequency = n()) %>%
  mutate(Percentage = (Frequency / sum(Frequency)) * 100)

# Display the summary table
health_issue_summary %>%
  kable(col.names = c("Health Issue During Lockdown", "Frequency", "Percentage (%)"), 
        caption = "Frequency and Percentage of Health Issues During Lockdown")

# Bivariate data analysis in R
tableStack(data=dt, c("Region.of.residence", "Age.of.Subject", "Time.spent.on.Online.Class", "Time.spent.on.social.media", "Number.of.meals.per.day"), by=Health.issue.during.lockdown)



# สร้างตัวแบบตั้งต้น logistic regression
ml <- glm(Health.issue.during.lockdown ~ Region.of.residence + Age.of.Subject + 
            Time.spent.on.Online.Class + Time.spent.on.social.media + 
            Number.of.meals.per.day, 
          data = dt, family = binomial)
# ดูผลการสรุปเบื้องต้นของตัวแบบ
summary(ml)

# การคัดเลือกตัวแปรด้วยวิธี backward selection
bw <- step(ml, direction = "backward")
# แสดงผลการวิเคราะห์จากการคัดเลือกตัวแบบ
bw$anova

# การคัดเลือกตัวแปรด้วยวิธี forward selection
dea2 <- step(ml, direction = "forward")
dea2$anova


# สร้างตัวแบบสุดท้าย (final model)
m2 <- glm(Health.issue.during.lockdown ~ Region.of.residence + Age.of.Subject + 
          Time.spent.on.Online.Class + Time.spent.on.social.media + 
          Number.of.meals.per.day, 
          data = dt, family = binomial)
# ดูสรุปผลการวิเคราะห์
summary(m2)


# คำสั่งเพื่อแปลผลเป็น Odds Ratio (OR)
exp(coef(m2))


# แสดงผลลัพธ์ OR และ CI
logistic.display(m2)


# การสร้างกราฟ ROC และคำนวณค่า AUC
ro <- lroc(m2, graph = TRUE)
attributes(ro)
# แสดงค่า AUC
ro$auc


# Create contingency table for Region of residence
m1 <- matrix(c(86, 635, 75, 386), nrow = 2, byrow = TRUE,
             dimnames = list("Region" = c("Delhi-NCR", "Outside Delhi-NCR"), 
                             "Health Issue" = c("Yes", "No")))
m1

# Calculate Odds Ratio (OR)
or_region <- (m1[1,1] / m1[1,2]) / (m1[2,1] / m1[2,2])
se_or_region <- sqrt(1/m1[1,1] + 1/m1[1,2] + 1/m1[2,1] + 1/m1[2,2])
lower_ci_region <- exp(log(or_region) - 1.96 * se_or_region)
upper_ci_region <- exp(log(or_region) + 1.96 * se_or_region)

cat("Odds Ratio (Region):", or_region, "\n")
cat("SE(OR) (Region):", se_or_region, "\n")
cat("95% CI (Region):", lower_ci_region, upper_ci_region, "\n")


# Create contingency table for Age of Subject
m2 <- matrix(c(80, 58, 107, 104, 96, 92, 46, 47), nrow = 4, byrow = TRUE,
             dimnames = list("Age" = c("19 years", "20 years", "21 years", "22 years"), 
                             "Health Issue" = c("Yes", "No")))
m2

# Calculate OR for 19 years vs 20 years
or_age <- (m2[1,1] / m2[1,2]) / (m2[2,1] / m2[2,2])
se_or_age <- sqrt(1/m2[1,1] + 1/m2[1,2] + 1/m2[2,1] + 1/m2[2,2])
lower_ci_age <- exp(log(or_age) - 1.96 * se_or_age)
upper_ci_age <- exp(log(or_age) + 1.96 * se_or_age)

cat("Odds Ratio (Age):", or_age, "\n")
cat("SE(OR) (Age):", se_or_age, "\n")
cat("95% CI (Age):", lower_ci_age, upper_ci_age, "\n")


# Create contingency table for Online Class
m3 <- matrix(c(86, 635, 53, 61, 101, 100, 90, 87, 112, 110, 65, 77), nrow = 6, byrow = TRUE,
             dimnames = list("Online Class Hours" = c("0.00 hours", "1.00 hour", "2.00 hours", "3.00 hours", "4.00 hours", "5.00 hours"), 
                             "Health Issue" = c("Yes", "No")))
m3

# Calculate OR for 0.00 hours vs 1.00 hour
or_class <- (m3[1,1] / m3[1,2]) / (m3[2,1] / m3[2,2])
se_or_class <- sqrt(1/m3[1,1] + 1/m3[1,2] + 1/m3[2,1] + 1/m3[2,2])
lower_ci_class <- exp(log(or_class) - 1.96 * se_or_class)
upper_ci_class <- exp(log(or_class) + 1.96 * se_or_class)

cat("Odds Ratio (Online Class):", or_class, "\n")
cat("SE(OR) (Online Class):", se_or_class, "\n")
cat("95% CI (Online Class):", lower_ci_class, upper_ci_class, "\n")


# Create the matrix for Social Media Hours vs Health Issue
m4 <- matrix(c(158, 185, 165, 170, 83, 86, 47, 65), nrow = 4, byrow = TRUE,
             dimnames = list("Social Media spent Hours" = c("1.00 hour", "2.00 hours", "3.00 hours", "4.00 hours"), 
                             "Health Issue" = c("Yes", "No")))

# Display the matrix
m4

or_class <- (m4[1,1] / m4[1,2]) / (m4[2,1] / m4[2,2])
se_or_class <- sqrt(1/m4[1,1] + 1/m3[1,2] + 1/m4[2,1] + 1/m4[2,2])
lower_ci_class <- exp(log(or_class) - 1.96 * se_or_class)
upper_ci_class <- exp(log(or_class) + 1.96 * se_or_class)

cat("Odds Ratio (Online Class):", or_class, "\n")
cat("SE(OR) (Online Class):", se_or_class, "\n")
cat("95% CI (Online Class):", lower_ci_class, upper_ci_class, "\n")


# Create the matrix for Number of meals vs Health Issue
m5 <- matrix(c(142, 145, 307, 303, 85, 144), nrow = 3, byrow = TRUE,
             dimnames = list("Number of Meals" = c("2 meals per day", "3 meals per day", "4 meals per day"), 
                             "Health Issue" = c("Yes", "No")))

# Display the matrix
m5

or_class <- (m5[1,1] / m5[1,2]) / (m5[2,1] / m5[2,2])
se_or_class <- sqrt(1/m5[1,1] + 1/m5[1,2] + 1/m5[2,1] + 1/m5[2,2])
lower_ci_class <- exp(log(or_class) - 1.96 * se_or_class)
upper_ci_class <- exp(log(or_class) + 1.96 * se_or_class)

cat("Odds Ratio (Online Class):", or_class, "\n")
cat("SE(OR) (Online Class):", se_or_class, "\n")
cat("95% CI (Online Class):", lower_ci_class, upper_ci_class, "\n")


