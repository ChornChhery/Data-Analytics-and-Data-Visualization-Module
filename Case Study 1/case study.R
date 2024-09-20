setwd("E:\\Prince of Songla University Pattani Campus In Thailand\\Third year\\Data Analytics and Visualizaations\\Case Study 1")

install.packages("ggplot2")
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate) 

dt <- read_excel("science-and-technology-indicators-for-india-1.xlsx")
dt <- data.frame(dt)
str(dt)

summary(dt)

missing<-colSums(is.na(dt))
missing


colnames(dt)[colnames(dt) == "Value"] <- "Indicator_Value"
dt$Indicator_Value[is.na(dt$Indicator_Value)] <- mean(dt$Indicator_Value, na.rm = TRUE)
sum(is.na(dt$Indicator_Value))

dt$Year <- as.numeric(dt$Year)
dt <- dt[!is.na(dt$Year), ]
dt$Indicator_Value <- as.numeric(dt$Indicator_Value)
head(dt$Indicator_Value)

missing_years <- sum(is.na(dt$Year))
missing_years
missing_values <- sum(is.na(dt$Indicator_Value))
missing_values 


duplicates <- dt[duplicated(dt), ]
duplicates

dt <- dt[!duplicated(dt), ]
str(dt)



# Graph 1
journal_articles <- dt %>%
  filter(`Indicator.Name` == "Scientific and technical journal articles")
# Plot the number of journal articles published each year
ggplot(journal_articles, aes(x = Year, y = Indicator_Value)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Number of Scientific and Technical Journal Articles Published Each Year", 
       x = "Year", y = "Number of Articles") +
  theme_minimal()



# Graph 2
# Filter data for R&D expenditure
rd_expenditure <- dt %>%
  filter(`Indicator.Name` == "Research and development expenditure (% of GDP)")

# Plot the trend of R&D expenditure
ggplot(rd_expenditure, aes(x = Year, y = Indicator_Value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "R&D Expenditure as a Percentage of GDP Over Time", 
       x = "Year", y = "Percentage of GDP") +
  theme_minimal()



# Graph 3
# Filter data for patent applications
patent_applications <- dt %>%
  filter(`Indicator.Name` %in% c("Patent applications, nonresidents", "Patent applications, residents"))

# Plot the comparison of patent applications
ggplot(patent_applications, aes(x = Year, y = Indicator_Value, color = `Indicator.Name`)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparison of Patent Applications by Residents and Nonresidents", 
       x = "Year", y = "Number of Applications") +
  theme_minimal()









































