library(tidyverse)
library(lubridate)
options(scipen = 100)
sales_data <- read_csv("Walmart_Store_sales.csv")
head(sales_data)

# Cleaning and Processing
sales_data <- sales_data %>%
  mutate(Date = dmy(Date),Weekly_Sales = as.integer(Weekly_Sales))
# Finding which store has maximum sales
sales_data %>% 
  group_by(Store) %>%
  summarize(totalsales=sum(Weekly_Sales)) %>%
  arrange(desc(totalsales))

"Store 20 hast the most sales in dataset."

# Which store has maximum standard deviation i.e., the sales vary a lot.
# Also, find out the coefficient of mean to standard deviation 

sales_data %>% 
  group_by(Store) %>% 
  summarize(sd= sd(Weekly_Sales),mean= mean(Weekly_Sales),CV= sd/mean) %>%
  arrange(desc(sd))

"Store 14's sales vary a lot than others. It has 0.157 CV."

# Which store/s has good quarterly growth rate in Q3’2012

q2_sales <- sales_data %>%
  group_by(Store) %>%
  filter(Date >= "2012-04-01", Date <= "2012-06-30") %>%
  summarise(sales_q2 = sum(Weekly_Sales))

q3_sales <- sales_data %>%
  group_by(Store) %>%
  filter(Date >= "2012-07-01", Date <= "2012-09-30") %>%
  summarise(sales_q3 = sum(Weekly_Sales))

growth_rates <- right_join(q2_sales,q3_sales,by="Store")

growth_rates%>%
  mutate(growthrate = (sales_q3-sales_q2)/sales_q2*100) %>%
  arrange(desc(growthrate))

"Store 7 the best growth rate in Q3 with 13.3%. Store 16, 35, 26 has good growth rates"

#Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together

Super_Bowl <- c("12-Feb-10", "11-Feb-11", "10-Feb-12", "8-Feb-13")
Super_Bowl <- dmy(Super_Bowl)

Labour_Day <- c("10-Sep-10", "9-Sep-11", "7-Sep-12", "6-Sep-13")
Labour_Day <- dmy(Labour_Day)

Thanksgiving <- c("26-Nov-10", "25-Nov-11", "23-Nov-12", "29-Nov-13")
Thanksgiving <- dmy(Thanksgiving)

Christmas <- c("31-Dec-10", "30-Dec-11", "28-Dec-12", "27-Dec-13")
Christmas <- dmy(Christmas)

holiday_sales <- sales_data %>%
  mutate(Holiday = case_when(Date %in% Super_Bowl ~ "Super Bowl",
                             Date %in% Labour_Day ~ "Labour Day",
                             Date %in% Thanksgiving ~ "Thanksgiving",
                             Date %in% Christmas ~ "Christmas",
                             TRUE ~ "Non-Holiday"))
holiday_sales %>%
  select(Weekly_Sales,Holiday) %>%
  group_by(Holiday) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  arrange(desc(average_sales))

"In christmas sales decreases from the average non-holiday season. Thanksgiving increases the average sales. Other holidays are closer to the average."

#  Provide a monthly and semester view of sales in units and give insights
monthly_data <- sales_data %>%
  mutate(Month = month(Date,label = TRUE),Year = year(Date),Semester = as.character(semester(Date,with_year = TRUE))) 

#2010 monthly graph

monthly_data %>% geom_
  filter(Year == 2010) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
    ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + geom_col() + ggtitle("2010") +theme(plot.title = element_text(hjust = 0.5))
"Increase in April, July and OCtober. Most increase in December."

# 2011 monthly graph

monthly_data %>%
  filter(Year == 2011) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
  ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + geom_col() + ggtitle("2011") +theme(plot.title = element_text(hjust = 0.5))
"Same trend with the last year"

# 2012 monthly graph

monthly_data %>%
  filter(Year == 2012) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
  ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + geom_col() + ggtitle("2012") +theme(plot.title = element_text(hjust = 0.5))
"Trends has changed. There are increases in March, June, and August."

# Semester view of sales

monthly_data %>% 
  group_by(Semester) %>%
  summarise(Semester_sales = sum(Weekly_Sales)) %>%
  ggplot(aes(x=Semester,y=Semester_sales,fill=Semester)) + geom_col() + ggtitle("Sales by Semesters") +theme(plot.title = element_text(hjust = 0.5))


# Searching for correlation
store1_sales <- sales_data %>%
  filter(Store==1) %>%
  mutate(Year = year(Date),Day = day(Date),Month = month(Date),Date=1:143) %>%
  select(-Store,-Holiday_Flag)
# Sales vs Temperature
ggplot(store1_sales,aes(x=Temperature,y=Weekly_Sales)) + geom_point() + geom_smooth(color="red1") +ggtitle("Sales vs Temperature") + theme(plot.title = element_text(hjust = 0.5))
cor(store1_sales$Weekly_Sales,store1_sales$Temperature)
# Sales vs Fuel Prices
ggplot(store1_sales,aes(x=Fuel_Price,y=Weekly_Sales)) + geom_point() + geom_smooth(color="red1") +ggtitle("Sales vs Fuel Prices") + theme(plot.title = element_text(hjust = 0.5))
cor(store1_sales$Weekly_Sales,store1_sales$Fuel_Price)
# Sales vs CPI
ggplot(store1_sales,aes(x=CPI,y=Weekly_Sales)) + geom_point() + geom_smooth(color="red1") +ggtitle("Sales vs CPI") + theme(plot.title = element_text(hjust = 0.5))
cor(store1_sales$Weekly_Sales,store1_sales$CPI)
# Sales vs Unemployment
ggplot(store1_sales,aes(x=Unemployment,y=Weekly_Sales)) + geom_point() + geom_smooth(color="red1") +ggtitle("Sales vs Unemployment") + theme(plot.title = element_text(hjust = 0.5))
cor(store1_sales$Weekly_Sales,store1_sales$Unemployment)

# Prediction
library(forecast)

sales_ts <- ts(store1_sales$Weekly_Sales,start=c(2010,2),end = c(2012,10),frequency = 52)
add_sales <- decompose(sales_ts,type =c("additive"))
autoplot(add_sales)
multi_sales <- decompose(sales_ts,type=c("multiplicative"))
autoplot(multi_sales) 

#mean method
meanf_sales <- meanf(sales_ts,h=4)
autoplot(meanf_sales,xlab="Year",ylab="Sales")
summary(meanf_sales)

#simple moving average method
library(smooth)
sma_sales <- sma(sales_ts,order=4,h=8,silent=FALSE)
summary(sma_sales)

#linear regression
model1 <- lm(formula=Weekly_Sales ~ Temperature + Fuel_Price + CPI + Unemployment,data=store1_sales)
prediction <- predict(model1,newdata=list(Temperature=46,Fuel_Price=4.5,Unemployment=6.5,CPI=225))
accuracy(model1)

#random forest
library(randomForest)
set.seed(31)
indexes <- sample(1:nrow(store1_sales), size = 72)
training <- store1_sales[indexes[1:71],]
validation1 <- store1_sales[-indexes,]
rf_classifier = randomForest(Weekly_Sales ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
prediction_for_table <- predict(rf_classifier,validation1[,-2])
validation1_1<-pull(validation1,Weekly_Sales)
colSums(table(observed=validation1_1,predicted=prediction_for_table))


