library(xts)#create time series
library(CausalImpact)#CausalImpact Analysis
library(readr)#read csv
library(tidyr)#gather
library(ggplot2)
library(dplyr)


setwd("C:/Users/Jessee/Desktop/York University/CSDA1040/Project 3 Time-Series")
sales <- read_csv("sales data-set.csv")
str(sales)
glimpse(sales)

#Change variables type to appropriate type for processing
sales <-
  sales %>%
  mutate(Store = as.factor(Store),
         Dept = as.factor(Dept),
         Date = as.Date(Date, "%d/%m/%Y"))

str(sales)
levels(sales$Store)
levels(sales$Dept)

#arbitrarily select a store for analysis (can be repeated for any store of interest)
sales_s1 <-
  sales %>%
  filter(Store == 1,
         Dept == 1 | Dept == 2 | Dept == 3 | Dept == 4) %>%
  dplyr::select(-IsHoliday)

glimpse(sales_s1)


#Visualize sales data by department
options(repr.plot.width=6, repr.plot.height=3)

ggplot(sales_s1, aes(x = Date, y = Weekly_Sales, colour = Dept)) +
  geom_line()
#RESULTS FROM STORE 1
#department 1 (orange) - seasons sales starting around nov/oct and ending around march/apr.  Implies seasonality (winter months)
#department 2 (green) - sales peaking around christmas
#department 3 (blue) - sales peaking every summer around june/july
#department 4 (purple) more even in sales, slight lift around christmas

#Analysis will be carried out on department 4 since there are no peaks
##OBJECTIVE #1 Predict future sales, will the trend persist (Use ARIMA)
##OBJECTIVE #2 Simulate a sales campaign


#Preapring data for analysis (pull complete periods of activity for Dept 4)
salesd4 <-
  sales_s1 %>%
  filter(Dept == 4)# Select department 4 only 

########################################################################################
#OBJECTIVE 1 - ARIMA FORECASTING

#plot department 4 2 year complete sales
salesd4 %>%
  dplyr::select(Date, Weekly_Sales) %>%
  gather(type, sales, -Date) %>%
  ggplot(aes(x = Date, y = sales)) +
  geom_line() +
  ylim(30000, 70000) +
  facet_wrap(~type, ncol = 1)

#remove store and dept columns (already filtered data to be Store 1 Dept 4 only)
head(salesd4_2year)
salesd4 = select(salesd4, -c (Store, Dept))

#create 2 new columns Month and Year
salesd4_m =
  salesd4 %>%
  mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(Weekly_Sales))

#create time series with 12 month cycle starting Feb 2010 and ending Oct 2012
salesd4_ts <- ts(salesd4_m$total, freq = 12, start = c(2010, 2), end = c(2012, 10)) # transform to ts
print(salesd4_ts, digits = 3)
str(salesd4_ts)

#breakdown ts into components
ts = decompose(salesd4_ts)

#get seasonal figures
ts$figure
plot(ts$figure, type="b", xaxt="n", xlab="")

# get names of 12 months in English words
monthNames <- months(ISOdate(2010,1:12,1))

# label x-axis with month names
# las is set to 2 for vertical label orientation
axis(1, at=1:12, labels=monthNames, las=2)
plot(ts)

#Time series forecasting is to forecast future events based on known past data. 
#To forecast future events based on known past data
#For example, to predict the price of a stock based on its past performance
#Popular models
# -Autoregressive moving average (ARMA)
#Autoregressive integrated moving average (ARIMA)
#install.packages("forecast")
library(forecast)
fit <- arima(salesd4_ts, order=c(0,0,0), list(order=c(0,1,0), period=12))
#fit <- auto.arima(salesd4_ts)
fore <- predict(fit, n.ahead=24)

# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se

ts.plot(salesd4_ts, fore$pred, U, L, col=c(1,2,4,4),lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2), cex=0.5)

#pretty plot
plot(forecast(auto.arima(ts(salesd4_ts,frequency=24),D=1),h=24))

#trend holds, same seasons/cyclical spikes but largely uniform compared to other departments

###################################################################################
#OBJECTIVE 2 - Simulate Sales Campaign

#filter 2 complete cycles (spike occur seasonally)
sales_marketing <-
  sales_s1 %>%
  #filter(Date > "2010-06-01",
         #Date < "2012-06-01") %>%
  mutate(marketing_campaign = ifelse(Dept == 4 & Date > "2012-02-28" & Date < "2012-04-01", TRUE, FALSE))#select period for Marketing Campaign

# check marketing campaign variable
sales_marketing %>%
  filter(Date > "2012-02-01" & Date < "2012-05-01",Dept == 4)

#Adjust sales based on marketing campaign, using 30% sales increase projected (weekly_sales * 1.3).
sales_adj <-
  sales_marketing %>%
  mutate(adj_sales = ifelse(marketing_campaign == TRUE, 
                            Weekly_Sales * 1.3, 
                            Weekly_Sales))

# plot: historical data vs marketing campaign data
#around mark there is a lift
sales_adj %>%
  filter(Dept == 4 & Date > "2011-06-01") %>%
  dplyr::select(Date, Weekly_Sales, adj_sales) %>%
  gather(type, sales, -Date) %>%
  ggplot(aes(x = Date, y = sales)) +
  geom_line() +
  ylim(30000, 70000) +
  facet_wrap(~type, ncol = 1)

#Cause impact analysis requires a matrix format for analysis. Separate departments into columns
marketing_CIA <-
  sales_adj %>%
  dplyr::select(Date, Dept, adj_sales) %>%
  spread(Dept, adj_sales) %>%
  rename(dept1 = 2, 
         dept2 = 3,
         dept3 = 4,
         dept4 = 5) %>%
  dplyr::select(Date, dept4, dept1, dept2, dept3)

head(marketing_CIA)

#plot
marketing_CIA %>%
  filter(Date > "2011-06-01") %>%
  ggplot(aes(x = Date, y = dept4)) +
  geom_line() +
  ylim(30000, 70000)

#Needed to convert into time series
marketing_xts <-
  marketing_CIA %>%
  dplyr::select(-Date) %>%
  as.xts(order.by = marketing_CIA$Date)

head(marketing_xts)
tail(marketing_xts)
plot(marketing_xts)
plot(marketing_xts$dept4)

###################################
###### CAUSAL INFERENCE MODEL #####
###################################

# create pre sale and post periods
pre_period <- as.Date(c("2010-02-01", "2012-02-28"))# period before marketing campaign
post_period <- as.Date(c("2012-03-01", "2012-03-31"))#duration of marketing campaign

# perform Bayesian structural time series analysis for causal inference
#install.packages("CausalImpact")
library(CausalImpact)
marketing_causal <- CausalImpact(marketing_xts, 
                                 pre.period = pre_period, 
                                 post.period = post_period)

# summary report of model and plot
summary(marketing_causal)
plot(marketing_causal, "original")

