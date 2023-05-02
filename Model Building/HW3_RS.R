# Loading Libraries

library(tidyverse)
library(mosaic)
library(forecast)
library(readxl)

# Loading the data 
WebSales <- read_excel("/Users/ruchi/Desktop/Ruchi's Work/UT Acads/Spring/TimeSeries/HWs/Web analytics sales.xlsx")
View(WebSales)
WebSales = WebSales %>% mutate(MONTH=1:69)

# LogSales
WebSales = WebSales %>% mutate(logSales = log(SALES))

# Regression
lm1 = lm(logSales ~ MONTH, data = WebSales)
summary(lm1)

# Test for L
library(lmtest)
resettest(lm1,power=2:4,type="fitted")

# Test for I
library(car)
durbinWatsonTest(lm1,max.lag=4,method="normal",alternative="positive")

# Test for N
# K-S 
fav_stats(lm1$residuals)
ks.test(lm1$residuals,"pnorm",0,0.3315651)
# Shapiro
shapiro.test(lm1$residuals)

# Test for E
library(whitestrap) 
white_test(lm1)

# Seasonal Indicators
WebSales$MonthNumber <- format(as.Date(WebSales$Month, format="%Y/%m/%d"),"%m")

WebSales = WebSales %>%
  mutate(Jan = ifelse(MonthNumber == '01', 1, 0),
         Feb = ifelse(MonthNumber == '02', 1, 0),
         Mar = ifelse(MonthNumber == '03', 1, 0),
         Apr = ifelse(MonthNumber == '04', 1, 0),
         May = ifelse(MonthNumber == '05', 1, 0),
         Jun = ifelse(MonthNumber == '06', 1, 0),
         Jul = ifelse(MonthNumber == '07', 1, 0),
         Aug = ifelse(MonthNumber == '08', 1, 0),
         Sep = ifelse(MonthNumber == '09', 1, 0),
         Oct = ifelse(MonthNumber == '10', 1, 0),
         Nov = ifelse(MonthNumber == '11', 1, 0),
         Dec = ifelse(MonthNumber == '12', 1, 0))

# leaving out Dec
# lm2 = lm(logSales ~ Month + I(MonthNumber, data = WebSales) 
lm2 = lm(logSales ~ MONTH + Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov, data=WebSales)
summary(lm2)
lrtest(lm1, lm2)
ss_0 = sum(WebSales$logSales^2)-sum(lm2$residuals^2)
ss_1 = sum(WebSales$logSales^2)-sum(lm1$residuals^2)
ss_0-ss_1
# creating lagged variables 

WebSales = WebSales %>% mutate(lagLSales1=lag(logSales))
WebSales = WebSales %>% mutate(lagLSales2=lag(lagLSales1))
WebSales = WebSales %>% mutate(lagLSales3=lag(lagLSales2))
WebSales = WebSales %>% mutate(lagLSales4=lag(lagLSales3))
WebSales = WebSales %>% mutate(lagLSales5=lag(lagLSales4))
WebSales = WebSales %>% mutate(lagLSales6=lag(lagLSales5))
WebSales = WebSales %>% mutate(lagLSales7=lag(lagLSales6))
WebSales = WebSales %>% mutate(lagLSales8=lag(lagLSales7))
WebSales = WebSales %>% mutate(lagLSales9=lag(lagLSales8))
WebSales = WebSales %>% mutate(lagLSales10=lag(lagLSales9))
WebSales = WebSales %>% mutate(lagLSales11=lag(lagLSales10))
WebSales = WebSales %>% mutate(lagLSales12=lag(lagLSales11))

lm3 = lm(logSales ~ MONTH + lagLSales1 + lagLSales2 + lagLSales3 + lagLSales4 + 
           lagLSales5 + lagLSales6 + lagLSales7 + lagLSales8 + lagLSales9 + lagLSales10 + 
           lagLSales11 + lagLSales12, data=WebSales)
summary(lm3)

# lrtest(lm1, lm3) # giving error
WebSales2 <- na.omit(WebSales)

lm1_2 = lm(logSales ~ MONTH, data = WebSales2)
summary(lm1_2)
lm3_2 = lm(logSales ~ MONTH + lagLSales1 + lagLSales2 + lagLSales3 + lagLSales4 + 
           lagLSales5 + lagLSales6 + lagLSales7 + lagLSales8 + lagLSales9 + lagLSales10 + 
           lagLSales11 + lagLSales12, data=WebSales2)
summary(lm3_2)
lrtest(lm1_2, lm3_2)

ss_0 = sum(WebSales$logSales^2)-sum(lm3_2$residuals^2)
ss_1 = sum(WebSales$logSales^2)-sum(lm1_2$residuals^2)
ss_0-ss_1

lm3_final = lm(logSales ~ MONTH + lagLSales1 + lagLSales12, data=WebSales2)
summary(lm3_final)

# Subset selection

nullmodel = lm(logSales ~ MONTH, data=WebSales2) 
fullmodel = lm(logSales ~ MONTH + lagLSales1 + lagLSales2 + lagLSales3 + lagLSales4 + 
                 lagLSales5 + lagLSales6 + lagLSales7 + lagLSales8 + lagLSales9 + lagLSales10 + 
                 lagLSales11 + lagLSales12, data=WebSales2)

step_model = step(nullmodel,scope=formula(fullmodel), direction="forward") 
summary(step_model)

bestmodel = lm(logSales ~ MONTH + lagLSales12 + lagLSales1, data=WebSales2)
summary(bestmodel)

ss = sum(WebSales2$logSales^2)-sum(bestmodel$residuals^2)
ss2 = sum(WebSales2$logSales^2)-sum(nullmodel$residuals^2)
ss3 = sum(WebSales2$logSales^2)-sum(fullmodel$residuals^2)
ss
ss2
ss3
ss-ss2
ss-ss3
ss/ss2
ss/ss3

(ss-ss2)/(ss3-ss2) # proportion of increased model



# Building the Model

# Check XY relationship visually
ggplot(WebSales, aes(x=Month, y=logSales)) + 
  geom_line(color = 'gray65')  + 
  geom_point(size = 1.5) +
  geom_smooth(method="lm",col="red", se=FALSE) +
  ggtitle("logSales vs Month") +
  scale_x_continuous(name = "Month")

# Check XY relationship visually
ggplot(WebSales, aes(x=Month, y=logSales)) + 
  geom_line(color = 'gray65')  + 
  geom_point(size = 1.5) +
  geom_smooth(method="lm",formula=y~x+I(x^2),col="red", se=FALSE) +
  ggtitle("logSales vs Month") +
  scale_x_continuous(name = "Month")

model1 = lm(logSales~MONTH+I(MONTH^2), data = WebSales)
detrend = WebSales$logSales - fitted(model1)

library(forecast)

model_arima = auto.arima(detrend, max.p = 12, max.d = 1, max.q = 1, seasonal = TRUE)
summary(model_arima)

final_model = arima(WebSales$logSales, c(1,0,0), xreg=WebSales$MONTH)
summary(final_model)
checkresiduals(final_model)

# lagLSales1

WebSales3 <- WebSales[2:nrow(WebSales),]

final_model2 <- lm(logSales~MONTH+I(MONTH^2)+lagLSales1, data = WebSales3)

summary(final_model2)

resettest(final_model2,power=2,type="fitted")
resettest(final_model2,power=3,type="fitted")
resettest(final_model2,power=4,type="fitted")

durbinWatsonTest(final_model2,max.lag=4)
shapiro.test(final_model2$residuals)
white_test(final_model2)

durbinWatsonTest(final_model2)

# Forecasting 

pred1=data.frame(lagLSales1=13.18431, MONTH=70)
f1 = forecast(final_model2, newdata=pred1, h=1)
f1

# Convert to original scale
exp(c(f1$mean,f1$lower,f1$upper))






