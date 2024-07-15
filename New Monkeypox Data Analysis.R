library(forecast)
library(xts)
library(ggplot2)
library(readxl)
library(splines)
library(MASS)
library(readxl)
library(forecast)

weekly_monkeypox_cases <- read_excel("Desktop/Summer Lab Program/Subproject 1.3/weekly monkeypox cases.xlsx", 
                                     sheet = "Canada Table")
View(weekly_monkeypox_cases)

data <- weekly_monkeypox_cases

par(mfrow=c(2,2))
hist(data$new_cases_smoothed, col = "magenta", main = "Histogram of Canada's Weekly Cases")

plot(density(data$new_cases_smoothed), main = "Density Distribution of Canada's Weekly Cases", col = "magenta", lwd = 4)

boxplot(data$new_cases_smoothed, col = "magenta", main = "Boxplot of Canada's Weekly Cases", xlab = "Cases", ylab = "Monkeypox", horizontal = TRUE, lwd = 4)

qqnorm(data$new_cases_smoothed, main = "Normal Q-Q Plot of Canada's Weekly Cases", lwd = 4, col = "magenta")
qqline(data$new_cases_smoothed)
dev.off()

data <- data
data$weekday <- weekdays(data$date)
b <- data$weekday
data

data$date

data$date <- as.Date(data$date)
a <- data$new_cases_smoothed
x <- data$date
cases <- glm(formula = a  ~ bs(x, knots=8) + as.factor(b), family = quasipoisson("log"), data = data )
summary(cases)
coef(cases)

plot(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "magenta", main = "7-Day Rolling Average of Monkeypox Cases in Canada")
lines(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "black")
axis(1, x,format(x, "%d-%m-%y"))

rsid <- residuals(cases)
d <- deviance(cases)
par(mfrow=c(2,2))
plot(data$new_cases, rsid)
boxplot(rsid~weekday,xlab="Weekday",ylab = "Std residuals",data = data)
qqnorm(rsid, ylab="Std residuals")
qqline(rsid,col="magenta",lwd=2)
hist(rsid)

n=length(cases$fitted.values)
p = 10 # 10 Predictor Variables.
D = sum(residuals(cases, type="deviance")^2)
phi = D/(n-p-1)
print(phi)

df <- n-p-1
## Test for GOF: Using deviance residuals
deviances2 <- residuals(cases,type="deviance")
dev.tvalue <- sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,df))

c(deviance(cases), 1-pchisq(deviance(cases),df))


weekly_monkeypox_cases <- read_excel("Desktop/Summer Lab Program/Subproject 1.3/weekly monkeypox cases.xlsx", 
                                     sheet = "USA Table")
View(weekly_monkeypox_cases)

data <- weekly_monkeypox_cases

par(mfrow=c(2,2))
hist(data$new_cases_smoothed, col = "blue", main = "Histogram of USA's Weekly Cases")

plot(density(data$new_cases_smoothed), main = "Density Distribution of USA's Weekly Cases", col = "blue", lwd = 4)

boxplot(data$new_cases_smoothed, col = "blue", main = "Boxplot of USA's Weekly Cases", xlab = "Cases", ylab = "Monkeypox", horizontal = TRUE, lwd = 4)

qqnorm(data$new_cases_smoothed, main = "Normal Q-Q Plot of USA's Weekly Cases", lwd = 4, col = "blue")
qqline(data$new_cases_smoothed)
dev.off()

data <- data
data$weekday <- weekdays(data$date)
b <- data$weekday
data

data$date

data$date <- as.Date(data$date)
a <- data$new_cases_smoothed
x <- data$date
cases <- glm(formula = a  ~ bs(x, knots=8) + as.factor(b), family = quasipoisson("log"), data = data )
summary(cases)


plot(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "blue", main = "7-Day Rolling Average of Monkeypox Cases in USA")
lines(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "black")
axis(1, x,format(x, "%d-%m-%y"))

rsid <- residuals(cases)
d <- deviance(cases)
par(mfrow=c(2,2))
plot(data$new_cases, rsid)
boxplot(rsid~weekday,xlab="Weekday",ylab = "Std residuals",data = data)
qqnorm(rsid, ylab="Std residuals")
qqline(rsid,col="blue",lwd=2)
hist(rsid)

n=length(cases$fitted.values)
p = 10 # 10 Predictor Variables.
D = sum(residuals(cases, type="deviance")^2)
phi = D/(n-p-1)
print(phi)

df <- n-p-1
## Test for GOF: Using deviance residuals
deviances2 <- residuals(cases,type="deviance")
dev.tvalue <- sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,df))



weekly_monkeypox_cases <- read_excel("Desktop/Summer Lab Program/Subproject 1.3/weekly monkeypox cases.xlsx", 
                                     sheet = "UK Table")
View(weekly_monkeypox_cases)

data <- weekly_monkeypox_cases


par(mfrow=c(2,2))
hist(data$new_cases_smoothed, col = "grey", main = "Histogram of UK's Weekly Cases")

plot(density(data$new_cases_smoothed), main = "Density Distribution of UK's Weekly Cases", col = "grey", lwd = 4)

boxplot(data$new_cases_smoothed, col = "grey", main = "Boxplot of UK's Weekly Cases", xlab = "Cases", ylab = "Monkeypox", horizontal = TRUE, lwd = 4)

qqnorm(data$new_cases_smoothed, main = "Normal Q-Q Plot of UK's Weekly Cases", lwd = 4, col = "grey")
qqline(data$new_cases_smoothed)
dev.off()

data <- data
data$weekday <- weekdays(data$date)
b <- data$weekday
data

data$date

data$date <- as.Date(data$date)
a <- data$new_cases_smoothed
x <- data$date
cases <- glm(formula = a  ~ bs(x, knots=8) + as.factor(b), family = quasipoisson("log"), data = data )
summary(cases)


plot(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "grey", main = "7-Day Rolling Average of Monkeypox Cases in UK", type = "b")
lines(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "black")
axis(1, x,format(x, "%d-%m-%y"))


rsid <- residuals(cases)
d <- deviance(cases)
par(mfrow=c(2,2))
plot(data$new_cases, rsid)
boxplot(rsid~weekday,xlab="Weekday",ylab = "Std residuals",data = data)
qqnorm(rsid, ylab="Std residuals")
qqline(rsid,col="grey",lwd=2)
hist(rsid)

n=length(cases$fitted.values)
p = 10 # 10 Predictor Variables.
D = sum(residuals(cases, type="deviance")^2)
phi = D/(n-p-1)
print(phi)

df <- n-p-1
## Test for GOF: Using deviance residuals
deviances2 <- residuals(cases,type="deviance")
dev.tvalue <- sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,df))


weekly_monkeypox_cases <- read_excel("Desktop/Summer Lab Program/Subproject 1.3/weekly monkeypox cases.xlsx", 
                                     sheet = "Germany Table")
View(weekly_monkeypox_cases)

data <- weekly_monkeypox_cases

par(mfrow=c(2,2))
hist(data$new_cases_smoothed, col = "purple", main = "Histogram of Germany's Weekly Cases")

plot(density(data$new_cases_smoothed), main = "Density Distribution of Germany's Weekly Cases", col = "purple", lwd = 4)

boxplot(data$new_cases_smoothed, col = "purple", main = "Boxplot of Germany's Weekly Cases", xlab = "Cases", ylab = "Monkeypox", horizontal = TRUE, lwd = 4)

qqnorm(data$new_cases_smoothed, main = "Normal Q-Q Plot of Germany's Weekly Cases", lwd = 4, col = "purple")
qqline(data$new_cases_smoothed)
dev.off()

data <- data
data$weekday <- weekdays(data$date)
b <- data$weekday
data

data$date

data$date <- as.Date(data$date)
a <- data$new_cases_smoothed
x <- data$date
cases <- glm(formula = a  ~ bs(x, knots=8) + as.factor(b), family = quasipoisson("log"), data = data )
summary(cases)


plot(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "purple", main = "7-Day Rolling Average of Monkeypox Cases in Germany", type = "b")
lines(x, a, xaxt = "n", xlab = "Date", ylab = "Daily Cases", col = "black")
axis(1, x,format(x, "%d-%m-%y"))

rsid <- residuals(cases)
d <- deviance(cases)
par(mfrow=c(2,2))
plot(data$new_cases, rsid)
boxplot(rsid~weekday,xlab="Weekday",ylab = "Std residuals",data = data)
qqnorm(rsid, ylab="Std residuals")
qqline(rsid,col="purple",lwd=2)
hist(rsid)

n=length(cases$fitted.values)
p = 10 # 10 Predictor Variables.
D = sum(residuals(cases, type="deviance")^2)
phi = D/(n-p-1)
print(phi)

df <- n-p-1
## Test for GOF: Using deviance residuals
deviances2 <- residuals(cases,type="deviance")
dev.tvalue <- sum(deviances2^2)
c(dev.tvalue, 1-pchisq(dev.tvalue,df))
