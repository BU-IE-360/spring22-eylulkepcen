library(xts)
library(lubridate)
library(forecast)
library(tseries)
library(urca)
library(ggplot2)
library(zoo)

data <- read.csv("C:\\Users\\eylulkepcen\\OneDrive\\Masaüstü\\bogazici\\ie360\\Homework2\\IE360_Spring22_HW2_data.csv")

data$Quarter <- as.yearqtr(data$Quarter, format = "%Y_Q%q")
colnames(data) <- c("Quarter","UGS","RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT")
data_new <- gsub(" ", "", data$UGS)                          
data_new <- as.data.frame(data_new)
colnames(data_new) <- c("UGS")
data_final <- data.frame(data$Quarter, data_new)
data_final$UGS <- as.numeric(data_final$UGS)
data_final$data.Quarter <- as.yearqtr(data_final$data.Quarter, format = "%Y_Q%q")

data_f_ts <- xts(x = data_final[-1], order.by = data_final$data.Quarter, frequency = 4)

plot(data_f_ts,
     main = "Unleaded gasoline sale vs. Time",
     ylab = "Unleaded gasoline sale per Quarter",
     xlab = "Time",
     yaxis.right = FALSE,
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     grid.ticks.lty = 3)

tsdisplay(data_f_ts[1:28,],
          main = "Unleaded gasoline sale",
          xlab = "Unleaded gasoline sale")

acf(data_f_ts[1:28,])

Box.test(diff(data_f_ts), lag=10, type="Ljung-Box")


data_final <- data.frame(data_final, data$RNUV, data$NLPG, data$PU, data$PG,data$NUGV,data$NDGV,data$GNPA, data$GNPC, data$GNPT)
colnames(data_final) <- c("Quarter","UGS","RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT")
data_final$NLPG <- gsub(" ", "", data_final$NLPG)                          
data_final$NLPG <- as.numeric(data_final$NLPG)

data_final$NUGV <- gsub(" ", "", data_final$NUGV)                          
data_final$NUGV <- as.numeric(data_final$NUGV)

data_final$GNPA <- gsub(" ", "", data_final$GNPA)                          
data_final$GNPA <- as.numeric(data_final$GNPA)

data_final$GNPC <- gsub(" ", "", data_final$GNPC)                          
data_final$GNPC <- as.numeric(data_final$GNPC)

data_final$GNPT <- gsub(" ", "", data_final$GNPT)                          
data_final$GNPT <- as.numeric(data_final$GNPT)

data_final$Quarter <- as.yearqtr(data_final$Quarter, format = "%Y_Q%q")

data_ts <- xts(x = data_final[-1], order.by = data_final$Quarter, frequency = 4)

data_final$quarters <- rep(1:4, 8)
data_final$trend <- 32:1
data_final$crisis <- rep(0,32)
data_final$crisis[4:6] <- rep(1,3)

data_ts <- xts(x = data_final[-1], order.by = data_final$Quarter, frequency = 4)

model <- lm(formula = UGS ~ as.factor(quarters) + trend + crisis, data = data_ts)
summary(model)
checkresiduals(model)

USD <- read.csv("C:\\Users\\eylulkepcen\\OneDrive\\Masaüstü\\bogazici\\ie360\\Homework2\\USD_TRY.csv")
colnames(USD) <- c("USD")
data_final <- data.frame(data_final, USD)
plot(x = data_final$USD[1:28], y = model$residuals[1:28], col = "#FD6467",
     main = "Residuals vs. USD TRY Exchange Rate",
     xlab = "USD TRY Exchange Rate",
     ylab = "Residuals")
abline(h = 0, lty = 2)
abline(h = 50000, lty = 3, col = "#FD6467")
abline(h = -50000, lty = 3, col = "#FD6467")

data_ts <- xts(x = data_final[-1], order.by = data_final$Quarter, frequency = 4)
model <- lm(formula = UGS ~ as.factor(quarters) + trend + crisis + USD , data = data_ts)
summary(model)
checkresiduals(model)


plot(x = data_final$PG[1:28], y = model$residuals[1:28], col = "#FD6467",
     main = "Residuals vs. PG",
     xlab = "PG",
     ylab = "Residuals")
abline(h = 0, lty = 2)
abline(h = 50000, lty = 3, col = "blue")
abline(h = -50000, lty = 3, col = "blue")


plot(x = data_final$GNPA[1:28], y = model$residuals[1:28], col = "#FD6467",
     main = "Residuals vs. GNPA",
     xlab = "GNPA",
     ylab = "Residuals")
abline(h = 0, lty = 2)
abline(h = 50000, lty = 3, col = "blue")
abline(h = -50000, lty = 3, col = "blue")


plot(x = data_final$GNPC[1:28], y = model$residuals[1:28], col = "darkred",
     main = "Residuals vs. GNPC",
     xlab = "GNPC",
     ylab = "Residuals")
abline(h = 0, lty = 2)
abline(h = 50000, lty = 3, col = "blue")
abline(h = -50000, lty = 3, col = "blue")


model <- lm(formula = UGS ~ as.factor(quarters) + trend + crisis + USD + PG  + GNPC + GNPA, data = data_ts)
summary(model)
checkresiduals(model)

data_final$residual_lag4 <- c(0,0,0,0,model$residuals)
data_ts <- xts(x = data_final[-1], order.by = data_final$Quarter, frequency = 4)

model <- lm(formula = UGS ~ as.factor(quarters) + residual_lag4 + trend + crisis + USD + PG + GNPC + GNPA, data = data_ts)
summary(model)
checkresiduals(model)


model <- lm(formula = UGS ~ as.factor(quarters) + trend + crisis + USD + PG + GNPC + GNPA, data = data_ts)
final <- xts(x = data.frame(data_final$UGS[1:32],predict(model, data_ts)), order.by = data_final$Quarter[1:32], frequency = 4)
colnames(final) <- c("Real", "Predicted")
plot(final,
     legend.loc = "topright",
     main = "Unleaded gasoline sale vs. Time",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("#FD6467","black"),
     grid.ticks.lty = 3)

final$Predicted[29:32]
