library(reshape)
library(xts)
library(forecast)
library(tidyverse)
library(dplyr)
library(padr) 
library(data.table)
library(TTR)

weather_long <- read.csv("/Users/eylulkepcen/Downloads/weather.csv")
production <- read.csv("/Users/eylulkepcen/Downloads/production.csv")

weather_long$date <- as.Date(weather_long$date, "%Y-%m-%d")
production$date <- as.Date(production$date, "%Y-%m-%d")
weather_wide <- cast(weather_long, date + hour ~ variable + lat + lon, value.var = "value")
weather_wide$date_hour <- paste(weather_wide$date, weather_wide$hour)
weather_wide$date_hour <- paste( weather_wide$date_hour,"00", sep=":")
weather_wide$date_hour <- paste( weather_wide$date_hour,"00", sep=":")
weather_wide$date_hour <- gsub(" ", "-", weather_wide$date_hour)                          
weather_wide$date_hour <- as.POSIXct(weather_wide$date_hour, format = "%Y-%m-%d-%H:%M:%S", origin ="2021-02-01 00:00:00",tz = "GMT")
weather_wide <- weather_wide[,c(39,1:(38))]

production$date_hour <- paste(production$date, production$hour)
production$date_hour <- paste(production$date_hour,"00", sep=":")
production$date_hour <- paste(production$date_hour,"00", sep=":")
production$date_hour <- gsub(" ", "-", production$date_hour)                          
production$date_hour <- as.POSIXct(production$date_hour, format = "%Y-%m-%d-%H:%M:%S", origin ="2021-02-01 00:00:00",tz = "GMT")
production <- production[,c(4,1:(3))]
production <- pad(production[,c(1,4)])
production$hour <- format(as.POSIXct(production$date_hour), format = "%H")
production$date <- format(as.POSIXct(production$date_hour), format = "%Y-%m-%d")
production$month <- format(as.POSIXct(production$date_hour), format = "%m")
production$hour <- as.numeric(production$hour)
production$date <- as.Date(production$date)
production$month <- as.numeric(production$month)

new_dates <- weather_wide[((nrow(production)+1):nrow(weather_wide)),c(1,2,3)]
new_df <- dplyr::tibble(new_dates, production = NA)

production <- production[,-c(5)]
production <- production[,c(1,4,3,2)]
production <- rbind(production, new_df)
production$month <- format(as.POSIXct(production$date_hour), format = "%m")
production$month <- as.numeric(production$month)

plot(x = production$date_hour, y = production$production, 
     xlab = "Time",
     ylab = "Total Production",
     main = "Hourly Solar Power Production",
     type = 'h')

production_ts <- xts(x = production[,c('production')], order.by = production$date_hour)
colnames(production_ts) <- c("production")
tsdisplay(production_ts)

plot(production_ts$production,
     main = "Hourly Solar Power Production",
     ylab = "Production",
     xlab = "Time",
     yaxis.right = FALSE,
     minor.ticks = "months",
     grid.ticks.on = "months",
     grid.ticks.lty = 3,
     type = 'h')

production$trend <- nrow(production):1
production_ts <- xts(x = production[,c('hour','production','month','trend')], order.by = production$date_hour, 24)

model <- lm(formula = production ~ as.factor(hour) + as.factor(month) + trend, data = production_ts)
summary(model)
checkresiduals(model)

production <- data.frame(production, weather_wide[,c(8,17,26,35)])
production_ts <- xts(x = production[,c(3:10)], order.by = production$date_hour)
model <- lm(formula = production ~ as.factor(hour) + as.factor(month)+trend+CLOUD_LOW_LAYER_36.5_33.25+DSWRF_36.5_33.25+REL_HUMIDITY_36.5_33.25+TEMP_36.5_33.25, data = production_ts)
summary(model)
checkresiduals(model)

is.na(production$production) 
which(is.na(production$production))

#Feb 20, 2021
#August 9-10-11,2022
#January 13-14, 2022
#Those dates and their data are missing

#Completing missing data in February 2021
production$production[457:464]<- 0
production$production[475:480]<- 0

production_ts <- xts(x = production[,c(3:10)], order.by = production$date_hour)

model_deneme <- lm(production_ts$production[1:672]~   
                     as.factor(production_ts$hour[1:672])  +
                     production_ts$CLOUD_LOW_LAYER_36.5_33.25[1:672]+
                     production_ts$DSWRF_36.5_33.25[1:672]+
                     production_ts$REL_HUMIDITY_36.5_33.25[1:672]+ 
                     production_ts$TEMP_36.5_33.25[1:672],
                   data = production_ts[1:672])
summary(model_deneme)
predict <- predict(object=model_deneme, newdata = production_ts[457:480])
predict <- data.frame(production$date_hour[1:672],predict)
feb_pred <- xts(x = data.frame(production$production[1:672],predict$predict), order.by = production$date_hour[1:672])
colnames(feb_pred) <- c("Real", "Predicted")
plot(feb_pred,
     legend.loc = "topright",
     main = "Solar Power Production in February 2021",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("#FD6467","black"),
     grid.ticks.lty = 3)
production$production[465:474]<- predict$predict[465:474]

#Completing missing data in August 2021
production$production[4537:4542]<- 0
production$production[4557:4566]<- 0
production$production[4581:4590]<- 0
production$production[4605:4608]<- 0
production_ts <- xts(x = production[,c(3:10)], order.by = production$date_hour)

model_deneme <- lm(production_ts$production[4345:5088]~   
                     as.factor(production_ts$hour[4345:5088])  +
                     production_ts$CLOUD_LOW_LAYER_36.5_33.25[4345:5088]+
                     production_ts$DSWRF_36.5_33.25[4345:5088]+
                     production_ts$REL_HUMIDITY_36.5_33.25[4345:5088]+ 
                     production_ts$TEMP_36.5_33.25[4345:5088],
                   data = production_ts[4345:5088])
summary(model_deneme)
predict <- predict(object=model_deneme, newdata = production_ts[4537:4608])
predict <- data.frame(production$date_hour[4345:5088],predict)
august_pred <- xts(x = data.frame(production$production[4345:5088],predict$predict), order.by = production$date_hour[4345:5088])
colnames(august_pred) <- c("Real", "Predicted")
plot(august_pred,
     legend.loc = "topright",
     main = "Solar Power Production in August 2021",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("#FD6467","black"),
     grid.ticks.lty = 3)
production$production[4543:4556]<- predict$predict[199:212]
production$production[4567:4580]<- predict$predict[223:236]
production$production[4591:4604]<- predict$predict[247:260]


#Completing missing data in January 2022
production$production[8305:8312]<- 0
production$production[8323:8336]<- 0
production$production[8347:8352]<- 0
production_ts <- xts(x = production[,c(3:10)], order.by = production$date_hour)

model_deneme <- lm(production_ts$production[8017:8760]~   
                     as.factor(production_ts$hour[8017:8760])  +
                     production_ts$CLOUD_LOW_LAYER_36.5_33.25[8017:8760]+
                     production_ts$DSWRF_36.5_33.25[8017:8760]+
                     production_ts$REL_HUMIDITY_36.5_33.25[8017:8760]+ 
                     production_ts$TEMP_36.5_33.25[8017:8760],
                   data = production_ts[8017:8760])
summary(model_deneme)

predict <- predict(object=model_deneme, newdata = production_ts[8017:8760])
predict <- data.frame(production$date_hour[8017:8760],predict)
jan_pred <- xts(x = data.frame(production$production[8017:8760],predict$predict), order.by = production$date_hour[8017:8760])
colnames(jan_pred) <- c("Real", "Predicted")
plot(jan_pred,
     legend.loc = "topright",
     main = "Solar Power Production in January 2022",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("#FD6467","black"),
     grid.ticks.lty = 3)
production$production[8313:8322]<- predict$predict[297:306]
production$production[8337:8346]<- predict$predict[321:330]

production[production<0]<- 0

model <- lm(formula = production ~ as.factor(hour) + as.factor(month)+trend+CLOUD_LOW_LAYER_36.5_33.25+DSWRF_36.5_33.25+REL_HUMIDITY_36.5_33.25+TEMP_36.5_33.25, data = production_ts)
summary(model)
#checkresiduals(model)

production_ts <- xts(x = production[,c(3:10)], order.by = production$date_hour, frequency = 24)
final <- xts(x = data.frame(production$production,predict(model, production_ts)), order.by = production$date_hour, frequency = 24)
colnames(final) <- c("Real", "Predicted")

plot(final,
     legend.loc = "topright",
     main = "Solar Power Production vs. Time",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("#FD6467","black"),
     grid.ticks.lty = 3)

ur_kpss = ur.kpss(production_ts$production) 
summary(ur_kpss)
production_ts = data.table(production_ts)
production_ts[,differ:=production-shift(production,12)]
ur_kpss=ur.kpss(production_ts$differ) 
summary(ur_kpss)

production$monthly_trend <- production$production
production$monthly_trend[1:672] <-(sum(production$production[1:672]/28))
production$monthly_trend[673:1416] <-(4146.556/31)
production$monthly_trend[1417:2136] <-(6085.955/30)
production$monthly_trend[2137:2880] <-(9437.120/31)
production$monthly_trend[2881:3600] <-(11001.904/30)
production$monthly_trend[3601:4344] <-(12182.799/31)
production$monthly_trend[4345:5088] <-(11897.100/31)
production$monthly_trend[5089:5808] <-(10351.786/30)
production$monthly_trend[5809:6552] <-(9270.799/31)
production$monthly_trend[6553:7272] <-(6098.576/30)
production$monthly_trend[7273:8016] <-(3854.962/31)
production$monthly_trend[8017:8760] <-(4657.830/31)
production$monthly_trend[8761:9432] <-(5582.57/28)
production$monthly_trend[9433:10176] <-(7707.24/31)
production$monthly_trend[10177:10896] <-(9420.35/30)
production$monthly_trend[10897:11760] <-(9405.97/31)

lags <- seq(72,1440,by=24)
lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), 
                   sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)

production <- production %>% mutate_at(vars(production), funs_(lag_functions))

production_ts <- xts(x = production[,c(3:ncol(production))], order.by = production$date_hour, frequency = 24)

model <- lm(production_ts$production~as.factor(production_ts$hour)*production_ts$DSWRF_36.5_33.25+as.factor(production_ts$month)*production_ts$TEMP_36.5_33.25
            +as.factor(production_ts$month)*production_ts$CLOUD_LOW_LAYER_36.5_33.25+as.factor(production_ts$month)*production_ts$DSWRF_36.5_33.25
              +production_ts$trend+as.factor(production_ts$hour)*production_ts$CLOUD_LOW_LAYER_36.5_33.25+as.factor(production_ts$hour)+
              production_ts$monthly_trend*production_ts$DSWRF_36.5_33.25 +production_ts$monthly_trend*production_ts$TEMP_36.5_33.25+as.factor(production_ts$month)*production_ts$REL_HUMIDITY_36.5_33.25
            +lag_0072+lag_0096+lag_0120+lag_0600+
            +lag_0168+lag_0192+lag_0216+lag_0384+lag_0600
            +lag_0288+lag_0312+lag_0336
            +lag_0360+lag_0432+lag_0456
            +lag_0480+lag_0528+lag_0552+lag_0576
            +lag_0648+lag_0672
            +lag_0720+lag_0744+lag_0816
            +lag_0864+lag_0888+lag_0912+lag_0936
            +lag_0960+lag_0984+lag_1032+lag_1128+lag_1176
            +lag_1200+lag_1224+lag_1248+lag_0768+lag_1272
            , data = production_ts)
summary(model)
#checkresiduals(model)


final <- xts(x = data.frame(production$production[1:nrow(production)],predict(model, production_ts)), order.by = production$date_hour[1:nrow(production)],frequency=24)
colnames(final) <- c("Real", "Predicted")
plot(final[11017:11496],
     legend.loc = "topright",
     main = "Solar Power Production vs. Time",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("#FD6467","black"),
     grid.ticks.lty = 3)

final$Predicted[11689:11712]

paste("",as.character(final$Predicted[11689:11712]),"",collapse=", ",sep="")

