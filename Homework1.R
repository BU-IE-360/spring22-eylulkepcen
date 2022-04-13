library(ggplot2)    
library(ggfortify)
library(xts)      
library(ggcorrplot)
library(readxl)
library(GGally)

EVDS_new <- read_excel("C:\\Users\\eylulkepcen\\Downloads\\EVDS_data.xlsx")

colnames(EVDS_new) = c("Date","UnemploymentRate", "CPI", "HousingUnitPrice") 
EVDS_new[,4] <- EVDS_new[,4]/100
EVDS_new[,3] <- EVDS_new[,3]/10
EVDS_new[,2] <- EVDS_new[,2]/1

EVDS_new$Date <- as.yearmon(x = EVDS_new$Date)
EVDS_time <- xts(x = EVDS_new[-1],order.by = EVDS_new$Date, frequency = 12)

colnames(EVDS_time) <- c("UnemploymentRate", "CPI", "HousingUnitPrice")

colors = c("#E6A0C4", "#FD6467", "#7294D4")
colnames <- colnames(EVDS_time)
parameters <- c("Unemployment Rate (%)", "CPI (per 10)", "Housing Unit Price (per 100 TL/m2)")
colnames(EVDS_time) <- parameters

plot(x = EVDS_time, ylab = "", main = "Unemployment Rate, CPI, Housing Unit Price",
     col = colors, lwd = c(2,2,3),legend.loc = "topleft", minor.ticks = "years",
     grid.ticks.on = "quarter", grid.ticks.lty = "dotted")

plot(x = EVDS_time,ylab = "Rate (TRY/%)", main = "Change of the Data with Respect to Time", col = c("darkblue"))

colnames(EVDS_time) <- c("UnemploymentRate", "CPI", "HousingUnitPrice")

EVDS_new$Year <- as.Date(x = EVDS_new$Date, format = "%m %Y")
EVDS_new$Year <- substr(EVDS_new$Year, 0, 4)

EVDS_Unemp <- EVDS_new[,c(1,2,5)]
EVDS_CPI <- EVDS_new[,c(1,3,5)]
EVDS_House <- EVDS_new[,c(1,4,5)]

#Unemployment
ggplot(data = EVDS_Unemp,aes(x = UnemploymentRate)) +
  geom_histogram(bins = 16, alpha = 0.5,aes(color = Year, fill = Year)) +
  geom_density(alpha=.2, fill="#E6A0C4", colour="#7294D4") +
  facet_wrap(facets = .~Year,scales = "free_x",nrow=3) +
  scale_fill_manual(values=c("#F1BB7B", "#FD6467", "#F8AFA8","#90D4CC", "#E6A0C4", "#C6CDF7", "#7294D4","#F4B5BD")) +
  labs(title = "Histograms of Unemployment Rates by Year",
       x = "Unemployment Rate (%)",
       y = "Frequency") +
  theme_minimal()

#CPI
ggplot(data = EVDS_CPI,aes(x = CPI*10)) +
  geom_histogram(bins = 16, alpha = 0.5,aes(color = Year, fill = Year)) +
  geom_density(alpha=.2, fill="#E6A0C4", colour="#7294D4") +
  facet_wrap(facets = .~Year,scales = "free_x",nrow=3) +
  scale_fill_manual(values=c("#F1BB7B", "#FD6467", "#F8AFA8","#90D4CC", "#E6A0C4", "#C6CDF7", "#7294D4","#F4B5BD")) +
  labs(title = "Histograms of CPI by Year",
       x = "CPI",
       y = "Frequency") +
  theme_minimal()

#Housing
ggplot(data = EVDS_House,aes(x = HousingUnitPrice*100)) +
  geom_histogram(bins = 16, alpha = 0.5,aes(color = Year, fill = Year)) +
  geom_density(alpha=.2, fill="#E6A0C4", colour="#7294D4") +
  facet_wrap(facets = .~Year,scales = "free_x",nrow=3) +
  scale_fill_manual(values=c("#F1BB7B", "#FD6467", "#F8AFA8","#90D4CC", "#E6A0C4", "#C6CDF7", "#7294D4","#F4B5BD")) +
  labs(title = "Histograms of Housing Unit Price by Year",
       x = "Housing Unit Price",
       y = "Frequency") +
  theme_minimal()


#Boxplot

data = EVDS_new[,1:4]

data_stacked <- cbind("Date" = as.yearmon(rep(data$Date,3)), stack(data))
years <- factor(format(data$Date, format = "%Y"),ordered = TRUE)
data_stacked$Year <- rep(years,3)
ggplot(data = data_stacked,mapping = aes(x = Year,y = values)) +
  geom_boxplot(mapping = aes(fill = ind)) +
  scale_fill_manual(values=c("#F4B5BD", "#7294D4","#798E87")) +
  labs(title = "Boxplots of CPI, Unemployment Rate, and Housing Unit Price",
       x = "Years", "Rate (TRY/%)",
       y = "Rate",
       fill = "Rate Type") +
  scale_y_continuous(minor_breaks = seq(0,25,by = 1)) +
  theme_minimal()

#Correlation test 
cor.test(x = EVDS_time$UnemploymentRate, y = EVDS_time$CPI, method = "pearson", alternative = "greater")
cor.test(x = EVDS_time$UnemploymentRate, y = EVDS_time$HousingUnitPrice, method = "pearson", alternative = "greater")
cor.test(x = EVDS_time$CPI, y = EVDS_time$HousingUnitPrice, method = "pearson", alternative = "greater")

ggcorrplot(corr = cor(EVDS_time),
           hc.order = TRUE,
           outline.col = "white",
           type = "upper",lab = TRUE,
           title = "Correlation Matrix",
           colors = c("#C6CDF7", "white","#3B9AB2"),
           legend.title = "Correlation",
           ggtheme = theme_void)

pairs = data.frame(rand=NA)
pairs = cbind(pairs, Date = EVDS_new$Date, CPI = EVDS_new$CPI, UnemploymentRate = EVDS_new$UnemploymentRate, Housing=EVDS_new$HousingUnitPrice)
pairs = pairs[,-1]
ggpairs(pairs[,-1]) + theme_minimal() + labs(title ="Scatter Plots of Unemployment Rate, CPI, and Housing Unit Price between 2014-2021")

par(mfrow=c(1,3))
plot(x = data$CPI,y = data$UnemploymentRate,
     main = "CPI vs. UnemploymentRate",
     xlab = "CPI", ylab = "Unemployment Rate (%)",
     sub = "Correlation: 56.80%", col="#CB2314")
plot(x = data$HousingUnitPrice,y = data$UnemploymentRate,
     main = "HousingUnitPrice vs. UnemploymentRate",
     xlab = "HousingUnitPrice", ylab = "Unemployment Rate (%)",
     sub = "Correlation: 39.39%", col="#273046")
plot(x = data$CPI,y = data$HousingUnitPrice,
     main = "CPI vs. HousingUnitPrice",
     xlab = "CPI", ylab = "HousingUnitPrice",
     sub = "Correlation: 95.50%", col = "#354823")