library(tis)                     #load required packages
library(forecast)
library(ggplot2)
library(gridExtra) 
library(MASS)

setwd("C:\\Users\\jjoshuac714\\Desktop\\Econ 144\\Week 3")   #set working directory

### 1. Modeling and Forecasting Trend

##### a) Creating time series plot

sales_em <- read.csv("ESHOP&MAIL.csv")       #store csv datset as data frame "sales_em"
sales_em <- ts(sales_em[,2], 1992, c(2019,11), freq = 12) #transform into time series
t <- ts(sales_em, frequency = 12, start = c(1992, 01), end = c(2019, 11))
t <- as.numeric(time(t))                     #create numeric time dummy vector that corresponds to sales_em data time period

plot(sales_em, main = "Retail Sales: Electronic Shopping and \nMail-Order Houses", 
     xlab = "Time", ylab = "$ in millions")  #plot time series
nberShade()                                  #add gray recession bands
lines(sales_em)                              #overlay plot over gray bands

##### b) Comparison with Total Retail plot to help explain covariance stationary

sales_total <- read.csv("TOTRTRADES.csv", header = FALSE) 
#store total retail trades dataset as data frame "sales_total"
sales_total <- as.numeric(gsub(",","",sales_total[,1]))  
#subset only sales values in column 1 and remove commas as a numeric    


par(mfrow = c(2,1))                                   #shrink plot window size to fit two rows, one column view
plot(sales_em, main = "Retail Sales: Electronic Shopping and \nMail-Order Houses", 
     xlab = "Time", ylab = "$ in millions")           #re-plot sales_em with recession bands
nberShade()
lines(sales_em)
plot(t, sales_total, type = "l", main = "Retail Sales: Total (including Food Services)",                                                
     xlab = "Time", ylab = "$ in millions")           #plot sales_total with recession bands
nberShade()
lines(t, sales_total, col = "navyblue")
par(mfrow = c(1,1))                                
#convert plot windows back to single plot view

##### c) Plotting ACF/PACF of data

ggAcf(sales_em)                  #plot Acf(autocorrelation function) of series 
ggPacf(sales_em)                 #plot Pacf (partial autocorrelation function) of series

##### d) Fitting linear and non-linear models and comparing

t_mod1 <- tslm(sales_em ~ trend)    #create linear model including trend dummy
plot1 <- ggplot() + geom_line(data = as.data.frame(sales_em), aes(x = t, y = sales_em[1:335])) +
   geom_line(data = as.data.frame(t_mod1$fit), aes(x = t, y = t_mod1$fit), col = "red", lwd = 1) +
   ggtitle("Trend Model 1") +
   ylab("sales_em") +
   xlab("Time")                  
#create ggplot the model 1(t_mod1) in red over the actual data (sales_em) as "plot1"

lsales_em <- log(sales_em)             #take natural log of sales_em as "lsales_em"
t_mod2 <- tslm(lsales_em ~ trend)      #create natural log model including trend
plot2 <- ggplot() + geom_line(data = as.data.frame(sales_em), aes(x = t, y = lsales_em[1:335])) +
   geom_line(data = as.data.frame(t_mod2$fit), aes(x = t, y = t_mod2$fit), col = "red", lwd = 1) +
   ggtitle("Trend Model 2") +
   ylab("ln(sales_em)") +
   xlab("Time")                     
#create ggplot the model 2(t_mod2) in red over the actual data (sales_em) as "plot2"

grid.arrange(plot1, plot2, nrow = 2)   
#arrange 2 rows in a single window to plot and visually compare both models

##### e) Plotting residuals vs fitted values for both trend models

plot3 <- ggplot() + geom_point(aes(x = as.numeric(t_mod1$fit), y = as.numeric(t_mod1$res))) +
   geom_line(aes(x = as.numeric(t_mod1$fit), y = 0), col = "red") +
   geom_line(aes(x = 0, y = t_mod1$res), col = "red") +
   ggtitle("Trend Model 1") +
   xlab("Fitted Values") +
   ylab("Residuals")
plot3
plot4 <- ggplot() + geom_point(aes(x = as.numeric(t_mod2$fit), y = as.numeric(t_mod2$res))) +
   geom_line(aes(x = as.numeric(t_mod2$fit), y = 0),col = "red") +
   ggtitle("Trend Model 2") +
   xlab("Fitted Values") +
   ylab("Residuals")

plot4
plot4a <- ggplot() + geom_point(aes(x = as.numeric(t_mod2$fit), y = as.numeric(t_mod2$res))) +
   geom_line(aes(x = as.numeric(t_mod2$fit), y = 0),col = "red") +
   geom_line(linetype = "dotted", aes(x = 9, y = as.numeric(t_mod2$res)), col = "navyblue") +
   geom_line(linetype = "dotted", aes(x = 10, y = as.numeric(t_mod2$res)), col = "navyblue") +
   ggtitle("Trend Model 2") +
   xlab("Fitted Values") +
   ylab("Residuals")

plot(lsales_em, main = "Retail Sales: Electronic Shopping and \nMail-Order Houses", 
     xlab = "Time", ylab = "ln(sales_em)")
nberShade()
lines(lsales_em, col = "black")
abline(h = 9, col = "navyblue", lty = "dotted")
abline(h = 10, col = "navyblue", lty = "dotted")

##### f) Plotting Residual histograms for both models

truehist(t_mod1$res, col = "skyblue", xlab = "Residuals", ylab = "Fraction",  
         main = "Trend Model 1 \nResiduals Histogram", xlim = c(-20000, 20000)) #plot histogram of t_mod1 
xr <- rnorm(1000000, mean(t_mod1$res), sd(t_mod1$res))  
#create a sample normal distribution from 1 mil. random values about t_mod1's residual mean and std deviation as "xr"
lines(density(xr), col = "red", lwd = 2)                #plot "xr" line
lines(density(t_mod1$res), col = "darkblue", lwd = 2)   #plot density distribution of t_mod1 residuals
legend(x = "topright", legend = c("Density", "Normal Distribution"), 
       col = c("darkblue", "red"), lty = 1, lwd = 2, bty = "n")  
#create legend for both distribution lines with corresponding color scheme

truehist(t_mod2$res, col = "skyblue", xlab = "Residuals", ylab = "Fraction",
         main = "Trend Model 2 \nResiduals Histogram", xlim = c(-0.6, 0.6))     #plot histogram of t_mod2
xr <- rnorm(1000000, mean(t_mod2$res), sd(t_mod2$res))  
#create a sample normal distribution from 1 mil. random values about t_mod2's residual mean and std deviation as "xr"
lines(density(xr), col = "red", lwd = 2)                #plot new "xr" line
lines(density(t_mod2$res), col = "darkblue", lwd = 2)   #plot density distribution of t_mod2 residuals
legend(x = "topright", legend = c("Density", "Normal Distribution"), 
       col = c("darkblue", "red"), lty = 1, lwd = 2, bty = "n")  
#create legend for both distribution lines with corresponding color scheme

##### g) Getting diagnostic statistics

summary(t_mod1)

summary(t_mod2)

##### h) Getting model selection criteria AIC and BIC for both models 

AIC(t_mod1, t_mod2)
BIC(t_mod1, t_mod2)

##### i) Using trend model 2 (t_mod2) to forecast 5 years ahead

fore1 <- forecast(t_mod2, h = 60)                              
#create forecast of model 2 for 60 monthly periods (5 years) as "fore1"
plot(fore1, main = "Trend Model 2", xlab = "Time", ylab = "lsales_em") #plot fore1
legend(x = "bottomright", legend = c("80% Prediction Interval", "95% Prediction Interval"), 
       fill = c("lightsteelblue3", "gray87"), bty = "n", cex = 1)    
#create legend that corresponds to the prediction intervals
legend(x = "bottom", "Point Forecast", col = "blue", lty = 1, lwd = 1.5, 
       bty = "n", cex = 1)                                           
#create legend that corresponds to the point forecast

### 2. Modeling and Forecasting Seasonality

##### a) Constructing seasonal aspect of lsales_em and looking at summary statistics


ggsubseriesplot(sales_em)
ggseasonplot(sales_em)

par(mfrow = c(2,1))                    #create plot window with 2 rows
s_mod1 <- tslm(lsales_em ~ season + 0) #create model based on lsales_em including a seasonal dummy and intercept as "s_mod1"
summary(s_mod1)                        #return summary statistics of s_mod1
plot(s_mod1$fit, xlim = c(1992, 2020), ylab = "Seasonal Model 1",  
     main = "Seasonal Component of Dummy Model")  #plot s_mod1 fitted values as a time series

s_lsales_em <- stl(lsales_em, s.window = "periodic")  #create periodic stl(seasonal decomposition of time)
s_lsales_em <- s_lsales_em$time.series[,1]            #subset only the seasonal component
plot(s_lsales_em, ylab = "Seasonal ln(sales_em)", main = "Seasonal Adjustment using\nSTL Decomposition")                                #plot the stl decomposed seasonal component to compare with s_mod1

##### b) Plot seasonal factors

par(mfrow = c(2,1))                                      #create plot window with 2 rows
plot(s_mod1$coef, type = "l", main = "Seasonal Factors", xlab = "Season",
     ylab = "Seasonal Factors", lwd = 2, col = "forestgreen")  #plot coefficients of s_mod1
hist(s_mod1$res, main = "Histogram of Residuals", col = "steelblue2", xlim = c(-2, 2), xlab = "Seasonal Model 1 Residuals")                                 #plot histogram of s_mod1 residuals

##### c) Incorporate both trend and seasonal dummies to create full model and plot residuals vs fitted values

par(mfrow = c(1,1))                                    
full_mod <- tslm(lsales_em ~ trend + season)           
#create full model of lsales_em containing both trend and seasonality as "full_mod"
plot5 <- ggplot() + geom_point(aes(x = as.numeric(full_mod$fit), y = as.numeric(full_mod$res))) +
   geom_line(aes(x = as.numeric(full_mod$fit), y = 0), col = "red") +
   ggtitle("Full Model") +
   xlab("Fitted Values") +
   ylab("Residuals")                                #plot full_mod's residuals vs fitted values
plot5                                                  


grid.arrange(plot5 + ylim(-0.4, 0.4), plot4, nrow = 2)
#plot and compare t_mod2 and full_mod for residuals vs fit

##### d) Summary statistics and Error metrics

summary(full_mod)                       #summary statistics of full_mod

AIC(t_mod1, t_mod2, s_mod1, full_mod)   #return AIC of both trend models, seasonal model, and full model
BIC(t_mod1, t_mod2, s_mod1, full_mod)   #return BIC of both trend models, seasonal model, and full model

accuracy(full_mod)                      #return error metrics of full model
accuracy(t_mod2)                        #return error metrics of trend model 2 (the next best fit/model)

##### e) Using full model (full_mod) to forecast 5 years ahead

fore2 <- forecast(full_mod, h = 60)    #create forecast of full model for 60 monthly periods (5 years) as "fore2"
plot(fore2, main = "Full Model", xlab = "Time", ylab = "lsales_em")
plot(fore2, main = "Full Model", xlab = "Time", ylab = "lsales_em", #zoom into fore2 plot for 2017 to 2025
     xlim = c(2017, 2025))
legend(x = "bottomright", legend = c("80% Prediction Interval", "95% Prediction Interval"), 
       fill = c("lightsteelblue3", "gray87"), bty = "n", cex = 1.1)  
#create legend that accounts for the prediction intervals
legend(x = "bottomleft", "Point Forecast", col = "blue", lty = 1,
       lwd = 2, bty = "n", cex = 1.1)
#create legend that accounts for the point forecast
