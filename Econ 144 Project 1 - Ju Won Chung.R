library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library("TTR")
library(tis)
require("datasets")
require(graphics)
library("forecast")
#install.packages("astsa")
#require(astsa)
library(RColorBrewer)
library(plotrix)
library(nlstools)
library(seasonal)
library(fpp2)
library(forecast)
library(dynlm)
library(Hmisc)
library(ggplot2)
library(gridExtra)

setwd("C:\\Users\\jjoshuac714\\Desktop\\Econ 144\\Week 3")

#KiM: economic factors: 1.tech progress 2. population growth 3. seasonal consumption 4. recessions

sales_em <- read.csv("ESHOP&MAIL.csv")
sales_em <- ts(sales_em[,2], 1992, c(2019,11), freq = 12)
#or t <- seq(1992, 2019.833, length = length(sales_em))
t <- ts(sales_em, frequency = 12, start = c(1992, 01), end = c(2019, 11))
t <- as.numeric(time(t))

plot(sales_em, main = "Retail Sales: Electronic Shopping and \nMail-Order Houses", 
     xlab = "Time", ylab = "$ in millions")
nberShade()
lines(sales_em)

sales_total <- read.csv("TOTRTRADES.csv", header = FALSE)
sales_total <- as.numeric(gsub(",","",sales_total[,1]))

par(mfrow = c(2,1))
plot(sales_em, main = "Retail Sales: Electronic Shopping and \nMail-Order Houses", 
     xlab = "Time", ylab = "$ in millions")
nberShade()
lines(sales_em)
plot(t, sales_total, type = "l", main = "Retail Sales: Total (including Food Services)",
     xlab = "Time", ylab = "$ in millions")
nberShade()
lines(t, sales_total, col = "navyblue")
par(mfrow = c(1,1))

ggAcf(sales_em)
ggPacf(sales_em)

t_mod1 <- tslm(sales_em ~ trend)
plot1 <- ggplot() + geom_line(data = as.data.frame(sales_em), aes(x = t, y = sales_em[1:335])) +
      geom_line(data = as.data.frame(t_mod1$fit), aes(x = t, y = t_mod1$fit), col = "red", lwd = 1) +
      ggtitle("Trend Model 1") +
      ylab("sales_em") +
      xlab("Time") 

lsales_em <- log(sales_em)
t_mod2 <- tslm(lsales_em ~ trend)
plot2 <- ggplot() + geom_line(data = as.data.frame(sales_em), aes(x = t, y = lsales_em[1:335])) +
      geom_line(data = as.data.frame(t_mod2$fit), aes(x = t, y = t_mod2$fit), col = "red", lwd = 1) +
      ggtitle("Trend Model 2") +
      ylab("ln(sales_em)") +
      xlab("Time")

grid.arrange(plot1, plot2, nrow = 2)

#afterwards plot with decomposed trend

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
#draw lines @ y = 9, and 10 for both graphs in comparison

truehist(t_mod1$res, col = "skyblue", xlab = "Residuals", ylab = "Fraction",
         main = "Trend Model 1 \nResiduals Histogram", xlim = c(-20000, 20000))
xr <- rnorm(1000000, mean(t_mod1$res), sd(t_mod1$res))
lines(density(xr), col = "red", lwd = 2)
lines(density(t_mod1$res), col = "darkblue", lwd = 2)
legend(x = "topright", legend = c("Density", "Normal Distribution"), 
       col = c("darkblue", "red"), lty = 1, lwd = 2, bty = "n")

truehist(t_mod2$res, col = "skyblue", xlab = "Residuals", ylab = "Fraction",
         main = "Trend Model 2 \nResiduals Histogram", xlim = c(-0.6, 0.6))
xr <- rnorm(1000000, mean(t_mod2$res), sd(t_mod2$res))
lines(density(xr), col = "red", lwd = 2)
lines(density(t_mod2$res), col = "darkblue", lwd = 2)
legend(x = "topright", legend = c("Density", "Normal Distribution"), 
       col = c("darkblue", "red"), lty = 1, lwd = 2, bty = "n")

summary(t_mod1)

summary(t_mod2)

AIC(t_mod1, t_mod2)
BIC(t_mod1, t_mod2)

fore1 <- forecast(t_mod2, h = 60)
plot(fore1, main = "Trend Model 2\n 5 year Ahead (h = 60) Forecast", xlab = "Time", ylab = "lsales_em")
legend(x = "bottomright", legend = c("80% Prediction Interval", "95% Prediction Interval"), 
       fill = c("lightsteelblue3", "gray87"), bty = "n", cex = 1)
legend(x = "bottom", "Point Forecast", col = "blue", lty = 1, 
       bty = "n", cex = 1)

#Seasons
ggsubseriesplot(sales_em)
ggseasonplot(sales_em)

s_mod1 <- tslm(lsales_em ~ season)
summary(s_mod1)
plot(s_mod1$fit, xlim = c(1992, 2020), ylab = "Seasonal Model 1", 
     main = "Seasons 2 to 12")

s_lsales_em <- stl(lsales_em, s.window = "periodic")
s_lsales_em <- s_lsales_em$time.series[,1]
plot(s_lsales_em, ylab = "Seasonal ln(sales_em)", main = "Seasonal Adjustment using\nSTL Decomposition")

s_mod0 <- tslm(lsales_em ~ season + 0) # diff w/ +0 is that adds y-int?
summary(s_mod0)

par(mfrow = c(2,1))
plot(s_mod1$coef, type = "l", main = "Seasonal Factors", xlab = "Season",
     ylab = "Seasonal Factors", lwd = 2, col = "forestgreen")
hist(s_mod1$res, main = "Histogram of Residuals", col = "steelblue2", 
     xlim = c(-2, 2), xlab = "Seasonal Model 1 Residuals")

par(mfrow = c(1,1))
full_mod <- tslm(lsales_em ~ trend + season) 
plot5 <- ggplot() + geom_point(aes(x = as.numeric(full_mod$fit), y = as.numeric(full_mod$res))) +
      geom_line(aes(x = as.numeric(full_mod$fit), y = 0), col = "red") +
      ggtitle("Full Model") +
      xlab("Fitted Values") +
      ylab("Residuals")
plot5

grid.arrange(plot5 + ylim(-0.4, 0.4), plot4, nrow = 2)

summary(full_mod)
AIC(t_mod1, t_mod2, s_mod1, s_mod0, full_mod)
BIC(t_mod1, t_mod2, s_mod1, s_mod0, full_mod)
accuracy(full_mod)
accuracy(t_mod2)
#later maybe ets or HoltWinters could improve the model?

fore2 <- forecast(full_mod, h = 60)
plot(fore2, main = "5 year Ahead \n(h = 60) Forecast", xlab = "Time", ylab = "lsales_em")
plot(fore2, main = "5 year Ahead \n(h = 60) Forecast", xlab = "Time", ylab = "lsales_em",
     xlim = c(2017, 2025))
legend(x = "bottomright", legend = c("80% Prediction Interval", "95% Prediction Interval"), 
       fill = c("lightsteelblue3", "gray87"), bty = "n", cex = 1.1)
legend(x = "bottomleft", "Point Forecast", col = "blue", lty = 1,
       lwd = 2, bty = "n", cex = 1.1)


#these are prediction intervals:
#"Forecasts are intervals for random variables and therefore do not fit within the 
## concept of confidence intervals, which concern parameters. You probably mean to refer 
### to prediction intervals rather than CIs"

#possible improvements: still not exactly additive, so many some smoothing/multiplicative measures
# stltest <- stl(lsales_em , s.window = 5) & check other metrics e.g X11
# plot(stltest$time.series[,1])

###
### NOTE: when change fullmod, and other; may need to go back to rmd and change notes 
###       based on new info


#KiM: add titles to all a) to whatever(explaining what is going on)
## also, add some explaining notes to final accuracy functions for full_mod and trend mod2
### also, check descriptions and make sure they correspond to updated info
#### also, make sure R Source section V. corresponds to r markdown output
#### also, in intro paragraph 2; expand more on e-shopping getting big in U.S(also source)
