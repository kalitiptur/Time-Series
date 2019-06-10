
# Use a variety of R tools to 
# investigate basic ARMA/ARIMA models.

#
### Some packages we will need
#

# install.packages("forecast")
require(forecast)
# install.packages("quantmod")
require(quantmod)

# investigate the properties of the unemployment rate
getSymbols("UNRATE",src="FRED")
head(UNRATE)
# convert the 'xts' formatting to numeric, taking the first column of data from UNRATE
rate <- as.numeric(UNRATE[,1])

# some detective work on the temporal behavior
ts.plot(rate)
acf(rate)
# the unemployment rate doesn't strictly conform to our expected pattern of 
# stationary/nonstationary (acf declining, but not exponentially)

# treat unemployment rate as nonstationary for the time being
# calculate the first difference (monthly change in unemployment) to get 
# a stationary series
ratechng = diff(rate)
ts.plot(ratechng)

# detective work on ARMA orders
acf(ratechng)
# MA6? MA12?
pacf(ratechng)
# AR5? AR12?

m1 <- ar(ratechng,order.max=10)
m1$order
m1
# explore some ARMA models
m2chng <- arima(ratechng,order=c(5,0,6))
m2chng
tsdiag(m2chng,gof=36)

# constrain insignificant coefficients to be zero
# check if the constrained model still has no 
# residual autocorrelation, check if it has lower AIC

# define a vector that constrains the insignificant coefficients
# first five entries are AR, next 6 are MA, final one is intercept
c1 <- c(0,0,NA,0,0,0,0,NA,0,0,0,0)
m2chnga <- arima(ratechng,order=c(5,0,6),fixed=c1)
m2chnga
tsdiag(m2chnga,gof=36)
Box.test(m2chnga$residuals,lag=12)
#adjust degrees of freedom = #lags - minus # parameters (12-2=10)
pv <- 1-pchisq(113.77,10)
pv
# in the forecast package
tsdisplay(residuals(m2chnga),main='Some results I found')
# The restricted model "m2a" is not as good as "m2" - residual autocorrelation is still there

# What if we treat the unemployment rate as stationary, try some ARMA models
# detective work
acf(rate)
pacf(rate)
m1 <- ar(rate,order.max=15)  ## AR order selection using AIC
m1$order
m2 <- arima(rate,order=c(13,0,0))
m2
tsdiag(m2,gof=36)
### Model refinement - some coefficients are not statistically significant
c1 <- c(NA,NA,0,0,0,NA,0,0,0,NA,NA,NA,NA,NA)
m3 <- arima(rate,order=c(13,0,0),fixed=c1)
m3
tsdiag(m3,gof=36)
#### The AIC falls to -382.38 indicating that the model fits better with 
#### those parameters set to zero (as given in c1).
#### Still has residual autocorrelation, however


#### We can also look at the roots of the lag polynomial of each model
# First model with changes in unemployment rate
lagpol_m2chng = c(1,-m2chng$coef[1:5])
roots = polyroot(lagpol_m2chng)
roots
Mod(roots)
# 5 roots, 1 complex conjugate pair (2 imaginary), 1 cycle, 3 real roots
# All greater than one in modulus (characteristic roots are less than 1)

# Refined model of changes in unemployment rate
lagpol_m2chnga = c(1,-m2chnga$coef[1:5])
roots = polyroot(lagpol_m2chnga)
roots
Mod(roots)
# 3 roots (4th and 5th lag coefficients are zero), 
#1 complex conjugate pair (2 imaginary), 1 cycle, 1 real root
# All greater than one in modulus (characteristic roots are less than 1)
# notice that the moduli are all equal- there is really only one 
# AR coefficient since 1st and second lag coefficients also zero

# First model with unemployment rate itself
lagpol_m2 = c(1,-m2$coef[1:5])
roots = polyroot(lagpol_m2)
roots
Mod(roots)
# 13 roots, 6 complex conjugate pairs (12 imaginary), 1 real root
# All are greater than one in modulus, but one of them (1.026606) is REALLY close to 1 - nonstationary

# Refined model of changes in unemployment rate
lagpol_m3 = c(1,-m3$coef[1:13])
roots = polyroot(lagpol_m3)
roots
Mod(roots)
# same. 12 complex roots/6 pairs, 1 real. One root very very close to 1 (1.026346)


############
# Investigate the properties of daily oil price returns

getSymbols("DCOILWTICO",src="FRED")
oil <- as.numeric(DCOILWTICO[,1])
oilrt <- na.omit(diff(log(oil)))
# detective work
acf(oilrt)
pacf(oilrt)
# some candidates:
#MA2, MA5? AR8?
moil = arima(oilrt,order=c(8,0,5))
moil
tsdiag(moil,gof=36)
# seems to get rid of residual autocorrelation. 
# can we do better (lower AIC, still no residual autocorrelation) by dropping some terms?
m2oil = arima(oilrt,order=c(7,0,4))
m2oil
tsdiag(m2oil,gof=36)
# higher AIC, now we have possible residual autocorrelation - overall worse model

# Try the automatic model selection from the "forecast" package:
auto.arima(oilrt)
# gives ARMA(1,2)
m3oil = arima(oilrt,order=c(1,0,2))
m3oil
tsdiag(m3oil,gof=36)
# higher AIC, now we have serious residual autocorrelation - overall worse model
# Be careful of how particular packages are doing their jobs!

# let's take the ARMA(8,5)
### Model refinement - some coefficients are not statistically significant
c1 <- c(0,NA,0,0,0,0,0,0,0,NA,0,0,0,0)
moil_a <- arima(oilrt,order=c(8,0,5),fixed=c1)
moil_a
#### The AIC is larger, indicating that the model does not fit as well
#### those parameters set to zero (as given in c1).
tsdiag(moil_a,gof=36)
tsdisplay(residuals(moil_a),main='Results')

Box.test(moil_a$residuals,lag=20) # uses 20 df, but should have 20-2=18.
pv = 1-pchisq(68.351,18) # p-value is lower than before.
pv

cp = c(1,-moil$coef[1:8])
roots = polyroot(cp)
roots
Mod(roots)




# investigate the properties of the daily 3 p.m. gold per troy ounce 
# price in London Bullion market, US dollars
getSymbols("GOLDPMGBD228NLBM",src="FRED")
head(GOLDPMGBD228NLBM)

# FRED does not allow selecting specific dates, so let's pick a fixed window
# so that answers don't change every day
gold <- GOLDPMGBD228NLBM$GOLDPMGBD228NLBM
gold <- gold[paste("1968-04-01","2017-09-03",sep="/")]

# gold <- as.numeric(GOLDPMGBD228NLBM[,1])
# let's remove missing observations while converting (but harder to use date info)
gold <- data.frame(na.omit(gold)) 
ts.plot(gold) # nonstationary or breaks in the mean?

# Use AR to pick the optimal AR order (if only looking at AR models, ignoring MA)
m1 <- ar(gold,order.max=15) # AR 12 selected
m1
m2 <- arima(gold,order=c(12,0,0)) # without modeling breaks, looks nonstationary
m2

# calculate returns
goldrtn <- diff(log(gold[,1]))
ts.plot(goldrtn)
ar(goldrtn,order.max=15) # 12 lags selected
m1 <- arima(goldrtn,order=c(12,0,0)) 
m1
# only lags 1, 2, 3, 9 and 12 are significant. AIC -73661.8
tsdiag(m1,gof=36)
# important: R's arima() reports the mean as `intercept`
# need to actually calculate the intercept if important. will do below

# create a model that drops insignificant coefficients.
c1 <- c(NA,NA,NA,0,0,0,0,0,NA,0,0,NA,NA) # last entry is for intercept
m2 <- arima(goldrtn,order=c(12,0,0),fixed=c1) # AIC -73674.61 an improvement
m2
tsdiag(m2,gof=36)
tsdisplay(residuals(m2),main='AR Model With Lags 1-3, 9, 12')
Box.test(m2$residuals,lag=20) # uses 20 df, but should have 20-5=15.
pv = 1-pchisq(6.8204,15) # p-value is 0.96 - lower than before.

# auto.arima can handle nonstationary data by finding how much differencing is required.
auto.arima(gold) # ARIMA(1,1,2) with drift

m3 <- auto.arima(goldrtn) 
m3
# ARIMA(1,0,2) with non-zero mean, AIC -73647.4 so not as good as AR 12
tsdisplay(residuals(m3),main='ARIMA (1,0,2) Model Residuals')
# notice there are autocorrelations at 9 and 12
tsdiag(m3,gof=36)

# by default auto.arima uses approximations to AIC to estimate many models quickly
# and does not search over all models. May not give optimal model. 
# Try to get auto.arima to give me the AR12 which I know is better
auto.arima(goldrtn,max.p=12,max.order=100,seasonal=F,stepwise=F,trace=T,approximation=F) 
# looks like ARIMA (12,0,0) is better after all
# among the "smaller" models, (2,0,2) is the best
m4 <- arima(goldrtn,order=c(2,0,2))
m4
tsdiag(m4,gof=36)
tsdisplay(residuals(m4))

# Model 2 passes all the tests, but may be "overfitting"
# Other models have some serial correlations in the residuals

mean(goldrtn)
predict(m2,50) # Prediction
m2p = predict(m2,50)
names(m2p)
lcl = m2p$pred-1.96*m2p$se  # calculate lower 95% interval
ucl = m2p$pred+1.96*m2p$se # calculate upper 95% interval
cl <- cbind(lcl,m2p$pred,ucl)
print(cl)
plot(cl)
fcast <- forecast(m2,h=50)
plot(fcast,include=50)

# forecast with a holdout sample to see how well we did
fit_no_holds <- arima(goldrtn[-c(12366:12415)],order=c(12,0,0),fixed=c1)
fcast_no_holds <- forecast(fit_no_holds,h=50)
plot(fcast_no_holds,main=" ",include=50)
lines(ts(goldrtn))

# Notice that forecasting the gold PRICE (not return) is harder
# Not stationary
# variance of forecast explodes
# prices don't stay within confidence intervals of forecast.
goldp = as.numeric(na.omit(GOLDPMGBD228NLBM[,1]))
goldp = goldp[-c(12417:12423)]
fit_no_holds <- auto.arima(goldp[-c(12000:12416)])
fcast_no_holds <- forecast(fit_no_holds,h=417)
plot(fcast_no_holds,main=" ")
lines(ts(goldp))
plot(fcast_no_holds,main=" ",include=500)
lines(ts(goldp))

# If there are complex conjugate pairs, there is a cyclical component
# Calculate the average length of stochastic cycles
# suppose AR 2 was the right model
m5 <- arima(goldrtn,order=c(2,0,0))
m5
# calculate intercept
phi0 = (1-m5$coef[1]-m5$coef[2])*m5$coef[3]
# characteristic polynomial
cp = c(1,-m5$coef[1:2])
roots = polyroot(cp)
roots
Mod(roots)
k = 2*pi/acos(-0.580027/7.664846)

# try with AR 3
m6 <- arima(goldrtn,order=c(3,0,0))
m6
# calculate intercept
phi0 = (1-m6$coef[1]-m6$coef[2]-m6$coef[3])*m6$coef[4]
# characteristic polynomial
cp = c(1,-m6$coef[1:3])
roots = polyroot(cp)
roots
Mod(roots)
k = 2*pi/acos(-1.585240/3.39955) 

# cycle seems to be 3 or 4 days ??

# try with our optimal model m2 which is mostly AR(12)
cp = c(1,-m2$coef[1:12])
roots = polyroot(cp)
roots
# 12 roots, 10 are complex, 5 complex conjugate pairs, 5 cycles
Mod(roots)
k1 = 2*pi/acos(0.681234/1.370811)
k2 = 2*pi/acos(-1.155101/1.314331)
k3 = 2*pi/acos(-0.636385/1.273249)
k4 = 2*pi/acos(1.121955/1.319210)
k5 = 2*pi/acos(0.032886/1.311828)


# More practice with the monthly returns
gld = monthlyReturn(gold,type="log")
chartSeries(gld)
ar(gld) # AR 11
arima(gld,order=c(11,0,0)) # only lags 9 and 11 are significant
c1 = c(0,0,0,0,0,0,0,0,NA,0,NA,NA)
g1 = arima(gld,order=c(11,0,0),fixed=c1)
cp = c(1,-g1$coef[1:11])
roots = polyroot(cp)
roots
Mod(roots)
# 11 roots, 10 are complex, 5 complex conjugate pairs, 5 cycles
k1 = 2*pi/acos(0.9539401/1.159320)
k2 = 2*pi/acos(-1.0930546/1.145395)
k3 = 2*pi/acos(-0.7364244/1.183012)
k4 = 2*pi/acos(0.4618785/1.213804)
k5 = 2*pi/acos(-0.1567677/1.238747)







