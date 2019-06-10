# Multivariate examples:
### VAR

require(quantmod)
require(forecast)
require(fBasics)
require(CADFtest)
require(urca)
# install.packages("sandwich")
require(sandwich)
# install.packages("lmtest")
require(lmtest)
require(nlme)
# install.packages("MTS")
require(MTS)
require(car)
# install.packages("strucchange")
require(strucchange)
# install.packages("vars")
require(vars)

# WTI
getSymbols("MCOILWTICO",src="FRED")
# Oil & gas drilling index
getSymbols("IPN213111N",src="FRED")
# Henry Hub
getSymbols("MHHNGSP",src="FRED")

# Brent
getSymbols("MCOILBRENTEU",src="FRED")
# LNG Asia
getSymbols("PNGASJPUSDM",src="FRED")

# Additional commodities to illustrate spurious regression
# Global Coffee
getSymbols("PCOFFOTMUSDM",src="FRED")
# Global Fish
getSymbols("PSALMUSDM",src="FRED")


# need to define them all as time series objects
# and make sure they cover the same time span
oil=ts(MCOILWTICO$MCOILWTICO,freq=12,start=1986)
oil=ts(oil[c(73:length(oil))],freq=12,start=1992)
boil=ts(MCOILBRENTEU$MCOILBRENTEU,freq=12,start=1987+(4/12))
boil=ts(boil[c(57:length(boil))],freq=12,start=1992)
lng=ts(PNGASJPUSDM$PNGASJPUSDM,freq=12,start=1992)
coff = ts(PCOFFOTMUSDM$PCOFFOTMUSDM,freq=12,start=1980)
coff = ts(coff[c(145:length(coff))],freq=12,start=1992)
slm = ts(PSALMUSDM$PSALMUSDM,freq=12,start=1980)
slm = ts(slm[c(145:length(slm))],freq=12,start=1992)
wells = ts(IPN213111N$IPN213111N,freq=12,start=1972)
wells = ts(wells[c(241:length(wells))],freq=12,start=1992)
gas = ts(MHHNGSP$MHHNGSP,freq=12,start=1997)
oilshort =ts(MCOILWTICO$MCOILWTICO,freq=12,start=1986)
oilshort=ts(oilshort[c(133:length(oilshort))],freq=12,start=1997)
wellshort = ts(IPN213111N$IPN213111N,freq=12,start=1972)
wellshort = ts(wellshort[c(301:length(wellshort))],freq=12,start=1997)



# Example with Henry Hub, WTI, and drilling activity index

# calculate log differences
dgas = na.remove(diff(log(gas)))
doil = na.remove(diff(log(oilshort)))
dwell = na.remove(diff(wellshort))

# Estimating a VAR for oil & gas price returns
# use vars() package
og = cbind(doil,dgas)
# show series and summary statistics
plot(og,xlab="")
summary(og)

# select lag length
VARselect(og,lag.max=15,type="none")
# HQ and SC select 1 lag, FPE and AIC select 9 lags, same as below:
varog = VAR(og,lag.max=13,type="none",ic="FPE")
# type = trend, const, both, none
# lag.max= or p=
# can enter exogen= for exogenous variables
# can enter season=12 for seasonal frequency
# from VAR() output, can do coef, fevd, fitted, irf, Phi, logLik, plot, 
# predict, print, Psi, resid, summary, Acoef, Bcoef, BQ, causality,
# restrict, roots, 
# diagnostics: arch.test, normality.test, serial.test, stability
plot(varog)
summary(varog)

# Should go with smaller model if it gets rid of residual autocorrelation
varog1 = VAR(og,type="none",p=1)
plot(varog1)
# mostly, residual autocorrelation is nill except 9th lag in the gas equation.
# looking at the VAR(1) vs. VAR(9), the 8th and 9th lags in the gas equation have
# a lot of explanatory power. 9 months to move rigs and begin production?

# lets go with the VAR(9) for the sake of discussion.
# basic results
summary(varog)
summary(varog,equation="dgas")
plot(varog)
plot(varog,name="dgas")
roots(varog)
causality(varog, cause="doil")
# appears that oil returns Granger-cause gas returns
# diagnostics:
normality.test(varog)
# rejects multivariate normality test for kurtosis!
arch.test(varog,lags.multi=9)
plot(arch.test(varog,lags.multi=9))
# significant ARCH effects in oil equation, could explain kurtosis/failure of normality.
serial.test(varog,lags.pt = 16)
# no residual serial correlation, which is good.
plot(serial.test(varog)) # same output as plot(arch.test())
stability(varog)
plot(stability(varog))
# forecasting
varogp = predict(varog,n.ahead=48)
# 95% CI by default
plot(varogp)
# 90% CI by default, some better visualization tools: 
fanchart(varogp)
# impulse resonse functions. options ortho=T, cumulative=T or F
# can define impulse = "doil", response="dgas"
plot(irf(varog,n.ahead=24,ortho=FALSE))
plot(irf(varog,n.ahead=24,ortho=T))
plot(irf(varog,n.ahead=24,ortho=T,impulse = "doil",response="dgas"))
plot(fevd(varog,n.ahead=24))


# Calculate the covariance matrix of VAR(p) residuals:
Omega = summary(varog)$covres
# Factor it into upper/lower triangular. Recall R gives the upper triangular, when we want its transpose:
P = chol(Omega)
# check that products of lower triangular P and with its transpose return Omega:
t(P)%*%P

# could report in ADA' form
Dhalf = diag(P)
A = t(P/Dhalf)
# check that we get Omega back if we factored correctly:
A%*%diag(Dhalf)%*%diag(Dhalf)%*%t(A)
# report A:
A

# notice that the simple regression of residuals from gas equation on
# residuals from oil equation gives identical coefficient to the adjustment made in the A matrix;
# A matrix adjusts for their contemporaneous linear relationship
Areg = lm(residuals(varog$varresult$dgas) ~ residuals(varog$varresult$doil))
summary(Areg)


# Compare to structural VAR
# transform reduced form into structural
Avar = matrix(c(1,NA,0,1),nrow=2,ncol=2)
svarog = SVAR(varog,estmethod="direct",Amat=Avar,hessian=T)
# estimation method direct is MLE. hessian=T required for standard errors.
# coefficients of the structural matrix (contemporaneous coefficients)
svarog$A
# standard errors of the structural coefficients matrix
svarog$Ase
summary(svarog)
plot(irf(svarog,n.ahead=24,ortho=T))



#######
# Repeat the whole process with three-dimensional oil, gas, drilling VAR example

god = cbind(doil,dgas,dwell)
# show series and summary statistics
plot(god,xlab="")
summary(god)

VARselect(god,lag.max=13,type="none")
# FPE and AIC select 4 lags, same as below:
vargod = VAR(god,lag.max=13,type="none",ic="FPE")
# type = trend, const, both, none
# lag.max= or p=
# can enter exogen= for exogenous variables
# can enter season=12 for seasonal frequency
# from VAR() output, can do coef, fevd, fitted, irf, Phi, logLik, plot, 
# predict, print, Psi, resid, summary, Acoef, Bcoef, BQ, causality,
# restrict, roots, 
# diagnostics: arch.test, normality.test, serial.test, stability

# basic results
summary(vargod)
summary(vargod,equation="dwell")
plot(vargod)
plot(vargod,name="dwell")
roots(vargod)
causality(vargod, cause="doil")
# diagnostics:
normality.test(vargod)
# rejects normality test for both skewness and kurtosis!
arch.test(vargod,lags.multi=4)
plot(arch.test(vargod,lags.multi=4))
# significant ARCH effects, could explain failure of normality.
serial.test(vargod,lags.pt = 16)
# no residual serial correlation, which is good.
plot(serial.test(vargod))
stability(vargod)
plot(stability(vargod))
# forecasting
vargodp = predict(vargod,n.ahead=48)
# 95% CI by default
plot(vargodp)
# 90% CI by default, some better visualization tools: 
fanchart(vargodp)
# impulse resonse functions. options ortho=T, cumulative=T or F
# can define impulse = "doil", response="dwell"
plot(irf(vargod,n.ahead=24,ortho=T))
plot(irf(vargod,n.ahead=24,ortho=T,impulse = "doil",response="dwell"))
plot(fevd(vargod,n.ahead=24))


# Calculate the covariance matrix of VAR(p) residuals:
Omega = summary(vargod)$covres
# Factor it into upper/lower triangular. Recall R gives the upper triangular, when we want its transpose:
P = chol(Omega)
# check that products of lower triangular P and with its transpose return Omega:
t(P)%*%P

# could report in ADA' form
Dhalf = diag(P)
A = t(P/Dhalf)
# check that we get Omega back if we factored correctly:
A%*%diag(Dhalf)%*%diag(Dhalf)%*%t(A)
# report A:
A

# transform reduced form into structural
Avar = matrix(c(1,NA,NA,0,1,NA,0,0,1),nrow=3,ncol=3)
svargod = SVAR(vargod,estmethod="direct",Amat=Avar,hessian=T)
# estimation method direct is MLE. hessian=T required for standard errors.
# coefficients of A matrix
svargod$A
Ao = solve(svargod$A)
# compare
Ao
A
# standard errors of A matrix
svargod$Ase
summary(svargod)
plot(irf(svargod,n.ahead=24,ortho=T))



#### Tsay also has a package called MTS (multivariate time series)
#### Many of the same command names as vars() package, need to be careful
require(MTS)
# Using MTS package commands
# investigate lag order
# criteria differ on optimal lag order, could go with 1, 2, 4
# just for sake of illustration let's do 2
VARorder(god,maxp=12,output=T)
# estimate the VAR
vardrill = MTS::VAR(god,p=2,output=T,include.mean = T)
# calculate forecasts
drpred = VARpred(vardrill,h=100)

# one way to illustrate impulse response functions
# need to feed in coefficients and error covariance from estimated model
VARMAirf(vardrill$Phi,Sigma=vardrill$Sigma)
# two graphs are shown: one time response and cumulative
# notice bottom row is response of wells to gas, oil, and wells in that order
# wells are really the only thing that responds to anything
# respond more to oil than gas, but both responses take a couple months.

# refVAR() drops insignificant coefficients
# default is t-stat below 1 gets dropped.
# take estimated model and "refine" it
# AIC improves, many zero coefficients in the AR matrices
vard2 = refVAR(vardrill)
drpred2 = VARpred(vard2,h=100)
VARMAirf(vard2$Phi,Sigma=vard2$Sigma)


# I can take the gas price return forecast only
predgas = drpred$pred[c(1:100),1]
# I can plot each return (oil, then wells)
plot(ts(drpred$pred[c(1:100),2]))
plot(ts(drpred$pred[c(1:100),3]))
ts.plot(dgas)
lines(ts(predgas,start=2017+10/12,freq=12),col=3)
plot(ts(predgas,start=2017+10/12,freq=12),col=3)

