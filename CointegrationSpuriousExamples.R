# Multivariate examples:
### Cointegration

# some packages we may use:
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
boil=ts(MCOILBRENTEU$MCOILBRENTEU,freq=12,start=1987+(4/12), end = 2017+(5/12))
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

length(oil)
length(coff)
head(oil)
head(coff)
length(boil)
length(coff)
tail(coff)
tail(boil)

# Testing cointegration in the levels of drilling activity, and logged oil & gas prices
# visual inspection suggests cointegration may exist
godlev = cbind(gas,oilshort,wellshort)
MTSplot(godlev)

# Phillips-Ouliaris test:
# by default, first element in godlev is regressed on others (Pu)
# Pz uses residuals from regressing each on the other two, then
# regresses each variable on those residuals, and averages the test statistics
# default is demean="none", type="Pu", lag="short"
xc=ca.po(godlev,demean=c("const"),type="Pu",lag="short")
xc = ca.po(godlev)
summary(xc)
xregW = lm(wellshort ~ gas + oilshort)
xregG = lm(gas ~ wellshort + oilshort)
ts.plot(residuals(xregW))
ts.plot(residuals(xregG))
adf.test(residuals(xregG))
# results are sensitive to specification
# a weakness is that there are 3 variables, could be 2 different cointegrating relations
# e.g., oil & gas, oil & drilling, OR oil & drilling in one way, then 
# oil+gas+drilling in another way

# Johansen test:
# tests rank of the matrix of cointegrating vectors
# ca.jo estimates the test and a vecm
xj = ca.jo(godlev,ecdet="trend")
summary(xj)
# seems like 1 cointegrating vector with trend for the three series

# repeat with logs of prices
godlog = cbind(log(gas),log(oilshort),wellshort)
MTSplot(godlog)
xcl=ca.po(godlog,demean=c("constant"),type="Pz",lag="short")
xcl = ca.po(godlog)
summary(xcl)

# Johansen test in logs
xjl = ca.jo(godlog,type="trace",ecdet="trend",K=2,spec="transitory",season=12)
summary(xjl)
# seems like 1 cointegrating vector with trend

# coefficients of the vecm
cajools(xjl)
# residuals from each equation in the vecm
plotres(xjl)

# transform vecm to its level-var representation
# r=1 for rank of cointegrating vector matrix
x = vec2var(xjl,r=1)
# forecast levels
xp = predict(x,n.ahead=24)
plot(xp)
plot(irf(x,n.ahead=24))

# test for cointegration with possible level shift at unknown time
bj = cajolst(godlog,K=2,trend=TRUE)
summary(bj)
plotres(bj)
bjc = cajools(bj)
summary(bjc)

# try Structural VECM/working with levels
godlog = cbind(log(oilshort),log(gas),wellshort)
# show series and summary stats
plot(godlog,xlab="")
summary(godlog)

VARselect(godlog,lag.max=13,type="both")
# optimal lags is 2. should really test each series for trend/drift unit roots
# However, residuals are autocorrelated when p=2. Add lags.
# Could do all of this for lags 1-5 and see which is best
VARur = VAR(godlog,p=5,type="both")
summary(VARur)
plot(VARur)
# diagnostics for VARur:
normality.test(VARur)
# rejects normality test for both skewness and kurtosis!
arch.test(VARur,lags.multi=5)
plot(arch.test(VARur,lags.multi=5))
# significant ARCH effects, could explain normality failure
serial.test(VARur,lags.pt = 16,type="PT.adjusted")
# no residual serial correlation when p=5 which is good.
plot(serial.test(VARur,lags.pt = 16,type="PT.adjusted"))
stability(VARur)
plot(stability(VARur))

# Let's use the VAR(5) in (log) levels as a basis for the cointegration analysis
# estimate Johansen test plus VECM in logged prices and well levels:
vecmgod = ca.jo(godlog,type="trace",ecdet="trend",K=5,spec="transitory")
# can add season=12 for seasonal components
summary(vecmgod)
# no cointegration with 5 lags. But there is with fewer lags.
vecmgod = ca.jo(godlog,type="trace",ecdet="trend",K=3,spec="transitory")
summary(vecmgod)
# if we think the system has prices determining wells, normalize cointegrating
# vector relative to wells
vecmgod = ca.jo(cbind(wellshort,log(gas),log(oilshort)),type="trace",ecdet="trend",K=3,spec="transitory")
summary(vecmgod)
vecmgod2 = cajorls(vecmgod,r=1)
vecmgod2
vecmgodl = vec2var(vecmgod,r=1)
plot(predict(vecmgodl,n.ahead=48))
plot(irf(vecmgodl,n.ahead=24,ortho=T))
plot(irf(vecmgodl,n.ahead=24,ortho=T,impulse = "log.oilshort.",response="wellshort"))
plot(fevd(vecmgodl,n.ahead=24))


# New example
# are Brent and LNG cointegrated?
lngoil = cbind(lng,boil)
MTSplot(lngoil)
plot(boil,lng)

# plots of the series on the same graph with different axes
par(mar=c(5,4,4,5)+0.1)
plot(boil)
par(new=T)
plot(lng,axes=FALSE,ylab="",col="red")
mtext("lng",side=4,line=2.5,col="red")
axis(side=4,col="red",col.axis="red")

par(mar=c(5,4,4,5)+0.1)
plot(log(boil))
par(new=T)
plot(log(lng),axes=FALSE,ylab="",col="red")
mtext("lng",side=4,line=2.5,col="red")
axis(side=4,col="red",col.axis="red")

# cointegration tests of the levels
lngoil.co = ca.po(lngoil,demean="constant",type="Pz",lag="short")
lngoil.co = ca.po(lngoil)
summary(lngoil.co)
xreg = lm(lng~boil)
ts.plot(residuals(xreg))
# Johansen test
xj = ca.jo(lngoil,ecdet="const")
summary(xj)


# now in logs
lngoil.log = cbind(log(lng),log(boil))
MTSplot(lngoil.log)
plot(log(boil),log(lng))
plot(diff(log(boil)),diff(log(lng)))

# cointegration test of logs
tsdisplay(residuals(lm(log(boil)~log(lng))))
lngoil.log.co = ca.po(lngoil.log,demean="const",type="Pu",lag="long")
summary(lngoil.log.co)
# may be cointegrated, but residuals are still highly autocorrelated
lngoil.log.reg = lm(log(lng) ~ log(boil))
tsdisplay(residuals(lngoil.log.reg))
# Johansen test
xj = ca.jo(lngoil.log,ecdet="const",spec="transitory")
summary(xj)

# coefficients of the vecm
cajools(xj)
# residuals from each equation in the vecm
plotres(xj)
# transform vecm to its level-var representation
# r=1 for rank of cointegrating vector matrix
x = vec2var(xj,r=1)
# forecast levels
xp = predict(x,n.ahead=24)
plot(xp)
plot(irf(x,n.ahead=24))


# explore more cointegration vs spurious regression stuff with energy/commodities

# look at oil and drilling activity, in levels and logs
par(mar=c(5,4,4,5)+0.1)
plot(oil)
par(new=T)
plot(wells,axes=FALSE,ylab="",col="red")
mtext("wells",side=4,line=2.5,col="red")
axis(side=4,col="red",col.axis="red")

par(mar=c(5,4,4,5)+0.1)
plot(log(oil))
par(new=T)
plot(log(wells),axes=FALSE,ylab="",col="red")
mtext("log(wells)",side=4,line=2.5,col="red")
axis(side=4,col="red",col.axis="red")


# consider the global coffee price vs brent oil
# both are probably random walks

reg2 = lm(coff ~ boil)
# very high t-stat, pretty good R-squared
summary(reg2)
# but residuals look non-stationary
ts.plot(residuals(reg2))
tsoilcoff = cbind(boil,coff)
ts.plot(tsoilcoff)
# test for cointegration formally
x2 = ca.po(tsoilcoff,demean=c("constant"),type="Pu",lag="long")
summary(x2)
# fail to reject the null of a unit root in the residual
# need to regress changes on changes
reg2d = lm(diff(coff) ~ diff(boil))
summary(reg2d)
# fairly low t-stat, R-squared almost zero
plot(diff(boil),diff(coff))
# coffee-brent oil relationship was spurious in levels

# fish price index vs brent oil
ts.plot(slm)
# unit root test on fish price index 
# fail to reject null of unit root
ar(na.omit(diff(slm)))
CADFtest(slm,max.lag.y=22,type="trend")
# plot the two series in levels, some correlation appears
plot(boil,slm)
reg3 = lm(slm ~ boil)
summary(reg3)
# high t-stats and R-squared
ts.plot(residuals(reg3))
tsfish = cbind(boil,slm)
MTSplot(tsfish)
# cointegration test shows they are NOT cointegrated
x3 = ca.po(tsfish,demean=c("constant"),type="Pu",lag="long")
summary(x3)
# need to look in changes
reg3d = lm(diff(slm) ~ diff(boil))
summary(reg3d)
# a true relationship still exists - oil is a major input to fishing
plot(diff(boil),diff(slm))

# coffee and LNG - another spurious relationship that seems to be good in levels
reg4 = lm(log(coff) ~ log(lng))
summary(reg4)
ts.plot(residuals(reg4))
tsmore = cbind(log(lng),log(coff))
ts.plot(tsmore)
x4 = ca.po(tsmore,demean="constant",type="Pu",lag="long")
summary(x4)
reg4d = lm(diff(log(coff)) ~ diff(log(lng)))
summary(reg4d)
tsdisplay(residuals(reg4d))
plot(diff(log(lng)),diff(log(coff)))


# what about wells vs. oil relationship
# not cointegrated, and no contemporaneous relationship in changes
CADFtest(log(oil),type="drift",max.lag.y=4)
reg5 = lm(log(wells) ~ log(oil))
summary(reg5)
ts.plot(residuals(reg5))
tswells = cbind(log(oil),log(wells))
MTSplot(tswells)
x=ca.po(tswells,demean=c("constant"),type="Pz",lag="long")
summary(x)
reg5d = lm(diff(log(wells)) ~ diff(log(oil)))
summary(reg5d)
plot(diff(log(oil)),diff(log(wells)))
tsdisplay(residuals(reg5d))
m5 = arima(diff(log(wells)),order=c(4,0,0),xreg=diff(log(oil)),include.mean=F)
summary(m5)
tsdisplay(residuals(m5))
tsdiag(m5,gof=25)