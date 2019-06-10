library(quantmod)
library(forecast)
library(fBasics)
library(CADFtest)
library(urca)
# install.packages("sandwich")
library(sandwich)
# install.packages("lmtest")
library(lmtest)
library(nlme)
# install.packages("MTS")
library(MTS)
library(car)
# install.packages("vars")
library(vars)

## AAA
getSymbols("AAA", src = "FRED")
head(AAA)

## BBB
getSymbols("BAA", src = "FRED")
head(BAA)

length(AAA)
length(BAA)


aaa <- ts(AAA$AAA, freq = 12, start = 1919)
head(aaa)
tail(aaa)

bbb <- ts(BAA$BAA, freq = 12, start = 1919)
head(bbb)



ybond <- cbind(aaa, bbb)
plot(ybond)
ts.plot( ybond[,1], ybond[,2], gpars = list(xlab = "Year", ylab = "Yield" ), main = "Moody's Corporate Bond Yield", lty = c(1:1), col = c( "blue", "green" ), lwd = 2)
legend("topright", legend = c("AAA", "BBB" ), col = c( "blue", "green" ), lty = 1) 

##### COINTEGRATION TEST #######
coint1 <- ca.po(ybond,demean=c("const"),type="Pu",lag="short")
summary(coint1)

coint2 <- ca.po(ybond,demean=c("const"),type="Pz",lag="short")
summary(coint2)

#Johansen test
j.coint <- ca.jo(ybond, type = "trace", ecdet = "trend")
summary(j.coint)

##### Regression Bond AAA on BBB ########

 regbond <- lm(bbb ~ aaa)
 summary(regbond)
 coeff <- coefficients(regbond)
 coeff
# equation of the Spread
 eq = paste0("BBBt = ", round(coeff[1],4) , " - ", round(coeff[2],4), " *AAAt")
 eq
 spread <- regbond$residuals
 
 plot(spread, type = 'l', main = eq, col = 'blue')
 abline(h = mean(spread), col="red", lwd=1, lty=2)
 adf.test(regbond$residuals)
 
 dif_reg <- lm( diff(bbb) ~ diff(aaa))
 summary(dif_reg)
 
 ################# Regress using the log values #######
 
 reglbnd <- lm(log(bbb) ~ log(aaa))
 summary(reglbnd)
###############################################################
 
 std_dev <- sd(spread)
 std_dev 
 
 ########## Hedge Ratio ###############
 #the hedge ratio as H = ??(XtN/YtN)
 #with tN the last day of the cointegration period.
 hedge_r <- sd_sprd * ((BAA$BAA[c(1198:1198)]) / (AAA$AAA[c(1198:1198)] ))

 
 
 