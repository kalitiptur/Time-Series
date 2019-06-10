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
head(BAA)r

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

##### Regression Bond AAA on BBB ########

 regbond <- lm(bbb ~ aaa)

 boxplot(bbb ~ aaa, col = 'blue')
 summary(regbond)
 coeff <- coefficients(regbond)
 coeff
# equation of the Spread
 eq = paste0("BBBt = ", round(coeff[1],4) , " - ", round(coeff[2],4), " *AAAt")
 eq
 spread <- regbond$residuals
 
 plot(spread, type = 'l', main = eq, col = 'blue' )
 abline(h = mean(spread), col="red", lwd=3, lty=2)
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
 hedge_r

 ###########Calculate 2*SD #########################
 ########
 
 ##################### Trading Rules ####################
x = last(spread)
x
  if (last(spread) >= 2*std_dev )
  {
    paste("Go short BBB and long AAA")
  }    
  if ( last(spread) <= - 2*std_dev)
  {
    paste("Go long BBB and short AAA")
  }
 
 if ( last(spread) = 0)
 {
   paste("Lock this profit")
 }
 
 if ( last(spread) >= 4*std_dev || last(spread) <= -4*std_dev )
 {
   paste("Book the loss")
 }
 
 #################### VECM ###########################
 #Johansen test
 j.coint <- ca.jo(ybond, type = "trace", ecdet = "trend")
 summary(j.coint)
 # seems like 1 cointegrating vector with trend
 
 # coefficients of the vecm
 coeff.jo <- cajools(j.coint)
 summary(coeff.jo)
 graphics.off()
 par("mar")
 par(mar=c(2,2,2,2))
 dev.off()
 #plot residuals from each Equation
 plotres(j.coint)
 
 ############################# Estimating VAR #####################33
 VARselect(ybond,lag.max=15,type="trend")
 # If it is AIC & FPE then it is 4 
 
 # Estimate the VAR
 varogd <- VAR(ybond, lag.max = 14, type = "trend" , ic = "FPE")
 plot(varogd)
 summary(varogd)

 
 # transform vecm to its level-var representation
 # r=1 for rank of cointegrating vector matrix
 vecvar1 = vec2var(j.coint,r=1) 
 vecvar1

 ########## forecast #########33
bond_p <- predict(vecvar1, n.ahead = 12)
plot(bond_p)

plot(irf(vecvar1,n.ahead=12))

########### Different levels #############


########## Structural Breaks ############

bondstrbrk <- breakpoints(bbb ~ aaa)
summary(bondstrbrk)     

######## confidence Interval #######3
ci_bondbrk <- confint(bondstrbrk)

breakpoints(bondstrbrk)
coef(bondstrbrk, breaks = 4)

plot(bondstrbrk)

brg0 <- lm(bbb ~ aaa)
brg1 <- lm(bbb ~ 0 + aaa + breakfactor(bondstrbrk, breaks = 4))
brg2 <- lm(bbb ~ 0 + aaa + breakfactor(bondstrbrk, breaks = 4)/ aaa)

summary(brg1)
summary(brg2)

plot(bbb, col = 'blue')

lines(ts(fitted(brg0), start = 1919, freq = 12), col = 2)
lines(ts(fitted(brg1), start = 1919, freq = 12), col = 3)
lines(ts(fitted(brg2), start = 1919, freq = 12), col = 6)
lines(bondstrbrk, breaks = 4)
lines(ci_bondbrk)

