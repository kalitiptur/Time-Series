# install.packages("systemfit")
require(systemfit)

data("Kmenta")
attach(Kmenta)
eqDemand <- consump~price + income
eqSupply <- consump~price + farmPrice + trend
eqSystem <- list(demand=eqDemand, supply=eqSupply)
fitols <- systemfit(eqSystem)
print(fitols)

fit3sls <- systemfit(eqSystem,method="3SLS",
                     inst=~income+farmPrice+trend,data=Kmenta)
summary(fit3sls)
fit2sls <- systemfit(eqSystem,method="2SLS",
                    inst=~income+farmPrice+trend,data=Kmenta)
hausman.systemfit(fit2sls,fit3sls)

# Import the regional heating and cooling degree days. Taken from American Gas Association
# https://www.aga.org/knowledgecenter/facts-and-data/annual-statistics/weekly-and-monthly-statistics
# Natural gas price and quantity data.
# citygate price from https://www.eia.gov/dnav/ng/hist/n3050ca3m.htm 
# consumption from https://www.eia.gov/dnav/ng/ng_cons_sum_dcu_SCA_m.htm 
# also use US NonFarm Payroll for income

calgasdata <- read.csv('C:/Users/bgilbert_a/Dropbox/Econometrics/gasdemand.csv',sep=",")
calgas <- ts(calgasdata, freq=12, start = 1989)
calgas <- window(calgas,c(2001,2),c(2016,12))

eqdem <- totalgas ~ ltotalgas + natgasprice + pacCDDm + pacHDDm + lpacCDDm + lpacHDDm + TIME + 
              + mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11
eqsup <- totalgas ~ ltotalgas + natgasprice + mtnCDDm + mtnHDDm + lmtnCDDm + lmtnHDDm + l2smtnCDD + l2smtnHDD + TIME + 
  + mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11
eqsys <- list(dem=eqdem, supply=eqsup)
fit3slsSLOPE <- systemfit(eqsys,method="3SLS",
                     inst=~pacCDDm + pacHDDm + lpacCDDm + lpacHDDm + lmtnCDDm + lmtnHDDm + 
                       l2smtnCDD + l2smtnHDD + mtnCDDm + mtnHDDm + TIME + 
                     mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11,data=calgas)
summary(fit3slsSLOPE)
fit2slsSLOPE <- systemfit(eqsys,method="2SLS",
                     inst=~pacCDDm + pacHDDm + lpacCDDm + lpacHDDm + lmtnCDDm + lmtnHDDm + 
                       l2smtnCDD + l2smtnHDD + mtnCDDm + mtnHDDm + TIME + 
                       mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11,data=calgas)
hausman.systemfit(fit2slsSLOPE,fit3slsSLOPE)


eqdem <- lntot ~ lntotlag + lnprice + pacCDDm + pacHDDm + lpacCDDm + lpacHDDm + TIME + 
  + mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11
eqsup <- lntot ~ lntotlag + lnprice + mtnCDDm + mtnHDDm + lmtnCDDm + lmtnHDDm + l2smtnCDD + l2smtnHDD + TIME + 
  + mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11
eqsys <- list(dem=eqdem, supply=eqsup)
fit3slsELAST <- systemfit(eqsys,method="3SLS",
                     inst=~pacCDDm + pacHDDm + lpacCDDm + lpacHDDm + lmtnCDDm + lmtnHDDm + 
                       l2smtnCDD + l2smtnHDD + mtnCDDm + mtnHDDm + TIME + 
                       mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11,data=calgas)
summary(fit3slsELAST)
fit2slsELAST <- systemfit(eqsys,method="2SLS",
                     inst=~pacCDDm + pacHDDm + lpacCDDm + lpacHDDm + lmtnCDDm + lmtnHDDm + 
                       l2smtnCDD + l2smtnHDD + mtnCDDm + mtnHDDm + TIME + 
                       mo_1 + mo_2 + mo_3 + mo_4 + mo_5 + mo_6 + mo_7 + mo_8 + mo_9 + mo_10 + mo_11,data=calgas)
hausman.systemfit(fit2slsELAST,fit3slsELAST)

