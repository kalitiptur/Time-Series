library(tseries)
data("USeconomic")
library(vars)

US.var <- vars::VAR(cbind(GNP, M1), p = 3, type = "trend")
coef(US.var)

US.var
acf(resid(US.var)[,1])
acf(resid(US.var)[,2])


US.pred <- predict(US.var, n.ahead = 4)
#display prediction values
US.pred


GNP.pred <- ts(US.pred$fcst$GNP[,1], st = 1988, fr = 4)
M1.pred <- ts(US.pred$fcst$M1[,1], st = 1988, fr = 4)

ts.plot(cbind(window(GNP, start = 1981), GNP.pred), lty = 1:2, col = 'blue')
ts.plot(cbind(window(M1, start = 1981), M1.pred), lty = 1:2, col = 'blue')

