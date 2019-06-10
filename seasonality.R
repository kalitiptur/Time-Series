library(fBasics)
library(forecast)
library(quantmod)
library(tseries)
library(fUnitRoots)
library(CADFtest)
library(AER)
library(astsa)


getwd()
setwd("C:\\Users\\prasadk\\Desktop\\KALI\\Mines\\Time Series Forecasting\\R Lab and data files")
getwd()

################ Fitting Seasonality with Milk Production #####################
# https://datamarket.com/data/set/22ox/monthly-milk-production-pounds-per-cow-jan-62-dec-75#!ds=22ox&display=line
milk<-read.csv('monthly-milk-production-pounds-p.csv',fileEncoding ="UTF-8-BOM")
head(milk)
Milk<-milk$Pounds

head(Milk)
plot(Milk, type = 'l', col = 'blue', lwd = 3)

ts.milk <- ts(na.omit(milk$Pounds))
head(ts.milk)
acf(ts.milk, lag.max = 50)
pacf(ts.milk)

### Differencing #####
#####  It should be stationary ####
plot(diff(diff(ts.milk,12)), col = 'blue')

milk_dff <- diff(diff(ts.milk,12))

acf(milk_dff)
pacf(milk_dff)





d=NULL
DD=NULL
d=1
DD=1

per=12
for(p in 1:1){
  for(q in 1:1){
    for(i in 1:3){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=Milk, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

model<- arima(x=Milk, order = c(0,1,0), seasonal = list(order=c(0,1,1), period=12))
summary(model)
plot(forecast(model))
forecast(model)

summary(sarima(Milk, 0,1,0,0,1,1,12))


######### Souvenir Shop ##########################
#https://datamarket.com/data/set/22mh/monthly-sales-for-a-souvenir-shop-on-the-wharf-at-a-beach-resort-town-in-queensland-australia-jan-1987-dec-1993#!ds=22mh&display=line

SUV<-read.csv('monthly-sales-for-a-souvenir-sho.csv', fileEncoding ="UTF-8-BOM")
head(SUV)
suv <- ts(na.omit(SUV$Sales))
        
library(astsa)
library(forecast)

head(suv)

par(mfrow=c(2,2))

plot(suv, main='Monthly sales for a souvenir shop', ylab='', col='blue', lwd=3)
plot(log(suv), main='Log-transorm of sales', ylab='', col='red', lwd=3)
plot(diff(log(suv)), main='Differenced Log-transorm of sales', ylab='', col='brown', lwd=3)
plot(diff(diff(log(suv)),12), main='Log-transorm without trend and seasonaliy', ylab='', col='green', lwd=3)

data<-diff(diff((log(suv)),12))
acf2(data, 50)

d=1
DD=1
per=12
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}


suv.model <- sarima(suv, 1,1,1,1,1,1,12)


suv.model<- arima(x=suv, order = c(1,1,1), seasonal = list(order=c(1,1,1), period=12))
suv.model
plot(forecast(suv.model))
summary(suv.model)
