#Loading the required library
library(ggplot2)
library(forecast)
library(tseries)
library(lmtest)

#Importing the data
INDF<-read.delim('clipboard')
data<-ts(INDF$Close,freq=12)
View(data)

#Creating the plot
autoplot(data)+xlab("Waktu (Bulan)") + 
  ylab("Data close") + 
  ggtitle("Plot Data Asli") + geom_point()

#Stationarity Test
adf.test(data)

#Transforming data
##Checking the number of differences required
ndiffs(log(data),"adf")

##Diff 1
ddif1 = diff(data, differences=1)
adf.test(ddif1)
autoplot(ddif1)

#Diff 2 with Log-Trans
dtrans1 = diff(log(data), differences=1)
adf.test(dtrans1)
autoplot(dtrans1)

#Identifying the model
#Membuat plot ACF dan PACF
ggAcf(dtrans1,lag.max = 40) + ggtitle("ACF")
ggPacf(dtrans1,lag.max = 40) + ggtitle("PACF")
##ARIMA(1,1,1)##

#Underfitting
c111<-Arima(data,order=c(1,1,1),include.constant = T, lambda = 0)
coeftest(c111)
c110<-Arima(data,order=c(1,1,0),include.constant = T, lambda = 0)
coeftest(c110)
c011<-Arima(data,order=c(0,1,1),include.constant = T, lambda = 0)
coeftest(c011)

tc111<-Arima(data,order=c(1,1,1),include.constant = F, lambda = 0)
coeftest(tc111)
tc110<-Arima(data,order=c(1,1,0),include.constant = F, lambda = 0)
coeftest(tc110)
tc011<-Arima(data,order=c(0,1,1),include.constant = F, lambda = 0)
coeftest(tc011)

#Diagnostic Checking
Box.test(tc110$residuals,type="Ljung") #uji autokorelasi
Box.test((tc110$residuals)^2,type="Ljung") #uji homoskedastik
jarque.bera.test(tc110$residuals) #uji normalitas
checkresiduals(tc110)

Box.test(tc011$residuals,type="Ljung") #uji autokorelasi
Box.test((tc011$residuals)^2,type="Ljung") #uji homoskedastik
jarque.bera.test(tc011$residuals) #uji normalitas
checkresiduals(tc011)

#Best model selection
mod_tc110 = data.frame(Model = "tc110", LogLik = logLik(tc110), AIC = AIC(tc110), BIC = BIC(tc110))
mod_tc011 = data.frame(Model = "tc011", LogLik = logLik(tc011), AIC = AIC(tc011), BIC = BIC(tc011))

model_selection = rbind(mod_tc110, mod_tc011)
model_selection

#Penulisan Model
tc110

#Best model
##Plot##
autoplot(tc110$x, col="darkblue") + 
  autolayer(fitted(tc110), series = "Data Ramalan") + 
  ylab("Data Close") + 
  ggtitle("Plot Data Asli vs Ramalan")
##Forecast and Accuracy##
accuracy(tc110)
forecast(tc110,1)
autoplot(forecast(tc110,1))
