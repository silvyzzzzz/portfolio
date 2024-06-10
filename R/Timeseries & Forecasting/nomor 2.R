#Importing the data
data1<-read.delim("clipboard")
Data1<-ts(data1$Monthly.beer.production,freq=12) #ubah tipe menjadi timeseries

#Creating the plot
autoplot(Data1)+xlab("Month") +
  ylab("Monthly Beer Production") +
  ggtitle("Monthly Beer Production Data") + geom_point()

#langsung ke alpha beta gamma terbaik
TripleSESMultiplikatif<-HoltWinters(Data1,seasonal = c("multiplicative"))
TripleSESMultiplikatif

#Untuk 12 periode
prediksiMultiplikatif=forecast(TripleSESMultiplikatif,h=12)
prediksiMultiplikatif

#menghitung akurasi
accuracy(prediksiMultiplikatif)

#membuat plot
plot(prediksiMultiplikatif,col="red",lwd=1.0)
lines(prediksiMultiplikatif$fitted,col="blue",lty=2,lwd=2.0)
legend("topleft", c("Asli", "Ramalan"), bty="n", 
       lwd=c(1.0,2.0),lty=c(1,2), col=c("red", "blue"))


########coba additive#############
#langsung ke alpha beta gamma terbaik
TripleSESMultiplikatif<-HoltWinters(Data1,seasonal = c("additive"))
TripleSESMultiplikatif

#Untuk 12 periode
prediksiMultiplikatif=forecast(TripleSESMultiplikatif,h=12)
prediksiMultiplikatif

#menghitung akurasi
accuracy(prediksiMultiplikatif)
