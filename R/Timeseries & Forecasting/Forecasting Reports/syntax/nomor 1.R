#library
library(ggplot2)
library(forecast)
library(tseries)

#Importing the data
data<-read.delim("clipboard")
Data<-ts(data$Close) #ubah tipe menjadi timeseries

#Creating the plot
autoplot(Data)+xlab("Waktu") +
  ylab("Penutupan(Close)") +
  ggtitle("Data Saham PT Telekomunikasi Indonesia") + geom_point()

##### Manual Function #####
holt_manual = function(y, Alpha, Gamma, start = y[1]){
  st_par = y
  st_par[1] = y[1] #inisiasi untuk s1
  bt_par = y
  bt_par[1] = y[2] - y[1] #inisiasi untuk b1
  n = length(y)+1 #Panjang data forecast lebih satu dari data asli
  forecast = array(NA, dim=c(n)) #Vektor kosong
  n_2 = length(y) #Panjang data error, st, dan bt sama dengan panjang data asli
  error = array(NA, dim=c(n_2)) #Vektor kosong
  for (i in 2:length(y)){ #Menggunakan fungsi perulangan untuk st dengan rumus yang sama pada modul dengan t dimulai dari 2 sampai dengan nilai panjang data asli
    st_par[i] = Alpha*y[i] +
      (1-Alpha)*(st_par[i-1]+bt_par[i-1])
    for (i in 2:length(y)){ #Menggunakan fungsi perulangan untuk bt dengan rumus yang sama pada modul dengan t dimulai dari 2 sampai dengan nilai panjang data asli
      bt_par[i] = Gamma*(st_par[i]-st_par[i-1]) +
        (1-Gamma)*bt_par[i-1]
    }
  }
  for (i in 3:n){ #Menggunakan fungsi perulangan untuk forecasting dengan t dimulai dari 3 sampai n yaitu panjang data asli plus 1
    forecast[i] = bt_par[i-1] + st_par[i-1]
  }
  for (i in 3:n_2){ #Menggunakan fungsi perulangan untuk menghitung error^2 dengan t dimulai dari 3 sampai dengan nilai panjang data asli
    error[i] = (y[i] - forecast[i])^2
  }
  max_ln = max(c(length(y), length(st_par),
                 length(forecast), length(bt_par), length(error)))
  #Mencari variabel dengan panjang data terpanjang
  df = data.frame(Data = c(y,rep(NA, max_ln -
                                   length(y))), #Menambahkan baris dengan nilai NA untuk menyamakan panjang data dengan panjang data terpanjang
                  St = c(st_par,rep(NA, max_ln -
                                      length(st_par))),
                  Bt = c(bt_par,rep(NA, max_ln -
                                      length(bt_par))),
                  Forecast = c(forecast,rep(NA, max_ln
                                            - length(forecast))),
                  Error = c(error,rep(NA, max_ln -
                                        length(error))))
  df #Menampilkan hasil ramalan
}
Alpha = rep(seq(0.1,0.9, by = 0.1), times = 9) #Membuat vektor nilai alpha
Gamma = rep(seq(0.1,0.3, by = 0.1), each = 3) #Membuat vektor nilai gamma
RMSE = NA #Vektor kosong
for (i in seq_along(Alpha)){ #Fungsi perulangan untuk mencari RMSE dari kombinasi nilai alpha dan gamma
  for (i in seq_along(Gamma)){
    param = holt_manual(Data, Alpha[i], Gamma[i])
    RMSE[i] = sqrt(mean(param$Error,na.rm=TRUE))
  }
}
Tabel.RMSE.Holt = data.frame(Alpha, Gamma, RMSE)
#Membuat tabel RMSE
head(Tabel.RMSE.Holt)
tail(Tabel.RMSE.Holt)
Min.RMSE =
  Tabel.RMSE.Holt[which.min(Tabel.RMSE.Holt$RMSE),]
#Mencari kombinasi nilai alpha dan gamma dengan RMSE terkecil
Min.RMSE
best.holt = holt_manual(Data, 0.6, 0.2) #Kombinasi alpha dan gamma dengan RMSE terkecil
RMSE.best.holt = sqrt(mean(best.holt$Error,na.rm=TRUE))
result.holt = function(best.holt, RMSE.best.holt,
                       Alpha, Gamma){
  print(best.holt)
  cat("\n", "-----------------\n", "RMSE = ",
      RMSE.best.holt,
      "\n", "Alpha = ", Alpha,
      "\n", "Gamma = ", Gamma)
}
result.holt(best.holt, RMSE.best.holt, 0.6, 0.2) #Hasil

##### Plot Data #####
data.plot=ts(best.holt$Data)
plot(data.plot, type="l", col="red", lwd=2,
     xlab="Tahun", ylab="GDP", main="Plot Data Asli dan
Ramalan 2 Parameter Holt")
forecast.holt=ts(best.holt$Forecast)
lines(forecast.holt, col="blue", lwd=2)
legend("topleft", c("Asli", "Ramalan"), bty="n", lwd=2,
       col=c("red", "blue"))
