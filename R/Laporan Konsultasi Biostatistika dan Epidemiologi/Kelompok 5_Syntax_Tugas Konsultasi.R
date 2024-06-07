#install and load readxl package
install.packages('readxl')
library("car")
library(readxl)
data <- read_excel("C:\\Users\\DELL\\Downloads\\Data Biostat.xlsx")
data

########Analisis Data Kelompok vs Perlakuan########
#ANALISIS DESKRIPTIF
desc_data <- data.frame(Kriteria=character(), Kelompok=numeric(), Min = numeric(),"1st Qu." = numeric(), Median = numeric(), Mean = numeric(), "3rd Qu."=numeric(), Max = numeric(),stringsAsFactors = FALSE)
#Tekanan darah Sistolik
for (kelompok in unique(data$KELOMPOK)) {
  subset_data <- data$SISTOL[data$KELOMPOK == kelompok]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "Sistolik", 
                                           Kelompok = kelompok, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
#Tekanan darah Diastolik
for (kelompok in unique(data$KELOMPOK)) {
  subset_data <- data$SISTOL[data$KELOMPOK == kelompok]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "Diastolik", 
                                           Kelompok = kelompok, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
#MAP
for (kelompok in unique(data$KELOMPOK)) {
  subset_data <- data$SISTOL[data$KELOMPOK == kelompok]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "MAP", 
                                           Kelompok = kelompok, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
#Heart Rate
for (kelompok in unique(data$KELOMPOK)) {
  subset_data <- data$SISTOL[data$KELOMPOK == kelompok]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "Heart Rate", 
                                           Kelompok = kelompok, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
rownames(desc_data) <- NULL
print(desc_data)


#UJI NORMALITAS 
hasil_uji <- data.frame(Kriteria=character(),Kelompok = character(), P_Value = numeric(), stringsAsFactors = FALSE) 
#Tekanan darah Sistolik 
for (kelompok in unique(data$KELOMPOK)) { 
  subset_data <- data$SISTOL[data$KELOMPOK == kelompok] 
  shapiro_test_result <- shapiro.test(subset_data) 
  # Menambahkan hasil uji ke dataframe 
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "Sistol", Kelompok = kelompok, P_Value = shapiro_test_result$p.value)) 
} 
#Tekanan darah diastolik 
for (kelompok in unique(data$KELOMPOK)) { 
  subset_data <- data$DIASTOL[data$KELOMPOK == kelompok] 
  shapiro_test_result <- shapiro.test(subset_data) 
  # Menambahkan hasil uji ke dataframe 
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "Diastol", Kelompok = kelompok, P_Value = shapiro_test_result$p.value)) 
} 
#MAP 
for (kelompok in unique(data$KELOMPOK)) { 
  subset_data <- data$MAP[data$KELOMPOK == kelompok] 
  shapiro_test_result <- shapiro.test(subset_data) 
  # Menambahkan hasil uji ke dataframe 
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "MAP", Kelompok = kelompok, P_Value = shapiro_test_result$p.value)) 
} 
#Heart Rate 
for (kelompok in unique(data$KELOMPOK)) { 
  subset_data <- data$`HEART RATE`[data$KELOMPOK == kelompok] 
  shapiro_test_result <- shapiro.test(subset_data) 
  # Menambahkan hasil uji ke dataframe 
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "Heart Rate", Kelompok = kelompok, P_Value = shapiro_test_result$p.value)) 
} 
# Menampilkan dataframe hasil uji 
print(hasil_uji) 


#UJI ASUMSI HOMOGENITAS DATA
typeof(data$KELOMPOK)
data$KELOMPOK <- as.character(data$KELOMPOK)
#Tekanan darah sistolik
levene_test_sistolik <- leveneTest(y = data$SISTOL, group = data$KELOMPOK, data = data)
print(levene_test_sistolik)  
#Tekanan darah diastolik
levene_test_diastolik <- leveneTest(y = data$DIASTOL, group = data$KELOMPOK, data = data)
print(levene_test_diastolik)
#MAP
levene_test_MAP<- leveneTest(y = data$MAP, group = data$KELOMPOK, data = data)
print(levene_test_MAP)
#Heart Rate
levene_test_HR<- leveneTest(y = data$`HEART RATE`, group = data$KELOMPOK, data = data)
print(levene_test_HR)


#UJI KRUSKAL WALLIS
uji_kruskal_wallis <- data.frame(Kriteria=character(), Chi_Square = numeric(), df = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
kw_sistol <- kruskal.test(SISTOL ~ KELOMPOK, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "Sistolik", Chi_Square = kw_sistol$statistic, df = kw_sistol$parameter, P_Value = kw_sistol$p.value))
kw_diastol <- kruskal.test(DIASTOL ~ KELOMPOK, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "Diastolik", Chi_Square = kw_diastol$statistic, df = kw_diastol$parameter, P_Value = kw_diastol$p.value))
kw_MAP <- kruskal.test(MAP ~ KELOMPOK, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "MAP", Chi_Square = kw_MAP$statistic, df = kw_MAP$parameter, P_Value = kw_MAP$p.value))
kw_HR <- kruskal.test(`HEART RATE`~ KELOMPOK, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "Heart Rate", Chi_Square = kw_HR$statistic, df = kw_HR$parameter, P_Value = kw_HR$p.value))
print(uji_kruskal_wallis)


########Analisis Data Kelompok vs Waktu########
#ANALISIS DESKRIPTIF
desc_data <- data.frame(Kriteria=character(), Waktu=numeric(), Min = numeric(),"1st Qu." = numeric(), Median = numeric(), Mean = numeric(), "3rd Qu."=numeric(), Max = numeric(),stringsAsFactors = FALSE)
#Tekanan darah Sistolik
for (waktu in unique(data$MENIT)) {
  subset_data <- data$SISTOL[data$MENIT == waktu]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "Sistolik", 
                                           Waktu = waktu, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
#Tekanan darah Diastolik
for (waktu in unique(data$MENIT)) {
  subset_data <- data$SISTOL[data$MENIT == waktu]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "Diastolik", 
                                           Waktu = waktu, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
#MAP
for (waktu in unique(data$MENIT)) {
  subset_data <- data$SISTOL[data$MENIT == waktu]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "MAP", 
                                           Waktu = waktu, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
#Heart Rate
for (waktu in unique(data$MENIT)) {
  subset_data <- data$SISTOL[data$MENIT == waktu]
  # Menambahkan hasil uji ke dataframe
  desc_data <- rbind(desc_data, data.frame(Kriteria = "Heart Rate", 
                                           Waktu = waktu, 
                                           Min = min(subset_data),
                                           '1st Qu' = quantile(subset_data,0.25), 
                                           Median = median(subset_data), 
                                           Mean = mean(subset_data), 
                                           '3rd Qu.'=quantile(subset_data,0.25), 
                                           Max = max(subset_data),
                                           stringsAsFactors = FALSE))
}
rownames(desc_data) <- NULL
print(desc_data)


#UJI NORMALITAS
hasil_uji <- data.frame(Kriteria=character(),Waktu = character(), P_Value = numeric(), stringsAsFactors = FALSE)
#Tekanan darah Sistolik
for (waktu in unique(data$MENIT)) {
  subset_data <- data$SISTOL[data$MENIT == waktu]
  shapiro_test_result <- shapiro.test(subset_data)
  # Menambahkan hasil uji ke dataframe
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "Sistol", Waktu = waktu, P_Value = shapiro_test_result$p.value))
}
#Tekanan darah diastolik
for (waktu in unique(data$MENIT)) {
  subset_data <- data$DIASTOL[data$MENIT == waktu]
  shapiro_test_result <- shapiro.test(subset_data)
  # Menambahkan hasil uji ke dataframe
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "Diastol", Waktu = waktu, P_Value = shapiro_test_result$p.value))
}
#MAP
for (waktu in unique(data$MENIT)) {
  subset_data <- data$MAP[data$MENIT == waktu]
  shapiro_test_result <- shapiro.test(subset_data)
  # Menambahkan hasil uji ke dataframe
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "MAP", Waktu = waktu, P_Value = shapiro_test_result$p.value))
}
#Heart Rate
for (waktu in unique(data$MENIT)) {
  subset_data <- data$`HEART RATE`[data$MENIT == waktu]
  shapiro_test_result <- shapiro.test(subset_data)
  # Menambahkan hasil uji ke dataframe
  hasil_uji <- rbind(hasil_uji, data.frame(Kriteria = "Heart Rate", Waktu = waktu, P_Value = shapiro_test_result$p.value))
}
# Menampilkan dataframe hasil uji
print(hasil_uji)


#UJI ASUMSI HOMOGENITAS DATA
typeof(data$MENIT)
data$MENIT <- as.character(data$MENIT)
#Tekanan darah sistolik
levene_test_sistolik <- leveneTest(y = data$SISTOL, group = data$MENIT, data = data)
print(levene_test_sistolik)  
#Tekanan darah diastolik
levene_test_diastolik <- leveneTest(y = data$DIASTOL, group = data$MENIT, data = data)
print(levene_test_diastolik)
#MAP
levene_test_MAP<- leveneTest(y = data$MAP, group = data$MENIT, data = data)
print(levene_test_MAP)
#Heart Rate
levene_test_HR<- leveneTest(y = data$`HEART RATE`, group = data$MENIT, data = data)
print(levene_test_HR)


#UJI KRUSKAL WALLIS
uji_kruskal_wallis <- data.frame(Kriteria=character(), Chi_Square = numeric(), df = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)
kw_sistol <- kruskal.test(SISTOL ~ MENIT, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "Sistolik", Chi_Square = kw_sistol$statistic, df = kw_sistol$parameter, P_Value = kw_sistol$p.value))
kw_diastol <- kruskal.test(DIASTOL ~ MENIT, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "Diastolik", Chi_Square = kw_diastol$statistic, df = kw_diastol$parameter, P_Value = kw_diastol$p.value))
kw_MAP <- kruskal.test(MAP ~ MENIT, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "MAP", Chi_Square = kw_MAP$statistic, df = kw_MAP$parameter, P_Value = kw_MAP$p.value))
kw_HR <- kruskal.test(`HEART RATE`~ MENIT, data = data)
uji_kruskal_wallis <- rbind(uji_kruskal_wallis, data.frame(Kriteria = "Heart Rate", Chi_Square = kw_HR$statistic, df = kw_HR$parameter, P_Value = kw_HR$p.value))
print(uji_kruskal_wallis)
