
install.packages('kableExtra')

library(kableExtra)
library(knitr)
data <- read.csv('C:\\Users\\Acer\\Downloads\\df_cleaned (1).csv')

vars <- c("Kelamin", "Kota.Kab", "L..Num", "L..Baca.B..Indo", "L..Baca.B..ING", "L..Sains", "Nilai.Prestasi")
str(data[, c(vars, "Pilihan")])

summary(data)
data<-data[,-1]

# Convert the character vector to a factor
data$Kelamin <- as.factor(data$Kelamin)
data$Kota.Kab <- as.factor(data$Kota.Kab)
data$Pilihan <- as.factor(data$Pilihan)

# a simple split
set.seed(2024)
install.packages('caTools')
library(caTools)
split <- sample.split(data$Pilihan, SplitRatio = 0.75)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

table(train_data$Pilihan)

########## CLASIFICATION
install.packages('C50')
library(C50)
tree_mod <- C5.0(x = train_data[, vars], y = train_data$Pilihan)

tree_boost <- C5.0(x = train_data[, vars], y = train_data$Pilihan, trials = 100)

library(partykit)
des_tree <- ctree(Pilihan~Kelamin+Kota.Kab+L..Num+L..Baca.B..Indo+L..Baca.B..ING+L..Sains+Nilai.Prestasi, data = train_data)

#### PREDICTIONS
predictions<-predict(tree_boost, newdata = test_data[, vars])

pred<-predict(tree_mod, newdata=test_data[,vars])

p<-predict(des_tree,newdata = test_data[,vars])

library(caret)
confusionMatrix(data=predictions,reference=test_data$Pilihan)

confusionMatrix(data=pred,reference=test_data$Pilihan)

confusionMatrix(data=p,reference=test_data$Pilihan)
