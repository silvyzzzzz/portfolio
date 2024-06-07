
########### DATA SMA
data_sma <- read.delim('clipboard')
unique(data_sma$Kota.Kab)

selected_cities <- c("Kab. Gunung Kidul", "Kab. Bantul", "Kab. Sleman","Kota Yogyakarta","Kab. Kulon Progo")
data_sma <- subset(data_sma, Kota.Kab %in% selected_cities)

any(is.na(data_sma))
# Drop rows with any null values in any column
data_sma <- na.omit(data_sma)

# Change all occurrences of 'old_value' to 'new_value'
data_sma$Kota.Kab[data_sma$Kota.Kab == "Kab. Gunung Kidul"] <- 'Gunung Kidul'
data_sma$Kota.Kab[data_sma$Kota.Kab == "Kab. Sleman"] <- 'Sleman'
data_sma$Kota.Kab[data_sma$Kota.Kab == "Kota Yogyakarta"] <- 'Yogyakarta'
data_sma$Kota.Kab[data_sma$Kota.Kab == "Kab. Bantul"] <- 'Bantul'
data_sma$Kota.Kab[data_sma$Kota.Kab == "Kab. Kulon Progo"] <- 'Kulon Progo'

# Convert the character vector to a factor
data_sma$Kelamin <- as.factor(data_sma$Kelamin)
data_sma$Kota.Kab <- as.factor(data_sma$Kota.Kab)
data_sma$Pilihan <- as.factor(data_sma$Pilihan)



######### DATA SMK
data_smk <- read.delim('clipboard')
unique(data_smk$Kota.Kab)

selected_cities1 <- c("Gunung Kidul", "Bantul", "Sleman","Yogyakarta","Kulon Progo")
data_smk <- subset(data_smk, Kota.Kab %in% selected_cities1)

any(is.na(data_smk))
# Drop rows with any null values in any column
data_smk <- na.omit(data_smk)

# Convert the character vector to a factor
data_smk$Kelamin <- as.factor(data_smk$Kelamin)
data_smk$Kota.Kab <- as.factor(data_smk$Kota.Kab)
data_smk$Pilihan <- as.factor(data_smk$Pilihan)

######################################################
data1 <- rbind(data_sma, data_smk)
######################################################

# Define breaks and labels for ranges
breaks <- c(0, 55, 70, 85, 100)
labels <- c("Low", "Medium", "High", "Very High")

data1<-data

# Create a new variable with labels for ranges
data1$Num <- cut(data1$L..Num, breaks = breaks, labels = labels, include.lowest = TRUE)
data1$Bindo <- cut(data1$L..Baca.B..Indo, breaks = breaks, labels = labels, include.lowest = TRUE)
data1$Bing <- cut(data1$L..Baca.B..ING, breaks = breaks, labels = labels, include.lowest = TRUE)
data1$Sains <- cut(data1$L..Sains, breaks = breaks, labels = labels, include.lowest = TRUE)

# Assuming 'df' is your data frame and 'dplyr' is installed
library(dplyr)
columns_to_drop <- c("L..Num", "L..Baca.B..Indo", "L..Baca.B..ING", "L..Sains")
data1 <- select(data1, -one_of(columns_to_drop))

# Convert the character vector to a factor
data1$Num <- as.factor(data1$Num)
data1$Sains <- as.factor(data1$Sains)
data1$Bindo <- as.factor(data1$Bindo)
data1$Bing <- as.factor(data1$Bing)
data1$Kelamin <- as.factor(data1$Kelamin)
data1$Kota.Kab <- as.factor(data1$Kota.Kab)
data1$Pilihan <- as.factor(data1$Pilihan)

summary(data1)
str(data1)

# a simple split
set.seed(2024)
library(caTools)
split1 <- sample.split(data1$Pilihan, SplitRatio = 0.7)
train_data1 <- subset(data1, split1 == TRUE)
test_data1 <- subset(data1, split1 == FALSE)

vars1 <- c("Kelamin", "Kota.Kab", "Num", "Bindo", "Bing", "Sains")

########## CLASIFICATION
library(C50)
tree_mod1 <- C5.0(x = train_data1[, vars1], y = train_data1$Pilihan)
summary(tree_mod1)
plot(tree_mod1)

tree_boost1 <- C5.0(x = train_data1[, vars1], y = train_data1$Pilihan, trials = 100)
plot(tree_boost1)
summary(tree_boost1)
table(tree_boost1)

library(partykit)
des_tree1 <- ctree(Pilihan~Kelamin+Kota.Kab+Num+Bindo+Bing+Sains, data = train_data1)
des_tree1
plot(des_tree1)

#### PREDICTIONS
predictions1<-predict(tree_boost1, newdata = test_data1[, vars1])

pred1<-predict(tree_mod1, newdata=test_data1[,vars1])

p1<-predict(des_tree1,newdata = test_data1[,vars1])

library(caret)
confusionMatrix(data=predictions1,reference=test_data1$Pilihan)

confusionMatrix(data=pred1,reference=test_data1$Pilihan)

confusionMatrix(data=p1,reference=test_data1$Pilihan)

#####################################

table(train_data1$Pilihan)

table(subset(train_data1,Kota.Kab=="Sleman",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Kota.Kab=="Gunung Kidul",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Kota.Kab=="Kulon Progo",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Kota.Kab=="Yogyakarta",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Kota.Kab=="Bantul",select = c(Pilihan))$Pilihan)

table(subset(train_data1,Kelamin=="Perempuan",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Kelamin=="Laki-laki",select = c(Pilihan))$Pilihan)

table(subset(train_data1,Num=="Very High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Num=="High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Num=="Medium",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Num=="Low",select = c(Pilihan))$Pilihan)

table(subset(train_data1,Bindo=="Very High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Bindo=="High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Bindo=="Medium",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Bindo=="Low",select = c(Pilihan))$Pilihan)

table(subset(train_data1,Bing=="Very High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Bing=="High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Bing=="Medium",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Bing=="Low",select = c(Pilihan))$Pilihan)

table(subset(train_data1,Sains=="Very High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Sains=="High",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Sains=="Medium",select = c(Pilihan))$Pilihan)
table(subset(train_data1,Sains=="Low",select = c(Pilihan))$Pilihan)
