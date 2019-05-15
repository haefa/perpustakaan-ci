#PRAPROSES


dataset <- read.csv(file="D:/Kuliah/Pasca/CI/Projek/BLE_RSSI_dataset/iBeacon_RSSI_Labeled.csv",header=TRUE)

dataset$date <- NULL

unique(dataset$location)

dataset$x[grep("01", dataset$location)] <- 1
dataset$x[grep("02", dataset$location)] <- 2
dataset$x[grep("03", dataset$location)] <- 3
dataset$x[grep("04", dataset$location)] <- 4
dataset$x[grep("05", dataset$location)] <- 5
dataset$x[grep("06", dataset$location)] <- 6
dataset$x[grep("07", dataset$location)] <- 7
dataset$x[grep("08", dataset$location)] <- 8
dataset$x[grep("09", dataset$location)] <- 9
dataset$x[grep("10", dataset$location)] <- 10
dataset$x[grep("11", dataset$location)] <- 11
dataset$x[grep("12", dataset$location)] <- 12
dataset$x[grep("13", dataset$location)] <- 13
dataset$x[grep("14", dataset$location)] <- 14
dataset$x[grep("15", dataset$location)] <- 15
dataset$x[grep("16", dataset$location)] <- 16
dataset$x[grep("17", dataset$location)] <- 17
dataset$x[grep("18", dataset$location)] <- 18

dataset$x <- as.factor(dataset$x)
dataset$location <- as.factor(dataset$location)


dataset$y[grepl("D", dataset$location, ignore.case=FALSE)] <- 4
dataset$y[grepl("E", dataset$location, ignore.case=FALSE)] <- 5
dataset$y[grepl("F", dataset$location, ignore.case=FALSE)] <- 6
dataset$y[grepl("G", dataset$location, ignore.case=FALSE)] <- 7
dataset$y[grepl("I", dataset$location, ignore.case=FALSE)] <- 8
dataset$y[grepl("J", dataset$location, ignore.case=FALSE)] <- 9
dataset$y[grepl("K", dataset$location, ignore.case=FALSE)] <- 10
dataset$y[grepl("L", dataset$location, ignore.case=FALSE)] <- 11
dataset$y[grepl("M", dataset$location, ignore.case=FALSE)] <- 12
dataset$y[grepl("N", dataset$location, ignore.case=FALSE)] <- 13
dataset$y[grepl("O", dataset$location, ignore.case=FALSE)] <- 14
dataset$y[grepl("P", dataset$location, ignore.case=FALSE)] <- 15
dataset$y[grepl("Q", dataset$location, ignore.case=FALSE)] <- 16
dataset$y[grepl("R", dataset$location, ignore.case=FALSE)] <- 17
dataset$y[grepl("S", dataset$location, ignore.case=FALSE)] <- 18
dataset$y[grepl("T", dataset$location, ignore.case=FALSE)] <- 19
dataset$y[grepl("U", dataset$location, ignore.case=FALSE)] <- 20
dataset$y[grepl("V", dataset$location, ignore.case=FALSE)] <- 21
dataset$y[grepl("W", dataset$location, ignore.case=FALSE)] <- 22

dataset$y <- as.factor(dataset$y)


#Mengubah kelas x dan y
# library(dummies)
# newdata_1<-c()
# newdata<-c()
# for(i in 15:16){
#   newdata_1<-dummy(dataset[,i])
#   newdata_1
#   if(i==15){
#     colnames(newdata_1)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10",
#                            "x13","x14","x15")
#   }
#   else{
#     colnames(newdata_1)<-c("y4","y5","y6","y7","y8","y9","y10","y11","y12","y13",
#                            "y14","y15","y16","y17","y18","y19","y20","y21","y22")
#   }
#   newdata_1
#   dataset<-cbind(dataset, newdata_1)
# }

#SOM
#SOM-X

#Inisialisasi data
library("BBmisc")
dataset[,2:14] <- normalize(dataset[,2:14], method = "range", range = c(0,1))

#Inisialisasi parameter
alpha <- 0.9
beta <- 0.2
epoch_max <- 100
epoch <- 0

n <- 13 #jumlah input
l <- 18 #jumlah output
dl <- integer(l) #untuk menyimpan jarak euclid

#Teknik pengambilan data latih
set.seed(1234)
#idx <- sample(2, nrow(dataset), replace = TRUE, prob = c(0.1, 0.9))
#data <- dataset[idx == 1,]
data <- data.matrix(dataset)
indeks <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
traindata <- data[indeks == 1,]
testdata <- data[indeks == 2,]

#untuk x

library(neuralnet)
nn <- neuralnet(x ~ b3001 + b3002 + b3003 + b3004 + b3005 + b3006 + b3007 + b3008 + 
                  b3009 + b3010 + b3011 + b3012 + b3013, data = traindata, hidden = 20, threshold = 0.01, learningrate = 0.5, stepmax=1e6)
nn$result.matrix
#plot(nn)

#Test the resulting output
temp_test <- subset(testdata, select = c("b3001","b3002", "b3003", "b3004", "b3005", "b3006", 
                                     "b3007", "b3008", "b3009", "b3010", "b3011", "b3012", "b3013"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testdata[,15], prediction = nn.results$net.result)

results$prediction <- round(results$prediction)

table(results$actual,results$prediction)

sama = 0
for(i in 1:nrow(testdata)){
  if(results$actual[i] == results$prediction[i]){
    sama = sama + 1
  }
}
akurasi = sama/nrow(testdata)


#untuk y

library(neuralnet)
nn_y <- neuralnet(y ~ b3001 + b3002 + b3003 + b3004 + b3005 + b3006 + b3007 + b3008 + 
                  b3009 + b3010 + b3011 + b3012 + b3013, data = traindata, hidden = 10, threshold = 0.01, learningrate = 0.5, stepmax=1e6)
nn_y$result.matrix
#plot(nn)

#Test the resulting output
temp_test_y <- subset(testdata, select = c("b3001","b3002", "b3003", "b3004", "b3005", "b3006", 
                                         "b3007", "b3008", "b3009", "b3010", "b3011", "b3012", "b3013"))
head(temp_test_y)
nn_y.results <- compute(nn_y, temp_test_y)
results_y <- data.frame(actual = testdata[,16], prediction = nn_y.results$net.result)

results_y$prediction <- round(results_y$prediction)

table(results$actual,results$prediction)

sama_y = 0
for(i in 1:nrow(testdata)){
  if(results_y$actual[i] == results_y$prediction[i]){
    sama_y = sama_y + 1
  }
}
akurasi_y = sama_y/nrow(testdata)
