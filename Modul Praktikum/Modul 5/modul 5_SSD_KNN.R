#memanggil  library
library(caret)
library(class)
library(mvtnorm)
library(ggplot2)
library(MASS)
library(gridExtra)

#import data dan eksplor data
alamat <- 'D:/pima-indians-diabetes.csv'
diabetes <- read.csv(alamat, stringsAsFactors = TRUE)
str(diabetes)

diabetes$class <- as.factor(diabetes$class)
table(diabetes$class)

mean(diabetes$age) # finding the mean age of patients
summary(diabetes) # gives the info about mean, median and other descriptive measures
train=diabetes[1:500,] # building training data to train knn
test=diabetes[501:768,] # test data to test the performance
pred_test=knn(train[,-9],test[,-9],train$class,k=2) # After training on the train data we calculating the output labels for the test data for k=2.
pred_test # to see the output labels
confusion=table(pred_test,test$class) # table() gives us the correct and incorrect predictions
sum(diag(confusion))/nrow(test) # this gives us the accuracy of the model on the test data
confusionMatrix(pred_test,test$class) # Confusion matrix gives us the accuracy, sensitivity and other measures which helps us to interpret the model.



#langkah KNN
plot(diabetes$preg,diabetes$pedi)

#standardize
stdmaxmin <- function(X) (X-min(X))/(max(X)-min(X))
preg1 <- stdmaxmin(diabetes$preg)
pedi1 <- stdmaxmin(diabetes$pedi)

m <-NULL; a <-b <-seq(0, 1, length.out = 70)
for (i in a) for (j in b) m <-rbind(m, c(i, j))
#k=3
prediksi<-knn(cbind(preg1,pedi1), m, diabetes$class, k = 3)
plot(m[,1], m[,2], col=ifelse(prediksi=="tested_positive", "cyan","yellow"),
     pch=ifelse(prediksi=="tested_positive",17,12), main="k=3")
points(preg1, pedi1, col=diabetes$class,
       pch=ifelse(diabetes$class=="tested_positive",17,12), cex=.7)

#k=7
prediksi<-knn(cbind(preg1,pedi1), m, diabetes$class, k = 7)
plot(m[,1], m[,2], col=ifelse(prediksi=="tested_positive", "cyan","yellow"),
     pch=ifelse(prediksi=="tested_positive",17,12), main="k=7")
points(preg1, pedi1, col=diabetes$class,
       pch=ifelse(diabetes$class=="tested_positive",17,12), cex=.7)
#k=10
prediksi<-knn(cbind(preg1,pedi1), m, diabetes$class, k = 10)
plot(m[,1], m[,2], col=ifelse(prediksi=="tested_positive", "cyan","yellow"),
     pch=ifelse(prediksi=="tested_positive",17,12), main="k=10")
points(preg1, pedi1, col=diabetes$class,
       pch=ifelse(diabetes$class=="tested_positive",17,12), cex=.7)

