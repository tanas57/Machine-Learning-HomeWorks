# Decision Tree Tutorial on Iris Data Set 

library(tree) # Contains the "tree" function

dataSet <- read.delim(file = "wheat_types.txt", sep = ";")

set.seed(551235)  #Set the seed for reproducibility

#first DT
dt <- tree(as.factor(type) ~ ., data = dataSet, split = "deviance")

summary(dt)
misclass.tree(dt)

#Use 80% of samples for training and 20% of them for test purposes
train <- sample(1:nrow(dataSet), size=nrow(dataSet)*0.8)
dt2 <- tree(as.factor(type) ~ . -type, data = dataSet, subset = train)

# plot final DT
plot(dt2,  type = "uniform")
text(dt2)

# Compute training performance of the DT by using only training samples (their indices were saved in the "sub" vector)
train_predict <- table(predict(dt2, dataSet[train, ], type = "class"), dataSet[train, "type"])
rownames(train_predict) <- paste("Actual", rownames(train_predict), sep = ":")
colnames(train_predict) <- paste("Predicted", colnames(train_predict), sep = ":")
print(train_predict)

# Compute test performance of the DT  by using only test samples
test_predict <- table(predict(dt2, dataSet[-train, ], type = "class"), dataSet[-train, "type"])
rownames(test_predict) <- paste("Actual", rownames(test_predict), sep = ":")
colnames(test_predict) <- paste("Predicted", colnames(test_predict), sep = ":")
print(test_predict)

dt2
#Cross-validation version - Construct a new DT for different partitions of the samples - 100 times

dt_acc <- numeric()
set.seed(2561850)

max = 0.0
dtMax = NULL # for finding DT of the best accuracy

for(i in 1:100){
  temp_train <- sample(1:nrow(dataSet), size=nrow(dataSet)*0.8)
  fit2 <- tree(as.factor(type) ~ .-type, data = dataSet, subset = temp_train)
  test_predict <- table(predict(fit2, dataSet[-temp_train, ], type = "class"), dataSet[-temp_train, "type"])
  accuracy = sum(diag(test_predict)) / sum(test_predict)
  
  # find the best accuracy
  if(accuracy >= max){
    max = accuracy
    dtMax = fit2
  }
  
  dt_acc <- c(dt_acc, sum(diag(test_predict)) / sum(test_predict))
}

# average accuracy
mean(dt_acc)

# plot all accuracys
plot(dt_acc, type="l", ylab="Accuracy", xlab="Iterations", main="Accuracy Subsets of Data")
# plot error rates
plot(1-dt_acc, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for our dataset With Different Subsets of Data")

# What is the average perfomance of all DTs?

# plot final DT
plot(dtMax,  type = "uniform")
text(dtMax)

#=========================================================================
#=========================================================================
#=========================================================================

# kNN Tutorial on Iris Data Set 

library(class) # Contains the "knn" function
library(ISLR)
set.seed(5910401) #Set the seed for reproducibility

#Create partitions in the Iris data set (75% for training, 25% for testing/evaluation)
Smarket_sample <- sample(1:nrow(Smarket), size=nrow(Smarket)*0.75)
Smarket_train <- Smarket[Smarket_sample, ] #Select the 75% of rows
Smarket_test <- Smarket[-Smarket_sample, ] #Select the 25% of rows

#First try to determine the right K-value 
Smarket_acc <- numeric() #holding variable

combinations <- list(2:4, 3:5, 2:6)

max_acc <- NULL  #find the maximum accuracy that is possible scenery of all combinations
list_acc <- NULL #for plot combinations accuracy

for(comb in 1:3){
  
  maxAccuracy = 0
  maxKValue = 0
  
  for(i in 1:50){
    #Apply knn with k = i
    predict <- knn(train=Smarket_train[,combinations[[comb]]], test=Smarket_test[,combinations[[comb]]], cl=Smarket_train$Direction, k=i)
    tempAccuracy = mean(predict==Smarket_test$Direction)
    Smarket_acc <- c(Smarket_acc, tempAccuracy)
    
    if(tempAccuracy >= maxAccuracy){
      maxAccuracy = tempAccuracy
      maxKValue = i
    }
  }
  
  print(maxAccuracy)
  print(maxKValue)
  
  max_acc <- c(max_acc, list(maxAccuracy, maxKValue))
  list_acc <- c(list_acc, list(Smarket_acc))
  Smarket_acc <- NULL
}

#determine which combination is the best accuracy
max = 0
maxID = 0
for (a in 1:length(max_acc)) {
  if(a %% 2 == 1){ # accuracy
    if(max_acc[[a]] >= max){
      max = max_acc[[a]]
      maxID = a
    }
  }
}


#plot accuracys of combination1
plot(list_acc[[1]], type="l", ylab="Accuracy",  xlab="K", main="Accuracy of Smarket with KNN")

#plot accuracys of combination2
plot(list_acc[[2]], type="l", ylab="Accuracy",  xlab="K", main="Accuracy of Smarket with KNN")

#plot accuracys of combination3
plot(list_acc[[3]], type="l", ylab="Accuracy",  xlab="K", main="Accuracy of Smarket with KNN")


# Which K-value did provide the best performance ?

print(c("The maximum accuracy is ", max_acc[[maxID]], " and k-value is ", max_acc[[maxID+1]]))
