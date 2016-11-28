

#install.packages("e1071")
#library(e1071)
#library(datasets)
# library(reprtree)
data(mtcars)
 names(mtcars)
head(mtcars)

attach(mtcars)
# suppose we want to classify cars by automatic or manual
x <- subset(mtcars, select = -am)
y <- as.factor(am)

svm_model.cars <- svm(as.factor(am)~., data = mtcars) # add a cost parameter,  = 10, penalty is the cost, for missclassficiation
# if cost = .001, you can missclassify all you want
summary(svm_model.cars)


# fit.svm <- train()
# you can implement SVM through caret package 


###
train.index <- sample(seq(1:(nrow(mtcars))), nrow(mtcars)/2)
test.index <- sample(seq(1:(nrow(mtcars))))[-train.index]
train.data = mtcars[train.index, ]
test.data  = mtcars[test.index,]

svm_model.cars <- svm(as.factor(am)~., data = train.data)

pred <- predict(svm_model.cars,test.data)
length(pred)

length(test.data$am)
system.time(pred <- predict(svm_model.cars,x))

table(pred,test.data$am)


####################
 
