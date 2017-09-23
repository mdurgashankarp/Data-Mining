data(iris)
str(iris)
set.seed(1234);
index = sample(2,nrow(iris), replace = T, prob = c(0.7, 0.3))
?sample
index
trainData = iris[index==1, ]
trainData
testData = iris[index==2, ]
testData
?ctree
iris_ctree = ctree(Species ~ ., data = trainData)
iris_ctree
plot(iris_ctree)
predict(iris_ctree)
table(predict(iris_ctree), trainData$Species)
predict(iris_ctree, newdata = testData)
table(predict(iris_ctree, newdata = testData), testData$Species)
#ctree_control() - Used for pre pruning

iris_rpart = rpart(Species ~ ., data = trainData, control = rpart.control(minsplit = 5), parms = list(split="gini"))

plot(iris_rpart)
text(iris_rpart, use.n = T, xpd = T)
table(predict(iris_rpart, type = "class", newdata = trainData), trainData$Species)

printcp(iris_rpart)

nrow(iris)
nrow(index)
