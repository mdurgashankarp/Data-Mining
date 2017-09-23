
###############################################
### Neural Network
###############################################

install.packages("ISLR")
library(ISLR)


# Create vector of column Max and Min values
maxs = apply(College[ , 2:18], 2, max)
mins = apply(College[,2:18], 2, min)
#Use scale() and convert the resulting matrix to a data frame

# scale(data, center, scale); If center is a numeric vector with length equal to the number of columns of data, then each column of data has the corresponding value from centersubtracted from it. If center is TRUE then centering is done by subtracting the column means (omitting NAs) of data from their corresponding columns, and if center is FALSE, no centering is done.The value of scale determines how column scaling is performed (after centering). If scale is a numeric vector with length equal to the number of columns of data, then each column of data is divided by the corresponding value from scale. If scale is TRUE then scaling is done by dividing the (centered) columns of data by their standard deviations if center is TRUE, and the root mean square otherwise. If scale is FALSE, no scaling is done.

scaled.data = as.data.frame(scale(College[,2:18], center = mins, scale = maxs - mins))

College[,2:18]<-scaled.data 

set.seed(1234)
ind = sample(2, nrow(College), replace = T, prob = c(0.7, 0.3))
TrainData = College[ind == 1, ]
TestData = College[ind == 2, ]

library(nnet)
nn  =nnet(Private ~ ., data=TrainData,linout=F,size=10,decay=0.01,maxit=1000)

summary(nn)
# You could also use wts to get the best weights found and fitted.values to get the fitted values on training data
nn$wts
nn$fitted.values


nn.preds = predict(nn, TestData)

nn.preds = predict(nn, TestData, type = "class")


table(TestData$Private, nn.preds)

install.packages("neuralnet")

library(neuralnet) 
nn= neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=c(2,1), err.fct="ce", linear.output=FALSE)



out  =cbind(nn$covariate, nn$net.result[[1]]) 
dimnames(out) = list(NULL, c("age","parity","induced", "spontaneous","nn-output"))
head(out)


nn.bp = neuralnet(case~age + parity + induced + spontaneous, data=infert, hidden=2, err.fct="ce", linear.output= FALSE, algorithm="backprop", learningrate=0.01)
nn.bp

nn.nnet = nnet(case~age + parity+induced+ spontaneous, data=infert, size=2, entropy=T,  abstol=0.01) 

plot(nn)

par(mfrow=c(2,2))
gwplot(nn,selected.covariate="age", min=-2.5, max=5)
gwplot(nn,selected.covariate= "parity", min=-2.5, max=5)
gwplot(nn,selected.covariate="induced",min=-2.5, max=5)
gwplot(nn,selected.covariate= "spontaneous", min=-2.5, max=5)

var(nn$generalized.weights[[1]][,1])
var(nn$generalized.weights[[1]][,2])
var(nn$generalized.weights[[1]][,3])
var(nn$generalized.weights[[1]][,4])



##################################################
### K-Nearest Neighbors
##################################################


# packages required
install.packages("shiny")
install.packages("ggvis")
library(ggvis)

#map the data
data("iris")

iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill=~Species) %>% layer_points()

iris %>% ggvis(~Petal.Length, ~Petal.Width, fill=~Species) %>% layer_points()

# To illustrate the KNN algorithm, we use the package"class"

install.packages("class")
library(class)

normalize<-function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
# lapply() returns a list of the same length as the data set 
# that is given in. Each element of the list is the result of 
# application of the normalize argument to the data set iris.

# split data into training and testing
set.seed(1234)
ind=sample(2, nrow(iris), replace = TRUE, prob = c(0.67,0.33))

iris.training <- iris[ind==1,1:4]
iris.test <- iris[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

# build model on training data
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
# cl contains the true classes of the training set
# To get the probability of predictions you could use "prob=TRUE" as an argument
# The argument use.all constrols handling of ties. If true, all distances equal to the 
# k-th largest are included. If false, a random selection of distances equal to the 
# k-th is chosen to use exactly K neighbors

iris_pred

# analyze the prediction
conf.matr <- table(iris_pred, iris.testLabels)
conf.matr

# end summary of result
install.packages("caret")
install.packages("stringi")
library(stringi)
library(caret)

caret::confusionMatrix(conf.matr)



