
#reading the data
salary <- read.csv("C:/Users/mpdur/Downloads/salary-class.csv", stringsAsFactors = TRUE)

#setting the seed
set.seed(1234);


#Subsetting the data - Removing the " ?" from the data
salary <- subset(salary, (salary$COUNTRY != " ?" & salary$EMPLOYER != " ?" & salary$JOBTYPE != " ?"))

#replacing the outliers of age variable
salary$AGE[salary$AGE > 78 | salary$AGE < -2] <- 78

#replacing the outliers of hours variable
salary$HOURS[salary$HOURS > 52.5] <- 52.5
salary$HOURS[salary$HOURS < 32.5] <- 32.5

#dropping the levels which are not required
salary <- droplevels(salary)

#setting the index
SIndex = sample(2,nrow(salary), replace = T, prob = c(0.6, 0.4))
STrainData = salary[SIndex==1, ]
STestData = salary[SIndex==2, ]

#Load Rpart Package
library(rpart)

#decision tree using rpart
salary_rtree = rpart(INCOME ~ ., data = STrainData, parms = list(split="gini"))
plot(salary_rtree)
text(salary_rtree, xpd=T, use.n = T)


#To generate an image for rpart
png('Salary_rtree.png', res=80, height=1000, width=2200)
plot(salary_rtree)
text(salary_rtree, xpd=T, use.n = T)
dev.off()

#using prp to plot the rpart tree
png('Salary_rtreePRP.png', res=80, height=1000, width=2200)
prp(salary_rtree, extra=108,fallen.leaves = F, uniform = T, tweak = 1.5, box.palette = "auto" )
dev.off()

print(salary_rtree)

sal_infoTree = rpart(INCOME ~ ., data = STrainData, parms = list(split = "information"))

#To generate an image for rpart
png('Sal_infoTree.png', res=80, height=1000, width=2200)
plot(sal_infoTree)
text(sal_infoTree, xpd=T, use.n = T)
dev.off()

#using prp to plot the rpart tree
png('Salary_infoTreePRP.png', res=80, height=1000, width=2200)
prp(sal_infoTree, extra=108,fallen.leaves = F, uniform = T, tweak = 1.5, box.palette = "auto" )
dev.off()

print(sal_infoTree)

#No Pruning Trees

sal_infoTree1 = rpart(INCOME ~ ., data = STrainData, parms = list(split = "information"), control = rpart.control(minsplit = 500,minbucket = 100,cp=-1))
prp(sal_infoTree1, extra=108,fallen.leaves = F, uniform = T, tweak = 1.5, box.palette = "auto" )

printcp(sal_infoTree1)

o<-which.min(sal_infoTree1$cptable[,"xerror"])
o

c=sal_infoTree1$cptable[o,"CP"]
c
sal_infoTree2 = rpart(INCOME ~ ., data = STrainData, parms = list(split = "information"), control = rpart.control(minsplit = 500,minbucket = 100,cp=c))
prp(sal_infoTree2, extra=108,fallen.leaves = F, uniform = T, tweak = 1.5, box.palette = "auto" )

printcp(sal_infoTree2)

#The three trees differ by the complexity parameter. The first default tree plotted was a pruned tree. The second tree plotted 
#has a cp value of -1 so as to have the full grown tree and that is not a pruned one.
#The third one was plotted with the best cp value after the cross validation. 

#--default tree---
p.matrix<-table(predict(salary_rtree, type = "class"), STrainData$INCOME)
accuracy1train<-sum(diag(p.matrix))/sum(p.matrix)
accuracy1train


p.matrix<-table(predict(salary_rtree, type = "class", newdata = STestData), STestData$INCOME)
accuracy1test<-sum(diag(p.matrix))/sum(p.matrix)
accuracy1test

#---Fully Grown or Not pruned tree (cp=-1)----
p.matrix<-table(predict(sal_infoTree1, type = "class"), STrainData$INCOME)
accuracy1train<-sum(diag(p.matrix))/sum(p.matrix)
accuracy1train


p.matrix<-table(predict(sal_infoTree1, type = "class", newdata = STestData), STestData$INCOME)
accuracy1test<-sum(diag(p.matrix))/sum(p.matrix)
accuracy1test

#---Best pruned tree (cp=0.0009920635)---
p.matrix<-table(predict(sal_infoTree2, type = "class"), STrainData$INCOME)
accuracy1train<-sum(diag(p.matrix))/sum(p.matrix)
accuracy1train


p.matrix<-table(predict(sal_infoTree2, type = "class", newdata = STestData), STestData$INCOME)
accuracy1test<-sum(diag(p.matrix))/sum(p.matrix)
accuracy1test

#The model with cp=-1 was the most accurate on the training as well as the test data.