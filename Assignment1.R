# Qustion 1a)
salary <- read.csv("C:/Users/mpdur/Downloads/salary-class.csv")
nrow(salary)
str(salary)
dim(salary)

#Question 1b)
str(salary)
summary(salary)

#Question 1c)
mean(salary$AGE)
var(salary$AGE)
sd(salary$AGE)
median(salary$AGE)
#Using library psych
describe(salary$AGE) 

#Question 1d)
#INCOME AND AGE
plot(salary$INCOME,salary$AGE, main="Relationship between AGE and INCOME", col=c("darkblue","brown" ), xlab="INCOME", 
     ylab="AGE")
library(ggplot2)
qplot(salary$INCOME, salary$AGE, geom = "boxplot", xlab = "INCOME", 
      ylab = "AGE", main = "Relationship between AGE and INCOME", fill=salary$INCOME)
#INCOME AND EMPLOYER
tableX <- table(salary$INCOME, salary$EMPLOYER)
barplot(tableX, col = c("darkblue","brown"), xlab = "EMPLOYER", 
        ylab = "NUMBER OF EMPLOYEES", main = "Relationship between INCOME and EMPLOYER", legend.text = rownames(tableX))
mosaicplot(salary$EMPLOYER~salary$INCOME, las=3, cex=1.3, col=c("darkblue","brown" ), 
           xlab="EMPLOYER", ylab="INCOME", main = "Relationship between INCOME and EMPLOYER" )
#INCOME AND MSTATUS
tableY <- table(salary$INCOME, salary$MSTATUS)
barplot(tableY, col = c("darkblue","brown"), xlab = "MARITAL STATUS", 
        ylab = "NUMBER OF PEOPLE", main = "Relationship between INCOME and MSTATUS", legend.text = rownames(tableY))
mosaicplot(salary$MSTATUS~salary$INCOME, las=3, cex=1.3, col=c("darkblue","brown" ), 
           xlab="MARITAL STATUS", ylab="INCOME", main = "Relationship between INCOME and MSTATUS" )
#INCOME AND JOBTYPE
tableZ <- table(salary$INCOME, salary$JOBTYPE)
barplot(tableZ, col = c("darkblue","brown"), xlab = "JOB TYPE", 
        ylab = "NUMBER OF PEOPLE", main = "Relationship between INCOME and JOBTYPE", legend.text = rownames(tableZ), 
        cex.names = 0.7)
mosaicplot(salary$JOBTYPE~salary$INCOME, las=3, cex=1, col=c("darkblue","brown" ), 
           xlab="JOB TYPE", ylab="INCOME", main = "Relationship between INCOME and JOBTYPE" )
#INCOME AND SEX
tableA <- table(salary$INCOME, salary$SEX)
barplot(tableA, col = c("darkblue","brown"), xlab = "SEX", 
        ylab = "NUMBER OF PEOPLE", main = "Relationship between INCOME and SEX", legend.text = rownames(tableA))
mosaicplot(salary$SEX~salary$INCOME, col=c("darkblue","brown" ), 
           xlab="SEX", ylab="INCOME", main = "Relationship between INCOME and SEX" )

#Question 1e)
hist(salary$HOURS, col = "darkblue", freq = F, xlab = "HOURS", main = "Distribution of HOURS variable")
curve(dnorm(x, mean=mean(salary$HOURS), sd=sd(salary$HOURS)), add=TRUE)
lines(density(salary$HOURS), col="brown")

#Question 1f)
tableB <- table(salary$SEX, salary$JOBTYPE)
barplot(tableB, col = c("darkblue","brown"), xlab = "JOB TYPE", 
        ylab = "NUMBER OF PEOPLE", main = "Relationship between SEX and JOBTYPE", legend.text = rownames(tableB), 
        cex.names = 0.7, beside = T)

#Question 1g)
boxplot(salary$C.GAIN, ylab = "C.GAIN")
boxplot.stats(salary$C.GAIN)$out
str(boxplot.stats(salary$C.GAIN)$out)












