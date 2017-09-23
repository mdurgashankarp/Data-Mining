hist(hmeq$CLAGE)
DataNew = subset(hmeq, hmeq$CLAGE <500)
hist(DataNew$CLAGE)
outliers = boxplot(hmeq$CLAGE)$out
outliers
outliers1 = boxplot.stats(hmeq$CLAGE)$out
outliers1
clageNoOutliers = ifelse(hmeq$CLAGE %in% outliers, NA, hmeq$CLAGE)
boxplot(clageNoOutliers)
N = hmeq$NINQ
is.na(N)
sum(is.na(N))
sum(!complete.cases(N))
summary(hmeq)
N[is.na(N)] = mean(N, na.rm = T)
sum(is.na(N))
install.packages('mice')
mice(hmeq, m = 5, maxit = 10, meth = 'pmm')
install.packages('VIM')
library(VIM)
aggr(hmeq, col = c('red', 'blue'), numbers = TRUE, prop = TRUE, sortVars= TRUE, 
     labels = names(hmeq), cex.axis = 1, gap = 0, ylab = ('Histogram of missing data'))