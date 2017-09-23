library(kernlab)
data("ticdata")
?ticdata
attach(ticdata)
table(CARAVAN)
str(ticdata)
install.packages("Random Forest")
library(randomForest)
tic0 = ticdata[1:5822,]
tic1 = ticdata[-(1:5822),]
rf = randomForest(
  CARAVAN ~ .,
  data = tic0,
  ntree = 100,
  proximity = T,
  replace = T,
  sampsize = ceiling(0.65 * nrow(tic0)),
  importance = T,
  mtry = if (!is.null(CARAVAN) &&
             !is.factor(CARAVAN))
    max(floor(ncol(tic0) / 3), 1)
  else
    floor(sqrt(ncol(tic0)))
)
?randomForest
print(rf)
attributes(rf)
plot(rf)
rf$err.rate
rndF1.legend <-
  if (is.null(rf$tic0$err.rate)) {
    colnames(rf$err.rate)
  } else {
    colnames(rf$tic0$err.rate)
  }
legend(
  "top",
  cex = 0.5,
  legend = rndF1.legend,
  lty = c(1, 2, 3),
  col = c(1, 2, 3),
  horiz = T
)

bestsize <-
  function(n0 = 696,
           mtry = 9,
           nselect = 800,
           formula = CARAVAN ~ . ,
           data = tic0[, -1]) {
    ticm.rf <-
      randomForest(formula,
                   sampsize = c(n0, 348),
                   mtry = mtry,
                   data = data)
    nr <-
      (1:nrow(tic0))[order(ticm.rf$votes[, 2], decreasing = T)[1:nselect]]
    sum(tic0[nr, 86] == 'insurance')
  }

bestsize()

rf$votes
