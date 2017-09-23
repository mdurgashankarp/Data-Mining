# Linear regression 

data(iris)

# We want to use a numerical target variable. Therefore, we predict Sepal. Length
# fit = lm(formual (e.g. y~x), data)

fit = lm(Sepal.Length~Sepal.Width + Petal.Length + Petal.Width, data= iris)

summary(fit)

# To get predicted values
fitted(fit) 
fit$fitted

# To get the slope coefficients
coefficients(fit)
fit$coefficients

# To get the residuals
residuals(fit) 
fit$residual

# Analyzing the fit
layout(matrix(c(1,2,3,4),2,2))
plot(fit)


null = lm(Sepal.Length~1, data= iris) # Includes only the intercept
full = lm(Sepal.Length~Sepal.Width + Petal.Length + Petal.Width, data= iris)
step(null, scope=list(lower=null, upper=full), direction="forward")
step(full, scope=list(lower=null, upper=full), direction="backward")



