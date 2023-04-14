## 문제 4번 ##

X = runif(30, 0, 10)
constant = sqrt(X)
new_X = 1/X
n = length(new_X)

# Original Linear Regression
epcilon_1 = NULL
for (j in 1:n){
  epcilon_1 = c(epcilon_1, rnorm(1, 0, 36*new_X[j]))
}
Y = 2 + 3 * X + epcilon_1

# Weighted Linear Regression (-> OLR 식 양변에 sqrt(X)를 곱해줌.)
new_Y = 2 * constant + 3 * constant * X + rnorm(n, 36)

# Scatter plot 확인
dat1 = data.frame(Y,X)
dat2 = data.frame(new_Y,X)
plot(X, Y)
plot(X, new_Y)

# Fitting
fit1 = lm(Y ~ X)
summary(fit1)
fit2 = lm(new_Y ~ X)
summary(fit2)

b.hat.1 = fit1$coefficients
b.hat.2 = fit2$coefficients

m = 10000
b1 =numeric(m)
b2 =numeric(m)
three = numeric(m)

for (j in 1:m)
{
  epcilon = NULL
  for (k in 1:n){
    epcilon = c(epcilon, rnorm(1, 0, 36*new_X[k]))
  }
  Y.star_1 = b.hat.1[1] + b.hat.1[2] * X + epcilon
  new_fit1 = lm(Y.star_1 ~ X)
  b1[j] = new_fit1$coefficients[2]
}
for (j in 1:m)
{
  Y.star_2 = b.hat.2[1] + b.hat.2[2] * X + rnorm(n, 36)
  new_fit2 = lm(Y.star_2 ~ X)
  b2[j] = new_fit2$coefficients[2]
}

# Compare estimators by calculating MSE
for (j in 1:m){
    dev1 <- (b1[j] - b.hat.1[2])^2
    dev2 <- (b1[j] - b.hat.2[2])^2
}
