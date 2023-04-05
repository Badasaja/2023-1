###################################
# Statisitcal Simulation Example2 #
###################################

# EX ==================================================

# X1, X2 ~ N(0,1)
# theta = E(|X1 - X2|)
# theta.hat & SE(theta.hat)

m = 10000
theta = numeric(m)
for (i in 1:m) 
{
  x = rnorm(2)
  theta[i] = abs(x[1] - x[2])
}

# Empirical distribution of theta.hat
hist(theta,main='Distribution of theta.hat',prob=TRUE)

theta.hat =  mean(theta)			# MC estimator
theta.hat
2/sqrt(pi)						       	# Exact theta


sd(theta)							    # Standard error of estimator of theta


# EX ==================================================

# X1,...,Xn ~ N(2,1)
# MSE of sample mean, median, & trimmed mean

theta=2
n = 100
m = 1000
k=2

ave = med = tmean = numeric(m)
for (j in 1:m) 
{
  x = sort(rnorm(n,theta,1))
  ave[j] = mean(x)
  med[j] = median(x)	
  tmean[j] = sum(x[(k+1):(n-k)]) / (n-2*k)
}

mean((ave-theta)^2)		# MSE of sample mean
mean((med-theta)^2)		# MSE of sample median
mean((tmean-theta)^2)	# MSE of sample trimmed mean


# EX ================================================== 

# Data
set.seed(1)
n = 10
x = rexp(n,rate=2)
# The distribution is known but the parameter is unknown.

# theta = E(X)
theta.hat = mean(x)

m = 10000
#10만개. 각 칼럼이 하나의 데이터셋이 된다.
x.rep = matrix(rexp((n*m),rate=(1/theta.hat)),n,m)

theta.j = apply(x.rep,2,mean)
  
hist(theta.j)  

# 95% CI for theta
a = seq(0.001,0.049,0.001) #옮기는 시작점, 0.001씩 95% 유지하면서 이동. 
CI = NULL
# grid search해서. 하나씩 옮기면서, 
for (k in a)
{
  conf = quantile(theta.j, prob=c(k,(0.95+k)))
  CI = rbind(CI,conf) 
}

leng = CI[,2] - CI[,1]
#그렇게 구한 인터벌 중 제일 작은 값.
CI[which.min(leng),]
#coverage rate (ci 아는 상황에서, true값 포함하는지 여부. 이때도 세어주면 된다.)

# EX: Empirical Type I error rate ====================

# Case1: Normal population
n = 20
alpha = 0.05
mu0 = 500
sigma2 = 100
#단측 검정이라서 alpha를 2로 나누지 않는다.
m = 10000
I = numeric(m)
for (j in 1:m) 
{
  x = rnorm(n, mu0, sqrt(sigma2))
  Tj = (mean(x) - mu0)/(sd(x)/sqrt(n))
  if (Tj > qt((1-alpha),df=(n-1))) I[j]=1
  else I[j]=0
}

# Empirical Type I error rate;
TypeI = mean(I)

#얘도 estimator이다. 따라서 se.hat(sd of type I)로 그리는 
#confidence interval을 갖고 그 안에 0.05를 잡아야 한다. 
#이항분포~정규분포 근사
se.hat = sqrt(TypeI*(1-TypeI) / m)
print(c(TypeI,se.hat))

#일단 CI안에 alpha있으면 control.
#어쩌다가 나가면 -> 왼쪽이면 control. 더 낮으니깐.
#오른쪽으로 나가면 -> control 못하는거.
#MCMC 하면 CI여러개 나올것. 근데 그렇게 못하니까.
#CI하나로만. 왼쪽, 포함 한 애들로만 power 비교.

## EX: Empirical power

n = 10
alpha = 0.05
mu0 = 500
sigma2 = 100

I = I1 = numeric(m)
mu1 = seq(490,520,5)

m=10000
r = length(mu1)
power.hat = power1.hat = numeric(r)

for (i in 1:r)
{
  for (j in 1:m)
  {
    #x is distribution under alt hypo.
    x = rnorm(n, mu1[i], sqrt(sigma2)) #x는 alt 하에서 generate.
    Tj = (mean(x) - mu0)/(sd(x)/sqrt(n))
    # t test 
    if (Tj > qt((1-alpha),df=(n-1))) I[j]=1
    else I[j]=0
    # z test 비교. 
    if (Tj > qnorm((1-alpha),0,1)) I1[j]=1
    else I1[j]=0
  }
  power.hat[i] = mean(I)	
  power1.hat[i] = mean(I1)
}

#결구 Ij를 수집하는 것
plot(mu1,power.hat,type='b',main='Empirical Power',ylab='Power',col='blue')
lines(mu1,power1.hat,type='b',col='red')
#z test가 power가 더 좋다. 모집단이 normal이기 때문.
#근데 사진 찍은 코드 보면 control 못하고 있는 것 확인. 알파 예선 탈락.

# EX: Regression =======================================

# Data
set.seed(0)
X1 = seq(1,30,2)
X2 = sample(100:120,15)
n = length(X1)
#true model.
Y = 1 - 0.5 * X1 + 0.7 * X2  + rnorm(n,sd=10)
dat = data.frame(Y,X1,X2)


# Estimation
fit = lm(Y ~ X1 + X2)
summary(fit)

b.hat = fit$coefficients
X = cbind(1,X1,X2)
sigma2 = sum(fit$residuals^2)/(n-3)
V.bhat = sigma2 * solve(t(X) %*% X) #공식으로 나온거
T = b.hat[2]/ sqrt(V.bhat[2,2])


# S.E. & CI for beta
m = 1000
b1 =numeric(m)
EY = NULL
for (j in 1:m)
{
  #null하의 distribution -> b.hat은 유효하다.
  Y.star = b.hat[1] + b.hat[2] * X1 + b.hat[3] * X2 + rnorm(n,sd=sqrt(sigma2))
  fit1 = lm(Y.star ~ X1 + X2)
  b1[j] = fit1$coefficients[2] #null 하 fitting한 coefficients들이 true beta를 포함하는가? 
  Yhat = fit1$coefficients[1] + fit1$coefficients[2]*X1 + fit1$coefficients[3]*X2
  EY = rbind(EY,Yhat)
}

sd(b1) #위에 공식으로 나온거랑 비교.

quantile(b1,prob=c(0.025,0.975))

apply(EY,2,quantile,prob=c(0.025,0.975))
#이제 true beta값이 이 interval안에 포함되는지. 


# Prediction interval for E(Y|X=c(10, 110))
m = 1000
EY0 =numeric(m)
for (j in 1:m)
{
  Y.star = b.hat[1] + b.hat[2] * X1 + b.hat[3] * X2 + rnorm(n,sd=sqrt(sigma2))
  fit1 = lm(Y.star ~ X1 + X2)
  b = fit1$coefficients
  EY0[j] = b[1] + b[2] * 10 + b[3] * 110
}

quantile(EY0,prob=c(0.025,0.975))


# Hypothesis Test
# H_0: beta_1 = 0 vs. H_1: beta_1 != 0

m = 5000
TS = numeric(m)
for (j in 1:m)
{
  #null 하 y_hat distribution. 
  Y = b.hat[1] + b.hat[3]*X2 + rnorm(n,sd=sqrt(sigma2))
  fit1 = lm(Y ~ X1 + X2)
  b = fit1$coefficients
  sigma2.star = sum(fit1$residuals^2) / (n-3)
  V.bhat.star = sigma2.star * solve(t(X) %*% X)
  TS[j] = b[2]/ sqrt(V.bhat.star[2,2])
}

sum(TS > abs(T) |  TS < -abs(T)) /m


####practice======================================

m <- 1000
theta <- numeric(m)
for (i in 1:m) {
  x<- rnorm(2)
  theta[i] <- abs(x[1] - x[2])
}
hist(theta, main = 'Distribution of Theta', prob = TRUE)

theta_hat <- mean(theta)

2 / sqrt(pi)
##

theta <- 2 # tru theta.
n <- 100
m <- 1000
k <- 2

ave <- med <- tmean <- numeric(m)

for (j in 1:m) {
  # > generate a sorted sample x1, ..., xn ~ N(2,1)
  x <- sort(rnorm(n, theta, 1))
  ave[j] = mean(x)
  med[j] = median(x)
  # > truncated mean을 구하기 위한 k개 절사. 
  tmean[j] = sum(x[(k+1):(n-k)]) / (n - 2*k)
}
mean(ave - theta)^2
mean(med - theta)^2
mean(tmean - theta)^2

##

set.seed(1)
n <- 10
x <- rexp(n, rate = 2)
##########이 위에는 black box.
# The distribution is known but the parameter is unknown.

theta_hat <- mean(x)

m <- 10000

x_rep <- matrix(rexp((n*m), rate = (1/theta_hat)), n, m)
theta_j <- apply(x_rep, 2, mean)


a <- seq(0.001, 0.049, 0.001)
CI <- NULL
for (k in a) {
  conf <- quantile(theta_j, prob = c(k, (0.95 + k)))
  CI <- rbind(CI, conf)
}
leng <- CI[, 2] - CI[, 1]
CI[which.min(leng), ]
CI


####

n <- 20
alpha <- 0.05
mu0 <- 500
sigma2 <- 100
m <- 10000
I <- numeric(m)

for (j in 1:m) {
  x <- rnorm(n, mu0, sqrt(sigma2))
  Tj <- (mean(x)- mu0) / (sd(x) / sqrt(n))
  if (Tj > qt((1-alpha), df = (n-1))) {I[j] = 1}
  else {I[j] = 0}
}

typeI <- mean(I)
se_hat <- sqrt(typeI * (1 - typeI) / m)
print(c(typeI, se_hat))

## Empirical Power 

n = 10
alpha = 0.05
mu0 = 500
sigma2 = 100

I = I1 = numeric(m)
mu1 = seq(490,520,5)

m=10000
r = length(mu1)
power.hat = power1.hat = numeric(r)


#comparison between t and z statistic
for (i in 1:r)
{
  for (j in 1:m)
  {
    #null dist하의 distribution 생성
    x = rnorm(n, mu1[i], sqrt(sigma2))
    Tj = (mean(x) - mu0)/(sd(x)/sqrt(n))
    #두 개의 test statistic간의 비교 
    if (Tj > qt((1-alpha),df=(n-1))) I[j]=1
    else I[j]=0
    if (Tj > qnorm((1-alpha),0,1)) I1[j]=1
    else I1[j]=0
  }
  power.hat[i] = mean(I)	
  power1.hat[i] = mean(I1)
}

plot(mu1,power.hat,type='b',main='Empirical Power',ylab='Power',col='blue')
lines(mu1,power1.hat,type='b',col='red')
