#---black box----
set.seed(5)
n <- 10
x <- rexp(n, rate = 2)
#----------------
#emp.dist 만드는 theta-hat를 sample로 추정.
theta_hat <- mean(x)
m <- 10000
x_rep <- matrix(rexp((n*m), rate = (1/theta_hat)), n,m)
emp_dist <- apply(x_rep, 2, mean)
1 / mean(emp_dist)


##
a <- seq(0.001, 0.049, 0.001)
CI <- NULL
for (k in a) {
    #위에서 만든 emp_dist에서 윈도우를 옮긴다. 
    conf <- quantile(emp_dist, prob = c(k, (0.95 + k))) 
    CI = rbind(CI, conf)
}
leng = CI[, 2] - CI[, 1]
CI[which.min(leng), ]

##
n <- 20
alpha <- 0.05
mu0 <- 500 #hypothesized pop mean
sigma2 <- 100 #hypothesized pop var
m <- 10000
I <- numeric(m)

for (j in 1:m) {
    x <- rnorm(n, mu0, sqrt(sigma2))
    Tj <- (mean(x) - mu0) / (sd(x) / sqrt(n))
    if (Tj > qt((1 - alpha), df = (n - 1))) I[j] = 1
    else I[j] = 0
}
type1 <- mean(I)
se_hat <- sqrt(type1 * (1 - type1)/m)

#observed가 주어진 경우
observed <- c(1.77, .78, .02, .18, 1.34, .15, 2.28, .65, .55, .46)
T <- (mean(observed) - 0.5) / (sd(observed)/sqrt(10)) #얘가 qnorm역할
T
#T_j 생성.
i <- 0
T_j <- NULL
for (i in 1:10000) {
    H_0samp <- rexp(10,2)
    T_j <- c(T_j,(mean(H_0samp) - 0.5) / (sd(H_0samp)/sqrt(10)))
}
sum(T_j > abs(T) | T_j < -abs(T)) / 10000
#hypothesized dist안에서 관찰된 T는 극단적인 값이 아님. (T로 만든 인터벌안에 75%의 null들어감)

#empirical power-compare btw T-stat and Z-stat
n = 10
alpha = 0.05
mu0 = 500
sigma2 = 100
m = 10000
I = I1 = numeric(m)
mu1 = seq(490, 520, 5)
r = length(mu1)
power.hat = power1.hat = numeric(r)

for (i in 1:r) {
    for (j in 1:m) {
        x <- rnorm(n, mu1[i], sqrt(sigma2)) #alt하 dist(505, 510)
        Tj <- (mean(x) - mu0) / (sd(x)/ sqrt(n)) #statistic계산시 mu0사용!
        #statistic은 H_0를 기준으로 하는 stat이다.
        #alt하 계산된 mean으로 나온 Tj가
        #기각이 많이 돼야 power가 좋은 것.
        if (Tj > qt(1 - alpha, df = n - 1)) {I[j] = 1}
        else {I[j] = 0}
        if (Tj > qnorm((1 - alpha), 0, 1)) {I1[j] = 1}
        else {I1[j] = 0}
    }
    power.hat[i] = mean(I)
    power1.hat[i] = mean(I1)
}
power.hat
#--------------------------------------------
set.seed(0)
X1 <- seq(1, 30, 2)
X2 <- sample(100:120, 15)
n <- length(X1)
Y <- 1 - 0.5 *X1 + 0.7 *X2 + rnorm(n, sd = 10)
#---black_box-------------------------------
dat = data.frame(Y, X1, X2) #pseudo pop
fit <- lm(Y ~ X1 + X2, data = dat)
summary(fit)

#이제 나온 coefficient들로 데이터 생성.
#이때 필요한 random error의 variance를 pp로 추정.
b_hat <- fit$coefficients
X <- cbind(1, X1, X2)
sigma2 <- sum(fit$residuals^2/(n-3)) #MSE와 동일.
v_bhat <- sigma2 * solve(t(X) %*% X) #sigma2 * c_jj
#qt역할을 할, beta=0을 test하는 pp로부터의 test statistic.
T <- b_hat[2] / sqrt(v_bhat[2, 2])

#s.e and CI for beta
m = 1000
b1 <- numeric(m)
EY <- NULL
for (j in 1:m) {
    Y_star <- b_hat[1] + b_hat[2] * X1 + b_hat[3]* X2 + rnorm(n, sd = sqrt(sigma2))
    fit1 <- lm(Y_star ~ X1 + X2)
    Y_hat <- fit1$coefficients[1] + fit1$coefficients[2]*X1 + fit1$coefficients[3]*X2
    EY <- rbind(EY, Y_hat) #E(Y) 모으는 용도
}
apply(EY, 2, quantile, prob= c(0.025, 0.975))

#when new x is given. c(10, 110)
m <- 1000
EY0 <- numeric(m)
for (j in 1:m) {
    Y_star <- b_hat[1] + b_hat[2] * X1 + b_hat[3] * X2 + rnorm(n, sd = sqrt(sigma2))
    fit1 <- lm(Y_star~ X1 + X2)
    b <- fit1$coefficients
    EY0[j] = b[1] + b[2]* 10 * 110
}
quantile(EY0, prob = c(0.025, 0.975))

#hypothesis test
m <- 5000
TS <- numeric(m)

for (j in 1:m) {
    Y_star <- b_hat[1] + b_hat[3] * X2 + rnorm(n, sd = sqrt(sigma2))
    fit1 <- lm(Y_star ~ X1 + X2) #X1은 넣은채로 fitting!
    b <- fit1$coefficients
    sigma2_star <- sum(fit1$residuals^2) / (n - 3) #iteration별 tstat계산에는
    #H0하 sd이용
    V_bhat_star <- sigma2_star * solve(t(X) %*% X)
    TS[j] <- b[2] / sqrt(V_bhat_star[2, 2])
}
sum(TS > abs(T) | TS < -abs(T)) / m
