####Statistical Simulation HW2=================================================

### Q1. Let X and Y be independent exponential random variables----------

n <- 10
lam_x <- 2
lam_y <- 3
m <- 10000
theta <- numeric(m)

single_iteration <- function(n, lam_x, lam_y) {
    x_vals <- rexp(n, rate = lam_x)
    y_vals <- rexp(n, rate = lam_y)
    temp <- cbind(x_vals, y_vals)
    return(mean(apply(temp, 1, min)))
}
#mean의 역수는 rate.
for (i in 1:m){
    theta[i] <- single_iteration(n, lam_x, lam_y)
}

## When n = 10, estimate standard error of theta
# S.E of Theta hat is just S.D of simulated thetas
sd(theta)
# compare with formal equation
devs <- theta - mean(theta)
se_theta <- sqrt(sum(sapply(devs, function(x) {x ^ 2})) / (m - 1))
se_theta
print(sd(theta) == se_theta)

## Estimate 95% confidence interval for theta.
a <- seq(0.001, 0.049, 0.001)
CI <- NULL
for (k in a) {
    conf <- quantile(theta, prob = c(k, (0.95 + k)))
    CI <- rbind(CI, conf)
}
leng <- CI[,2] - CI[,1]


## Additionally, true theta E(Z) is 1 / (lam_x + lam_y)
true_theta <- 1 / (lam_x + lam_y)
true_theta


##
Q1_summary <- list(
    "True Theta" = true_theta,
    "Theta hat" = mean(theta),
    "Deviation" = true_theta - mean(theta),
    "SE of Theta hat" = sd(theta),
    "Confidence Interval" = CI[which.min(leng), ]
)
Q1_summary

### Q2. Use MC method to investigate whether type I error..----------
m <- 10000
alpha <- 0.05
mu0 <- 1
sample_n <- c(10, 30, 50)
n <- 10
df <- 1

## (i) chi squared with df = 1
Chi_emp <- function(mu0, n, df, m, alpha) {
    I_i <- numeric(m)
    single_iteration <- function(n, df) {
        x_vals <- rchisq(n, df)
        return(x_vals)
    }

    for (j in 1:m){
        x <- single_iteration(n, df)
        Tj <- (mean(x) - mu0) / (sd(x) / sqrt(n))
        upper_tstat <- qt((1 - alpha/2), df = (n-1))
        lower_tstat <- qt((alpha/2), df = (n-1))
        cond <- (Tj > upper_tstat | Tj < lower_tstat)
        if (cond){
            I_i[j] <- 1
        }
    }
    return(mean(I_i))
}
##(ii)
Unif_emp <- function(mu0, n, low, high, m, alpha) {
    I_i <- numeric(m)
    single_iteration <- function(n, low, high) {
        x_vals <- runif(n, low, high)
        return(x_vals)
    }

    for (j in 1:m){
        x <- single_iteration(n, low, high)
        Tj <- (mean(x) - mu0) / (sd(x) / sqrt(n))
        upper_tstat <- qt((1 - alpha/2), df = (n - 1))
        lower_tstat <- qt((alpha/2), df = (n - 1))
        cond <- (Tj > upper_tstat | Tj < lower_tstat)
        if (cond){
            I_i[j] <- 1
        }
    }
    return(mean(I_i))
}
##(iii)
Exp_emp <- function(mu0, n, lamb, m, alpha) {
    I_i <- numeric(m)
    single_iteration <- function(n, lamb) {
        x_vals <- rexp(n, lamb)
        return(x_vals)
    }

    for (j in 1:m){
        x <- single_iteration(n, lamb)
        Tj <- (mean(x) - mu0) / (sd(x) / sqrt(n))
        upper_tstat <- qt((1 - alpha/2), df = (n - 1))
        lower_tstat <- qt((alpha/2), df = (n - 1))
        cond <- (Tj > upper_tstat | Tj < lower_tstat)
        if (cond){
            I_i[j] <- 1
        }
    }
    return(mean(I_i))
}

result <- NULL
for (k in sample_n){
    a <- Chi_emp(mu0, k, df, m, alpha)
    b <- Unif_emp(mu0, k, 0, 2, m, alpha)
    c <- Exp_emp(mu0, k, 1, m, alpha)
    result <-rbind(result, c(a,b,c))
}
colnames(result) <- c("Chi", "Unif", "Exp")
rownames(result) <- c("10 samp:", "30 samp:", "50 samp:")
result

### Q3
alpha <- 0.05
m <- 10000
n <- 10
p <- 0.3
CI_maker <- function(n, p, alpha) {
    single_iteration <- function(n, p) {
        x_vals <- rbinom(n, 1, p)
        return(x_vals)
    }
    p_hat <- mean(single_iteration(n, p))
    conf <- abs(qnorm(alpha/2))*sqrt((p_hat*(1-p_hat))/n)
    return(c(p_hat - conf, p_hat + conf))
}
#create m x C.I. Count if true parameter in CI.
I_j <-numeric(m)
for (j in 1:m) {
    CI <- CI_maker(n, p, alpha)
    if (CI[1]< p & CI[2]>p) {
        I_j[j] <- 1
        }
}

mean(I_j)

# The interval estimator is too liberal compared
# to our desired confidence level.
# However, the estimator has 80~85% probability
# of capturing true p value

### Q4. Consider the following linear regression model.
# Using MSE to compare between LSE and WLSE
# Use LSE and WLSE to estimate beta coefficient.
# Then using MSE, compare each method

#True Beta = 3
m <- 10000
X_s <- runif(30, 1, 10)
#using true_Y data, regress coefficients.

#LSE case.
lse_coeff <- function(X_s) {
    true_Y <- NULL
    for (i in 1 : length(X_s)){
        y_i <- 2 + 3 * X_s[i] + rnorm(1, 0, sqrt(36/X_s[i]))
        true_Y <- c(true_Y, y_i)
    }
    lse_fit <- lm(true_Y ~ X_s)
    return(lse_fit$coefficients[2])
}

sum_lse <- 0
for (i in 1:m){
    theta_hat <- lse_coeff(X_s)
    true_theta <- 3
    dev_sq <- (theta_hat - true_theta)^2
    sum_lse <- sum_lse + dev_sq
}
MSE_lse <- sum_lse / m
#WLSE Case
wlse_coeff <- function(X_s) {
    true_Y <- NULL
    for (i in 1:length(X_s)){
        y_i <- 2 + 3*X_s[i] + rnorm(1, 0, sqrt(36 / X_s[i]))
        true_Y <- c(true_Y, y_i)
    }
    model <- lm(true_Y~X_s)
    wlse_model <- lm(true_Y ~ X_s, weights = X_s)
    return(wlse_model$coefficients[2])
}
sum_wlse <- 0
for (i in 1:m){
    theta_hat <- wlse_coeff(X_s)
    true_theta <- 3
    dev_sq <- (theta_hat - true_theta)^2
    sum_wlse <- sum_wlse + dev_sq
}
MSE_wlse <- sum_wlse / m

##result.
MSE_lse
MSE_wlse
## MSE_wlse is better performing in this case.
## Why would that be? Let us test for heteroscedasticity.
