### Test exam
# pdf = 2/3*e^(-2t/3)
# cdf = 0~t(pdf) -> -e^(-2t/3)]t_0 = 1 - e()

###Q1
##(1)
orig_cdf <- function(u) {
  - 3 / 2 * log(1 - u)
}
unif_rand <- runif(1000, 0, 1)
rand_nums <- orig_cdf(unif_rand)
mean(rand_nums)
##(2)

T_fun <- function(t){
  if (t < 3){
    return(5)
  }else{
    return(2*t)
  }
}
values <- sapply(rand_nums, T_fun)
mean(values)


###Q2

gvn_fun1 <- function(x) {
  return(x * exp(- x^2/2))
}
x <- seq(0, 10, by = 0.001)
plot(x, gvn_fun1(x))

#최고점을 가지고 있는 정규분포.
#최고점
gvn_fun <- expression ( x * exp( - x^2 / 2))
gvn_fund <- function(x){eval(D(gvn_fun, 'x'))}
max_xval <- uniroot(gvn_fund, c(0, 3))$root
max_yval <- gvn_fun1(max_xval)
max_yval

ar_dist <- function(n_rand, c_val, f_nond) {
  x_vec <- c()
  i <- 0
  while (i < n_rand) {
    iters <- runif(1)
    iters2 <- runif(1)
    if (iters2 <= f_nond(iters) /  c_val) {
      x_vec <- c(x_vec, iters)
      i <- i + 1
    }
  }
  return(x_vec)
}

hist(ar_dist(10000, max_yval, gvn_fun1))

#use MSE

a <- rnorm(10, 0, 2)
sig_tilda <- function(x){
  devs <- (x - mean(x))^2
  return(1/9 * sum(devs))
}
sig_hat <- function(x){
  devs <- (x - mean(x))^2
  return(1/10 * sum(devs))
}
tild_sum <- 0 ; hat_sum <- 0
for (i in 1:10000){
  a <- rnorm(10, 0, 2)
  tild_sum <- tild_sum + (sig_tilda(a) - 4)^2
  hat_sum <- hat_sum + (sig_hat(a) - 4)^2
}

## Q.4

#sample under null(null var(exp) = 4니깐
#lambda = 1/2인 data를 생성하라는 말과 같다.
sum = 0
for (i in 1:10000){
  sample <- rexp(10, rate = 1/2)
  test <- (9*sd(sample)^2)/4
  if (test <= qchisq(0.95, 9)){
    sum = sum + 1
  }
  
}
#confidence level
sum/10000

## Q5.
#bias = E(theta_hat) - theta
gvn_obs <- c(4, 8, 5, 3, 2, 7, 8, 12, 6, 6)

list <- NULL
for (i in 1:10000) {
  x <- mean(sample(gvn_obs, 10, replace = TRUE))
  list <- c(list, x)
}
mean(list)
