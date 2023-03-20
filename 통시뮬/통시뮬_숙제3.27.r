###---------------------------------------------------###
#Problems

### 1. Consider the binomial distribution with a parameter(n=5, p=0.2)
## (1) Generate 100 random numbers using the inverse transformation method

# we need distribution information.
cdf_maker <- function(num, n, p) {
  result <- rbinom(num, n, p)
  cuts <- c(0, cumsum(as.vector(table(result)) / num))
  return(cuts)
  }

# based on the distribution info,
# we generate inverse transformed binomial random v.
binom_inverse <- function(num, gvn_dist) {
  made_unif <- runif(num)
  temp <- cut(made_unif, breaks = gvn_dist)
  tags <- (0:(nlevels(temp) - 1))
  levels(temp) <- tags
  return(as.vector(sapply(as.vector(temp), as.double))) # nolint # nolint
  }
## (2) Generate 100 random numbers using the transformation method.

# 베르누이 생성기 정의한다.
bern_dist <- function(n, p) {
  rand_unifs <- runif(n)
  turner <- function(x) {
    if (x <= p) {
      result <- 1
      }else {
        result <- 0
        }
    return(result)
  }
  return(sapply(rand_unifs, turner))
  }

# Transformation Method
binom_dist <- function(num_random, n, p) {
  result <- c()
  for (iter in 1:num_random) {
    temp <- bern_dist(n, p)
    result <- c(result, sum(temp == 1))
    }
  return(as.vector(result))
}


#(3) Calculate mean and variance for random numbers.
num_rand <- 100
n <- 5
p <- 0.2
sample_dist <- cdf_maker(num_rand, n, p)
test_1 <- binom_inverse(num_rand, sample_dist)
test_2 <- binom_dist(num_rand, n, p)


### 2. Generate 100 Poisson Numbers using inverse transformation method

inv_pois <- function(n_rand, lambda) {
  rand_unifs <- runif(n_rand)
  y <- 0
  p <- exp(-1 * lambda)
  f_crit <- p
  poiss <- c()
  for (u in rand_unifs) {
  y <- 0
  p <- exp(-1 * lambda)
  f_crit <- p
  while (TRUE) {
    if (u < f_crit) {
    poiss <- c(poiss, y)
    break
  } else {
      p <- (lambda / (y + 1)) * p
      f_crit <- f_crit + p
      y <- y + 1
    }
  }
}
return(poiss)
}

n_rand <- 100
lambda <- 2
gen_result <- inv_pois(n_rand, lambda)
summary2 <- list(
  "Theoretical mu,var" = c(lambda, lambda), # nolint
  "Computed mu, var" = c(mean(gen_result), var(gen_result) # nolint
  ))
summary2
### 3. Consider the pdf of the random variable X as follows.

## (1) Generate 1,000 random numbers of X using inverse transformation method.

n_rand <- 100
rand_unifs <- runif(n_rand)

x_vec <- c()
for (rand_unf in rand_unifs) {
  #we gain root of the cdf of given distribution
    x <- uniroot(function(x) (x^2) / 4 + x / 2 + 1 / 4 - rand_unf,
            lower = -1, upper = 1, tol = 0.0001)$root
    x_vec <- c(x_vec, x)
    }

summary3_1 <- list(
  "mu" = mean(x_vec),
  "var" = var(x_vec)
)
summary3_1

## (2) Let Y=X^2. Estimate E(Y) and Var(Y) using the 1000 random numbrs.
# the distribution of Y will simply be distribution of X^2

y_vec <- round(sapply(x_vec, function(x) x^2), 4)
summary <- list(
  "mu" = mean(y_vec),
  "var" = var(y_vec)
)
summary

# check the distribution with cdf formula of y
y_vec2 <- c()
for (rand_unf in rand_unifs) {
  #we gain root of the cdf of given distribution
    y <- uniroot(function(x) (x^0.5) - rand_unf,
            lower = 0, upper = 1, tol = 0.0001)$root
    y_vec2 <- c(y_vec2, y)
    }

### 4. Suppose that we want to generate random numbers from gvn_dist
## (1) Obtain min c.
gvn_f <- expression(6 * x * (1 - x))
f_nond <- function(x) eval(gvn_f)
f_d <- function(x) eval(D(gvn_f, "x"))
max_f <- uniroot(f_d, lower = 0, upper = 1)
print("The minimum c is:")
c_val <- f_nond(max_f$root)
c_val
## (2) Using the acceptance-rejection method, compute 100 rand numbers.
n_rand <- 100
#accept-reject
#이때, c의 역수만큼. 즉, 1000개 넣으면 대략 640~660개정도가 accept.
ar_dist <- function(n_rand, c_val) {
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
x_vec <- ar_dist(n_rand, c_val) #result of ar method
#plot으로 확인
x <- seq(0, 1, by = 0.001)
y <- f_nond(x)
hist(x_vec, freq = FALSE)
lines(x, y, col ='red')

## (3) estimate average number of trials
ar_counter <- function (n_rand, c_val) {
  x_vec <- c()
  i <- 0
  trial_count <- 0
  while (i < n_rand) {
    trial_count <- trial_count + 1
    iters <- runif(1)
    iters2 <- runif(1)
    if (iters2 <= f_nond(iters) /  c_val) {
      x_vec <- c(x_vec, iters)
      i <- i + 1
    }
  }
  return(trial_count / i)
}
## The average number of trials approximate to 1.5, or c.
ar_counter(100, c_val)

### 5. Generate 200 random numbers with given u prime and cov mat.

rmvn_chol <- function(n, mu, sigma) {
  #generate n random vectors from MVN(mu, sigma)
  #dimension is inferred from mu and sigma
  d <- length(mu) #length 2면 2차원 multivariate normal 생성.
  #chol returns lower triangular. seems to be updated.
  chol_d <- chol(sigma)
  Z <- matrix(rnorm(n * d), nrow = n, ncol = d) #standard 생성.
  X <- Z %*% chol_d + matrix(mu, nrow = n, ncol = d, byrow = TRUE)
  return(X)
}
#choleski decomposition 사용하면, 안에서 분해 기법만 바뀐다.

#Now, use pairs to make scatterplot.
mu <- matrix(c(0, 1, 2))
cov_mat <- matrix(c(1.0, -.5, .5, -.5, 1, -.5, .5, -.5, 1), nrow = length(mu))
gvn_mults <- rmvn_chol(200, mu, cov_mat)
pairs(gvn_mults)