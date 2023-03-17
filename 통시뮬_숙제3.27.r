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
  return(as.vector(sapply(as.vector(temp), as.double)))
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

summary <- list(
  "Theoretical mean,variance" = c(n * p, n * p * (1 - p))
  , "Inverse method mean,variance" = c(mean(test_1), var(test_1))
  , "Transformation method mean,variance" = c(mean(test_2), var(test_2))
  , "mean deviation from Theory" = c(n * p - mean(test_1), n * p - mean(test_2))
  , "var deviation from Theory" = c(n * p * (1 - p) - var(test_1),
   n * p * (1 - p) - var(test_2))
)
summary

for (i in 1:3) { 
  print(summary)
}
