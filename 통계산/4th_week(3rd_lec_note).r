#####

# 3rd Lecture Note

#####
##Appendix: Nonlinear Function (R code)
x_value <- seq(-4, 4, by = 0.001)
func_y <- function(x) {
    y <- x^2 - 5
}
y_value <- func_y(x_value)
plot(x_value, y_value, type = "l", xlab = "x", ylab = "f(x)", xlim = c(-4, 1.5))
abline(h = 0, col = "blue")
points(-sqrt(5), 0, pch = 19)


### Bisection Method=================================================
## Problems of the Bisection Method, 3 plot draw---------------------
par(mfrow = c(2, 2)) #2x2로 플롯들 배치.
x_value <- seq(-4, 4, by = 0.001)
func_y1 <- function(x) {x ^ 2 - 4}
func_y2 <- function(x) {x ^ 2}
func_y3 <- function(x) {0.01 / (x-2)}

y_value1 <- func_y1(x_value)
y_value2 <- func_y2(x_value)
y_value3 <- func_y3(setdiff(x_value, 2))

plot(x_value, y_value1, type = "l", xlab = "x", ylab = "f(x)", xlim = c(-4, 4))
abline(v = c(-3.5, 1), col = "red", lty = 2)
points(c(-2, 2), c(0, 0), pch = 8)

plot(x_value, y_value2, type = "l", xlab = "x", ylab = "f(x)", xlim = c(-4, 4))
abline(v = c(-2, 2), col = "red", lty = 2)
points(0, 0, pch = 8)

plot(setdiff(x_value, 2), y_value3,
    type = "l", xlab = "x", ylab = "f(x)", xlim = c(1.9, 2.1))
abline(v = c(1.95, 2, 2.05), col = c("red", "blue", "red"), lty = c(2, 1, 2))

## abline examples-------------------------------------------------
# 1 - x
plot(x_value, y_value2, type = "l", xlab = "x", ylab = "f(x)", xlim = c(-4, 4))
abline(a = 1, b= -1, col = "red", lty = 1)

## Bisection Code---------------------------------------------------

Bisection <- function (f, x0, x1, eps = 10^(-5)) {
    if (f(x0) * f(x1) > 0) {
        print("Wrong Initial Interval!") }

    N <- 0 ; err <- x1 - x0

    while (N <= 1000 & err > eps) {
        x2 <- (x0 + x1) / 2
        if (f(x0) * f(x2) < 0){
            x1 <- x2} else {x0 <- x2}
        N <- N + 1
        err <- x1 - x0
    }
    return((x0 + x1) / 2)
}
my_fun <- function(x) {x^2 - 3}
Bisection(my_fun ,0, 5)

##Problems of Bisection
# No root
my_fun <- function(x) {.01 / (x - 2)}
x0 = 1.9 ; x1 = 2.1
Bisection(my_fun, x0, x1)
# Double root
my_fun <- function(x) {x ^ 2}
x0 = 1.9 ; x1 = 2.1
Bisection(my_fun, x0, x1)

##Exercise for Bisection
# Q1. Compute root(5) using Bisection Method.
my_fun <- function(x) {x^2 - 5}
Bisection(my_fun, 0, 3)

# Q2. Modify so it accumulates errors every time we iterate.

Bisection_witherror <- function(f, x0, x1, eps = 10^(-5)) {
    error_vec <- c()
    if (f(x0) * f(x1) > 0) {
        print("Wrong Initial Interval!") }

    N <- 0 ; err <- x1 - x0

    while (N <= 1000 & err > eps) {
        x2 <- (x0 + x1) / 2
        if (f(x0) * f(x2) < 0) {
            x1 <- x2} else {x0 <- x2}
        N <- N + 1
        err <- x1 - x0
        error_vec <- c(error_vec, err)
    }
    return(list("root" = (x0 + x1) / 2, errors = round(error_vec, 3)))
}
Bisection_witherror(my_fun, 0, 3)


###Linear Interpolation====================================================
##Attempt at drawing plot
par(mfrow = c(1,1))
my_fun <- function(x) {x^2 - 5}
x_val <- seq(-5, 5, by = 0.001)
y_val <- my_fun(x_val)
plot(x_val, y_val, type = "l", ylim = c(-6, 20), xlim = c(0, 4))
abline(h = 0, col = "blue", lty = 2)
abline(a = -5, b = 15/4, lty = 3, )
blue_intercept <- Bisection(my_fun, 0, 4)
points(blue_intercept, 0, pch = 13, cex = 7, col = "blue")
points(0, -5, pch = 3, cex = 7, col = "red")

##Lin interp Code
Linear_interpolation <- function(f, x0, x1, eps = 10^(-5)) {
    if (f(x0) * f(x1) > 0){
        return("Wrong initial Interval!")
    }
    N <- 0
    err <- 10
    errors <- NULL

    while (err > eps && N <= 1000) {
        x2 <- (f(x1) * x0 - f(x0) * x1) / (f(x1) - f(x0))
        err <- abs(f(x2) - 0)
        errors <- c(errors, err)
        N <- N + 1
        if (f(x0) * f(x2) < 0) {
            x1 <- x2} else{
                x0 <- x2
            }
    }
    return(list(solution = x2, errors = round(errors, 4)))
}

##Example
# root of x^2 + x - 5
my_fun <- function(x) {x^2 + x - 5}
Linear_interpolation(my_fun, -5, 0)
# x^4 - 4*x^2 ~~
my_fun <- function(x) {x^4 - 4*x^3 + 2*x^2 + x + 6}
x0 <- 1.70

### Newton's Method================================================

Newton_method <- function(f, fp, x, eps = 10^(-5)) {
    #fp is the derivative function of f.
    N <- 0
    err <- 10
    err_set <- NULL

    while (err > eps & N <= 1000) {
        x0 <- x
        x <- x - f(x) / fp(x)
        err <- abs(x - x0)
        err_set <- c(err_set, err)
        N <- N + 1
    }   
    return(list("solution" = x, "num_repeat" = N, "errors" = round(err_set, 4)))
}

##Exercise

#Q

my_fun <- function(x) {
    x^6 - 6.7*x^5 + 8.8*x^4 - 6.7*x^3 + 8.8*x^2 - 6.7*x + 7.8
}
x0 <- 1.30
x1 <- 2.05
Bisection_witherror(my_fun, x0, x1)
Linear_interpolation(my_fun, x0, x1)

my_fun_prime <- function(x) {
    6*x^5 - 33.5*x^4 + 35.2*x^3 - 20.1*x^2 + 17.6*x - 6.7
}
fun_ex <- expression(x^6 - 6.7*x^5 + 8.8*x^4 - 6.7*x^3 + 8.8*x^2 - 6.7*x + 7.8)
fun_ex_prime <- function(x){eval(D(fun_ex, 'x'))}
fun_ex_prime(3)==my_fun_prime(3)

#Plotting the three errors of each method.
err1 <- Bisection_witherror(my_fun, x0, x1)$errors
err2 <- Linear_interpolation(my_fun, x0, x1)$errors
err3 <- Newton_method(my_fun, fun_ex_prime, (x0 + x1)/2)$errors

plot(1:length(err1), err1, type = 'b', lty = 1)
lines(1:length(err2), err2, type = 'b', col = 'red', lty = 2)
lines(1:length(err3), err3, type = 'b', col = 'blue', lty = 3)
legend(1, 0.3, legend = c('Bisection', 'Lin_inter', 'Newt'),
    col = c('black', 'red', 'blue'), lty = 1:3, cex = 1.5)
