###Draw opening plot.

x_value <- seq(0, 3, by = 0.001)
func_y <- function(x){
    x^2 - 2*x + 4
}
y_value <- func_y(x_value)

plot(x_value, y_value,
    type = "l",
    xaxt = "n",
    xlab = "",
    ylab = "y",
    xlim = c(0, 3),
    ylim = c(0, 7), cex.lab = 2)
points(1, func_y(1), pch = 19)
points(1, 0, pch = 19)
points(2, func_y(2), pch = 19)
points(2, 0, pch = 19)
lines(c(1, 1), c(0, func_y(1)),
    lty = 2, col = "blue")
lines(c(2, 2), c(0, func_y(2)), lty = 2,
    col = "blue")
abline(h = 0)
axis(side = 1, at = c(1, 2),
    labels = c("1", "2"), cex.axis = 2)
lines(c(1, 2), c(func_y(1), func_y(2)), col = "red")

###Trapezoid
##Plot for sequential trapezoid.

x_value <- seq(0, 3, by = 0.001)
func_y <- function(x) {x^2 - 2 * x + 4}
y_value <- func_y(x_value)

plot(x_value, y_value, type = "l", xaxt = "n",
    ylim = c(0, 7), ylab = "y")
abline(h = 0)
x_points <- seq(1, 3, by = 0.4)
for (points in x_points){
    points(points, func_y(points), pch = 19)
    points(points, 0, pch = 19)
    lines(c(points, points), c(0, func_y(points)))
    if (points < max(x_points)){
        lines(c(points, points + 0.4), c(func_y(points), func_y(points + 0.4)),
        col = "red")
    }
}
axis(1, at = c(seq(1, 3, by = 0.4)),
    labels = expression(paste(x[0], "=a"), x[1], x[2], x[n-2],
        x[n-1], paste(x[n], "=b")), cex.axis = 1.3)

##Code for trapezoidal method
Trapezoidal <- function(f, a, b, n, print = TRUE) {
    # f is a given function
    # a, b are end points of the interval
    # n is the number of grid points
    if (n <= 0) {
        return("Choose a positive integer n")
    }
    x0 <- sort(c(a, b))[1]
    xn <- sort(c(a, b))[2]
    h <- (xn - x0) / n
    integral <- 0
    x_i <- x0
    for (i in 1:n){
        integral <- integral + h * (f(x_i) + f(x_i + h)) / 2
        x_i <- x_i + h
    }
    return(integral)
}
func_y <- function(x) { x^2 - 2*x + 4}
Trapezoidal(func_y, 1, 2, 10)
Trapezoidal(func_y, 1, 2, 100)

##Draw (n vs error plot)
int_func_y <- function(x) {
    x ^ 3 / 3 - x ^ 2 + 4 * x
}
n_set <- seq(5, 100, 5)
Trap_set <- sapply(n_set, Trapezoidal, f = func_y, a = 1, b = 2)
True_int <- int_func_y(2) - int_func_y(1)
error <- abs(Trap_set - True_int)
plot(n_set, error, type = "points", main = "n vs Error")
lines(n_set, error, lty = '22', col = 'red')

## Approx. error for Trap O(n^-2)
#If concave <- underestimate
#If convex <- overestimate
func_y <- function(x) {
    exp(-0.5 * x^2) / sqrt(2 * pi)
}
Trapezoidal(func_y, -2, 4, 10)
pnorm(4) - pnorm(-2)
#87 / n^2가 std norm의 upper bound인거 확인.
n_set <- seq(5, 100, 5)
result1 <- sapply(n_set, Trapezoidal, f = func_y, a = -2,
        b = 4)
true_int <- pnorm(4) - pnorm(-2)
error <- abs(result1 - true_int)
plot(n_set, error, type = 'b', ylim = c(0, 0.1),
    main = "n vs error")
bound_func <- function(n) {87 / n^2}
lines(n_set, bound_func(n_set))

#### SIMPSONS RULE.

### function estimation 하는거.
func_y <- function(x, n) {x ^ n}
c1 <- func_y(c(0, 2, 4), n = 0)
c2 <- func_y(c(0, 2, 4), n = 1)
c3 <- func_y(c(0, 2, 4), n = 2)

coef <- cbind(c1, c2, c3) # A
y_val <- dnorm(c(0, 2, 4)) # b
# x
x_val <- solve(coef, y_val)
approx_fun <- function(x) {x_val[1] + x_val[2] * x + x_val[3] * x^2}

x_points <- seq(0, 4, by = 0.001)
y_points <- dnorm(x_points)

plot(x_points, y_points, type = "l")
curve(approx_fun, 0, 4, add = TRUE, lty = 2, col = "blue")

for (x in c(0, 2, 4)){
    points(x, 0, pch = 19)
    points(x, dnorm(x), pch = 19)
}
abline(h = 0)
axis(side = 1, at = c(0, 2, 4), labels = c("a", "(a+b)/2", "b"), cex.axis = 2)
legend(2, 0.4, legend = c("f(x)"))

###Simpson's Rule

Simpson_rule <- function(f, a, b, n, print = TRUE){
    if (n %%2 != 0){
        return("Choose an even integer n!")
    }
    if (n < 5){
        return("Choose a larger integer n!")
    }
    x0 <- min(a, b)
    xn <- max(a, b)
    h <- (xn - x0) / n #n은 짝수.
    x_seq <- seq(x0, xn, h) #구간 2개당 bound 3개 생김
    integral <- 0
    for (k in seq(0, n - 2, 2)){
        integral <- integral + h / 3 * (f(x_seq[k + 1])
            + 4 * f(x_seq[k + 2]) + f(x_seq[k + 3]))
        }
    if (print == TRUE) {
        print(paste("Simpson gives", integral))
    }
    return(integral = integral)
    }
    

}
## Example
func_y <- function(x) {x^2 - 2*x + 4}
Trapezoidal(func_y, 1, 2, 100)
Simpson_rule(func_y, 1, 2, 100)

## Example 2
int_func_y <- function(x) {(1/3)*x^3 - x^2 + 4*x}
n_set <- seq(6, 100, by = 4)
trap_errors <- sapply(n_set, Trapezoidal, f = func_y, a = 1, b = 2) -
    (int_func_y(2) - int_func_y(1))
simp_errors <- sapply(n_set, Simpson_rule, f = func_y, a = 1, b = 2, print = FALSE) -
    (int_func_y(2) - int_func_y(1))
plot(n_set, trap_errors)
points(n_set, simp_errors, pch = 19)
legend(40, 0.003, legend = c("Trapezoidal", "Simpson"), 
    lty = c(1, 19), cex = 2.5)
