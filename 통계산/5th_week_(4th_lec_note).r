
## Basic tangent line plot

my_fun <- function(x) {x^2 - 3}
x <- seq(-5, 2, by = 0.001)
y <- my_fun(x)
plot(x,y, type = 'l')
#-4x -7
abline(b = -4, a = -7, lty = 1, col = "blue")
points(-2, 1, pch = 19, cex = 3)
legend(-1, 6, legend = c("y = x^2 -3", "y = -4x - 7"),
        col = c("black", "blue"), lty = 1, cex = 2
)

### 1. Forward Difference Method
## Plot
x <- seq(-3, 3, by = 0.001)
y <- my_fun(x)
plot(x, y, type = "l", ylim = c(-5, 6),
    xlab = "", ylab = "", xlim = c(-1.5, 5))
points(c(1, 2), c(-2, 1), pch = 19, cex = 2)
#x = 1에서 접선. 2x
abline(b=2, a= -4, col = "blue")
abline(b = 3, a = -5, col = "red", lty = 3)
legend(-1, 4.5, legend = c("f(x)", "tangent line at x", "Forward difference"),
        col = c("black", "blue", "red"), lty = c(1, 2, 3), cex = 2)
axis(side = 1, at = c(1, 2), labels = c("x", "x+h"),
    cex.axis = 2)
abline(v = 1, lty = 4, col = "green")
abline(v = 2, lty = 4, col = "green")

## Algorithm for forward diff

forward <- function(f, x, h, print = FALSE) {
    if (h <= 0){
        return("Choose positive h")
    }

    derivative <- (f(x + h) - f(x)) / h

    if (print == TRUE){
        print(paste("FD gives the derivative",
                derivative,
                "of the function f at the point",
                h))
    }
    return(derivative)
}

## forward diff test
f <- function(x) {x ^ 2 + x - 5}
x <- 3
h <- seq(from = 1, to = 10^(-3), by = -0.001)
result <- sapply(rev(h), forward, f = f, x = 2, print = FALSE)
plot(rev(h), result, xlab = "h", ylab = "result", type = 'b')

## Question
# Q1: f(x) = exp(sinx + cos^2x). x=2.5에서 미분값 찾아라.
my_fun <- function(x) {exp(sin(x) + cos(x)^2)}
x <- 2.5
h <- seq(from = 1, to = 10^(-3), by = -0.001)
result <- sapply(h, forward, f = my_fun, x = 2.5, print = FALSE)
result[length(h)]
# check with analytical method
my_d <- function(x){exp(sin(x) + cos(x) ^ 2) *
                    (cos(x) - 2 * (sin(x) * cos(x)))}
my_d(2.5)

# Q2 Draw error plot.
true_deriv <- my_d(2.5)
plot(h,abs(result - true_deriv),
    xlab = "h",
    ylab = "abs(deviation from true deriv)")

### 2. Backward Difference Method
##plot
my_fun <- function(x) {x^2 - 3}
x <- seq(-3, 3, by = 0.001)
y <- my_fun(x)
plot(x, y, type = "l", ylim = c(-5, 6), xaxt = "n"
    ,xlab = "", ylab = "", xlim = c(-1.5, 5))
points(c(1, 0), c(-2, -3), pch = 19, cex = 2)
#x = 1에서 접선. 2x
abline(b = 2, a = -4, col = "blue", lty = 1)
abline(b=1, a= -3, col = "red", lty = 3)
legend(-1, 4.5, legend = c("f(x)", "tangent line at x", 
        "backward difference"),
        col = c("black", "blue", "red"), lty = c(1, 1, 3), cex = 2)
axis(side = 1, at = c(0, 1), labels = c("x-h", "x"),
    cex.axis = 2)
abline(v = 1, lty = 4, col = "green")
abline(v = 0, lty = 4, col = "green")


##Algorithm for Forw/Backw diff

forward_backward <- function(f, x, h, type = 1, print = TRUE) {
    if (h <= 0){
        return("Choose positive h!")
    }
    if (type == 1){
        derivative = (f(x + h) - f(x)) / h
        Type = "forward"
    }else{
        derivative = (f(x) - f(x - h)) / h
        Type = "backward"
    }
    if (print == TRUE){
        print(paste(Type, "gives the derivative value",
                    derivative,
                    "of the function f at the point,", x))
    }
    return(derivative)
}
## Test
my_fun2 <- function(x) {x ^ 3 + 2 * x - 5}
forward_backward(my_fun2, 2, 0.1, type = 2, print = TRUE)
forward_backward(my_fun2, 2, -0.1, type = 2, print = TRUE)
forward_backward(my_fun2, 2, 0.1, type = 1, print = TRUE)

#actual derivative
my_fun2prime <- function(x) {3 * x^2 + 2}
true_deriv <- my_fun2prime(2)

#error plot
h_set <- seq(from = 1, to = 0.0001, by = -10 ^ (-5))
for_res <- sapply(h_set, forward_backward,
        f = my_fun2, x = 2, type = 1, print = FALSE)
back_res <- sapply(h_set, forward_backward,
        f = my_fun2, x = 2, type = 2, print = FALSE)
plot(h_set, abs(for_res - true_deriv), type = "l", col = "blue",
    main = "h vs error")
lines(h_set, abs(back_res - true_deriv), col = "red", lty = "23")
legend(0.2, 6.5, legend = c("forward method", "backward method"),
        col = c("blue", "red"),
        lty = c(1, 23),
        cex = 2)


### 3. Central Difference Method
my_fun <- function(x) {x^2 - 3}
x_value <- seq(-3, 3, by = 0.001)
y_value <- my_fun(x_value)
plot(x_value, y_value, type = "l",
    xaxt = "n", xlab = "", ylab = "",
    xlim = c(-1.5, 5), ylim = c(-5, 6))

points(c(0, 2), c(-3, 1), pch = 19)
abline(a = -4, b = 2, col = "blue")
abline(a = -3, b = 2, col = "red", lty = 2)
legend(-1.5, 6, legend = c("f(x)", "tangent line at x",
                            "central difference"),
                            col = c("black", "blue", "red"),
                            lty = c(1, 1, 2), cex = 2)
axis(side = 1, at = c(0, 1, 2), labels = c("x-h", "x", "x+h"),
    cex.axis = 2)

##R code for central

for_back_cent <- function(f, x, h, type = 1, print = TRUE) {
    if (h <= 0){
        return("Choose positive h!")
    }
    if (type == 1){
        derivative = (f(x + h) - f(x)) / h
        Type = "forward"
    }else if (type == 2){
        derivative = (f(x) - f(x - h)) / h
        Type = "backward"
    } else {
        derivative = (f(x+h) - f(x-h)) / (2*h)
        Type = "central"
    }
    if (print == TRUE){
        print(paste(Type, "gives the derivative value",
                    derivative,
                    "of the function f at the point,", x))
    }
    return(derivative)
}

my_fun3 <- function(x) {x^5 + 10*x^2 - 10*x + 3}
my_fun3prime <- function(x) {5*x^4 + 20*x - 10}
true_deriv <- my_fun3prime(1)
result_for <- sapply(h_set, for_back_cent,
    f = my_fun3, x = 1, type = 1, print = FALSE)
result_back <- sapply(h_set, for_back_cent,
    f = my_fun3, x = 1, type = 2, print = FALSE)
result_cent <- sapply(h_set, for_back_cent,
    f = my_fun3, x = 1, type = 3, print = FALSE)

plot(h_set, abs(result_for - true_deriv), type = "l", col = "blue",
    main = "h vs error")
lines(h_set, abs(result_back - true_deriv), col = "red", lty = 2)
lines(h_set, abs(result_cent - true_deriv), col = "green")
legend(0.2, 30, legend = c("forward method",
        "backward method", "central method"),
        col = c("blue", "red", "green"),
        lty = c(1, 23),
        cex = 2)
# central method가 error 가장 적은 이유: O(h^2)
# 여기에 error가 고작 O(h^4)인 걸 만들 수 있다.

cent_enhanced <- function(f, x, h) {
    deriv <- (8 * f(x + h) - 8 * f(x - h) -
        f(x + 2 * h) + f(x - 2 * h))/(12 * h)
    return(deriv)
}
result_enh <- sapply(h_set, cent_enhanced, f= my_fun3, x = 1)
#이걸 바탕으로 라인 추가하면...
lines(h_set, abs(result_enh - true_deriv), col = "purple")

### Application Example *이거 중요할 것 같다.
y_obs <- matrix(c(4.12, 17.31, 122.01, 662.33, 2601.55, 15672.11), ncol = 1)
times <- c(1, 2, 4, 7, 11, 20)


A_gen <- function(x) {c(x^3, x^2, x, 1)}
A_gen <- sapply(c(4, 7, 11, 20), A_gen)
A_gen <- t(A_gen)
a_s <- as.vector(solve(A_gen)%*%y_obs[3:6])

est_func <- function(x) {
    a_s[1] * x ^ 3 + a_s[2] * x ^ 2 + a_s[3] * x + a_s[4]
}

x_seq <- seq(1, 20, by = 0.01)
y_est <- est_func(x_seq)
plot(x_seq, y_est, type = "l", col = 'red')
points(cbind("time" = times, "vel_vec" = y_obs), pch = 12, cex = 1.5)
axis(side = 1, at = times, labels = times, cex = 2.5)

# 13 point에서의 가속력을 측정하라.
forward_backward(est_func, 13, h = 0.0001, type = 1)
