
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

# Q2: Modify the function forward such that when h>1, warning