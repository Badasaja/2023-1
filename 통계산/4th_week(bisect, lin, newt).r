


for_fun <- function(f, x, h, print = TRUE){
    if (h <= 0){
    return('Choose positive h!')
    }
    derivative = (f(x+h)- f(x))/h
    if(print == TRUE){
        print(paste('FD givest dv',
        derivative,
        'of the funciton f at the point,',
        x))
    }
}

my_fun1 <- function(x){x^2 + x - 5}
for_fun(my_fun1, 2, h=0.5)

my_fun2 <- function(x){
    exp(sin(x)+cos(x)^2)
}
for_fun(my_fun2, x=2.5,h =0.05)
h_set <- seq(1,10^(-4),by = -10^(-4))
