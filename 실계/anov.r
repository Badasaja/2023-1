A <- c(17, 20, 15, 21, 28)
B <- c(7, 11, 15, 10, 10)



con<-c(16, 18, 19, 21, 24, 20)
cof <- c(10, 13, 10, 8, 12, 13)
von <-c(21, 23, 19, 26, 22, 24)
vof <- c(12, 16, 13, 14, 16, 13)
love <- c(con, cof, von, vof)
lovefac <- rep(c("con", "cof", "von", "vof"), c(6, 6, 6, 6))
model <- aov(love~lovefac)
summary(model)
###########HW2 
a <- c(17, 20, 15, 21, 28)
b <- c(7, 11, 15, 10, 10)
c <- c(11, 9, 5, 12, 6)
d <- c(5, 4, 3, 7, 6)
love <- c(a,b,c,d)
lvoe <- c(mean(a), mean(b), mean(c), mean(d))
lvoe

lovefac <- rep(c('a','b','c','d'), c(5, 5, 5, 5))
model <- aov(love ~ lovefac)
summary(model)
res <- as.vector(model$residuals)
plot(model$fitted.values, rstandard(model))
abline(h = 0)

plot(model2$fitted.values, rstandard(model2))
abline(h =0)
#########
d_a <- abs(a - median(a))
d_b <- abs(b - median(b))
d_c <- abs(c - median(c))
d_d <- abs(d- median(d))
love <- c(d_a, d_b, d_c, d_d)
lovefac <- rep(c('a','b','c','d'), c(5, 5, 5, 5))
library('car')
leveneTest(y = love, factor(lovefac))
model <- aov(love ~ lovefac)
summary(model)

###book method
one <- c(.18, .40, .71, .18, 1.23, .4)
two <- c(1.7, .33, .47, .25, .25, 1.94)
three <- c(1.495, .565, 1.945, 1.715, 2.015, .565)
four <- c(1.56, 3.77, 4.64, 1.61, 1.24, 1.23)
love <- c(one, two, three, four)
lovefac <- rep(c('a','b','c','d'), c(6, 6, 6, 6))
model <- aov(love~lovefac)
summary(model)

###empirical selection of transformation

#let us say sigma_yi ~ mu_i^alpha

a_a <- c(mean(a), mean(b), mean(c), mean(d))
### approximation method
y <- matrix(log(c(sd(a), sd(b), sd(c), sd(d))), ncol = 1)
X <- matrix(c(1, 1, 1, 1, log(a_a)), ncol = 2)
hat <- solve(t(X)%*%X)%*%t(X)
hat
beta_hat <- hat%*%y
1-beta_hat

###formal method.
love
lamb_transformer <- function(y_set, gvn_lamb){
    y_dot <- prod(y_set^(1/length(y_set)))
    if (gvn_lamb == 0){
        return(y_dot * log(y_set))
    }
    else {
        return((y_set^(gvn_lamb) - 1) / (gvn_lamb * y_dot^(gvn_lamb - 1)))
    }
}
lamb_seq <- seq(from = -2, to = 2, by = 0.01)
SSes <- NULL
for (lambs in lamb_seq) {
    data <- lamb_transformer(love, lambs)
    res <- aov(data ~ lovefac)
    model <- aov(data ~ lovefac)
    SSes <- c(SSes, sum((data - model$fitted.values)^2))
}
lamb_seq[which.min(SSes)]
    
##alpha가 1에 가까우니 log transformation

a <- c(17, 20, 15, 21, 28)
b <- c(7, 11, 15, 10, 10)
c <- c(11, 9, 5, 12, 6)
d <- c(5, 4, 3, 7, 6)
love <- c(log(a), log(b), log(c), log(d))
lovefac <- rep(c('a','b','c','d'), c(5, 5, 5, 5))
model <- aov(love ~ lovefac)
summary(model)
model$residuals
a <- rstandard(model)
plot(model$fitted.values, model$residuals) #단순 residual 쓰는게 아니다 

abline(h = 0)

#levene after transform
d_a <- abs(log(a) - median(log(a)))
d_b <- abs(log(b) - median(log(b)))
d_c <- abs(log(c) - median(log(c)))
d_d <- abs(log(d)- median(log(d)))
love <- c(d_a, d_b, d_c, d_d)
lovefac <- rep(c('a','b','c','d'), c(5, 5, 5, 5))
model <- aov(love~lovefac)
summary(model)
#Declined. 


### Two way anova.
meth_1 <- c(360, 1035, 632, 581, 463)
meth_2 <- c(435, 1152, 750, 703, 520)
meth_3 <- c(391, 1002, 591, 583, 471)
meth_4 <- c(502, 1230, 804, 790, 502)
love<-c(meth_1, meth_2, meth_3, meth_4)
love_mat <- matrix(love, nrow = 4)
sum(love_mat^2) - sum(love_mat)^2/20
meth_fac1 <- rep(c('meth1', 'meth2', 'meth3','meth4'), c(5,5,5,5))
block_fac2 <- rep(c('patient1', 'patient2', 'patient3', 'patient4', 'patient5'), 4)
dat <- data.frame('love'=love, 'method' = factor(meth_fac1), 'patient_block' = factor(block_fac2))

model <- aov(data = dat, love ~ method+patient_block)
summary(model)
# do not look at patient p value, since it is block.
#(c)
#construct 95% confidence interval

mm <- c(mean(meth_1), mean(meth_2), mean(meth_3), mean(meth_4))
t_val <- abs(qt(0.05 / 2 * 6, 16))
MSE <- 1525
n <- 5
int_val <- t_val * sqrt(MSE / n)
int_val
t_val
#mu1 - mu2
mm[1]- mm[4]
pairs <- matrix(c(1,2, 1,3, 1,4, 2,3, 2,4, 3,4), nrow = 2)
pairs <- t(pairs)
Ls <- NULL
Us <- NULL
for (j in 1 : dim(pairs)[1]){
    ind_1 <- pairs[j, ][1]
    ind_2 <- pairs[j, ][2]
    mean_diff <- mm[ind_1] - mm[ind_2]
    Ls <- c(Ls, mean_diff - int_val)
    Us <- c(Us, mean_diff + int_val)
}
cbind(Ls, Us)


## Tukey method
q_crit <- 4.20
int_val <- q_crit * sqrt(MSE/5)
Lst <- NULL
Ust <- NULL
mean_diffs <- NULL
for (j in 1:dim(pairs)[1]){
    ind_1 <- pairs[j,][1]
    ind_2 <- pairs[j,][2]
    mean_diff <- mm[ind_1] - mm[ind_2]
    mean_diffs <- c(mean_diffs, mean_diff)
    Lst <- c(Lst, mean_diff - int_val)
    Ust <- c(Ust, mean_diff + int_val)
}
mean_diffs
mm
cbind(Lst, Ust)
int_val
mean_diffs

(20.2 + 10.6 + 8.6 + 5) * 5

