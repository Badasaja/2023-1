#### 통계계산입문 ASSIGNMENT #1 오준석2017311184
#### Test for all functions can be found at the lower script of the program.

##########################Test Data######################################
data <- c(4,2,2,6,3,4,3,5,-3,13,3,13) 
tm_6q <- matrix(data, nrow = 3)

data <- c(3,-6,6,12,-13,4,-2,-8,9,1,2,6,3,18,4,10,-19,-34,16,26)
tm_7q <- matrix(data, nrow = 4)

data <- c(20, 3, 2, 1, 20, -3, -2, -1, 20, 17, -18, 25)
tm_8q <- matrix(data, nrow = 3)

qr_test <- matrix(c(2,5,1,3,5,1,9,-3,1),ncol = 3)
qr_test2 <- matrix(c(4,10,6,5,-2,3,2,5,-10), nrow = 3)

#########################################################################

### Question 6.
## 3x4 Case
## Function with Back-substitution is given in the answer for question 7

gauss_34case <- function(gvn_matrix) {
    swap_row <- function(gvn_matrix, victim, attack) {
        save <- gvn_matrix[victim, ]
        gvn_matrix[victim, ] <- gvn_matrix[attack, ]
        gvn_matrix[attack, ] <- save
        return(gvn_matrix)
    }

    # row 1 처리
    max_index <- which.max(gvn_matrix[, 1])
    result <- swap_row(gvn_matrix, max_index, 1)
    result[2, ] <- result[2, ] - result[1, ] * (result[2, 1] / result[1, 1])
    result[3, ] <- result[3, ] - result[1, ] * (result[3, 1] / result[1, 1])
    temp <- result
    temp[1, ] <- 0

    # row 2 처리
    max_index <- which.max(temp[, 2])
    result <- swap_row(result, max_index, 2)
    result[3, ] <- result[3, ] - result[2, ] * (result[3, 2] / result[2, 2])
    return(result)

}

### Question 7.
## General Case
## This function provides back-substitution,
## giving solution for the system.

gauss_pivot <- function(gvn_matrix) {

    gvn_dim <- dim(gvn_matrix)[1]
    # function for row swap.
    swap_row <- function(gvn_matrix, victim, attack) {
        gvn_matrix[c(victim, attack), ] <- gvn_matrix[c(attack, victim), ]
        return(gvn_matrix)
    }
    # swap current row with row that contains largest absolute value
    largest_swap <- function(gvn_matrix, gvn_row) {
    if (gvn_row == 1) {
        max_index <- which.max(abs(gvn_matrix[, gvn_row]))
        if (max_index == gvn_row) {
            return(gvn_matrix)
            }
        return(swap_row(gvn_matrix, max_index, gvn_row))
        }
    if (gvn_row == dim(gvn_matrix)[1]) {
        return(gvn_matrix)
    } else {
        temp <- gvn_matrix
        temp[seq(from = 1, to = gvn_row - 1), ] <- 0
        max_index <- which.max(abs(temp[, gvn_row]))
        return(swap_row(gvn_matrix, max_index, gvn_row))
        }
    }
    # cleans the rows below(or above, if below = FALSE) the given row
    row_cleaner <- function(gvn_matrix, gvn_row, below = TRUE) {
        unit_row <- gvn_matrix[gvn_row, ] / gvn_matrix[gvn_row, gvn_row]
        mults <- gvn_matrix[, gvn_row]
        if (below == TRUE) {
            mults[seq(from = 1, to = gvn_row)] <- 0
        }else {
            mults[seq(from = gvn_row, to = dim(gvn_matrix)[1])] <- 0
        }
        temp <- outer(mults, unit_row)
        return(gvn_matrix - temp)
    }
    part_pivot <- function(gvn_matrix) {
        # with functions above, we iterate to create a matrix
        # suitable for back substitution
        for (iters in 1:(gvn_dim - 1)) {
            if (iters == 1) {
                temp <- largest_swap(gvn_matrix, 1)
                temp <- row_cleaner(temp, 1)
                }else {
                temp <- largest_swap(temp, iters)
                temp <- row_cleaner(temp, iters)
                }
        }
        return(temp)
    }
    back_sub <- function(gvn_matrix, gvn_dim) {
        for (iters in seq(from = gvn_dim, to = 2)){
            if (iters == gvn_dim) {
                temp <- row_cleaner(gvn_matrix, gvn_dim, FALSE)
                }else {
                temp <- row_cleaner(temp, iters, FALSE)
                }
    }
    return(temp)
    }
    # Use all the functions.
    pvt_and_backsub <- function(gvn_matrix) {
        #1. Pivot
        pvtd_matrix <- part_pivot(gvn_matrix)
        #2. Backsub
        back_subd <- back_sub(pvtd_matrix, gvn_dim)
        #3. temporary matrix to gain reduced row form.
        A <- back_subd[seq(from = 1, to = gvn_dim), seq(from = 1, to = gvn_dim)]
        b <- matrix(diag(A), nrow = gvn_dim)
        t <- cbind(A, b)
        result <- back_subd / t
        result[is.nan(result)] <- 0
        #define result summary to see
        # 1. pivoted matrix, 2. reduced row form matrix
        summary_r <- list('pivoted_matrix' = pvtd_matrix,
                    'reduced_row_form' = result)
        return(summary_r)
    }
return(pvt_and_backsub(gvn_matrix))
}


# test
gauss_pivot(tm_7q)

###Question 8

gs_iter <- function(gvn_matrix, initial_guess, eps = 1e-10) {
    grow <- dim(gvn_matrix)[1]
    gcol <- dim(gvn_matrix)[2]
    x <- initial_guess
    A <- gvn_matrix[1:grow, 1:grow]
    b <- gvn_matrix[, gcol]
    #check if gvn_matrix is augmented matrix
    if (grow + 1 != gcol) {
        stop("Please give an augmented matrix form.")
    }
    #check if initial guess is vector
    if (!is.vector(x) || length(x) != grow) {
        print(paste("Wrong guess format, Iterate with zeros as initial guess."))
        x <- rep(0, grow)
    }

    # Check if GS method converges
    diag_domin <- function(gvn_matrix) {
        x <- diag(gvn_matrix)
        dim <- dim(gvn_matrix)[1]
        gvn_matrix <- abs(gvn_matrix)
        crit <- 0
        for (iter in 1:dim){
            if (2 * x[iter] >= sum(gvn_matrix[iter,])) {
                crit <- crit + 1
            }
        }
        if (crit == dim){return (TRUE)} else{
            return(FALSE)
        }
        }
    if (diag_domin(A)==FALSE){
        stop("The matrix diverges when using Gauss-Seidel Method.")
    }
    # Algorithm for guessing process.
    GS <- function(A, b, x) {
        a <- diag(A)
        diag(A) <- 0
        for (i in 1:length(x)) {
            x[i] <- (b[i] - crossprod(A[i, ], x)) / a[i]
        }
        return(x)
    }

    while (TRUE) {
    x <- GS(A, b, x)
    temp <- round(x, 1)
    if (all(abs(temp - x) < eps)) {
        break
    }}
    return(x)
}


### Question 9.
##Write the R code for QR decomposition for any A.
#test input zone

#define QR_decomp
QR_decomp <- function(gvn_matrix, round_val = 3) {
  #Projection function.
  proj <- function(a, b) {
    a <- as.vector(a)
    b <- as.vector(b)
    return(as.vector((a %*% b / as.vector(a %*% a)) %*% a))
    }
  #normalizing function
  normalize <- function(a) {
    a <- as.vector(a)
    return(a / sqrt(as.vector(a %*% a)))
  }
  #G-S Process
  p <- dim(gvn_matrix)[2] #p개의 칼럼들.
  v_list <- list(gvn_matrix[, 1])
  for (iter in 2:p){
    temp <- gvn_matrix[, iter]
    for (sub_iter in 1:(iter - 1)){ #이전의 v들을 이용해 정사영을 빼준다.
         temp <- temp - proj(v_list[[sub_iter]], gvn_matrix[,iter])}
    #그렇게 성분제거한 벡터들을 v_list에 쌓아 orthonormal한 벡터를 누적.
    v_list[[iter]] <- temp
    }
  #v_list에는 정규화되지 않은 직교기저들이 있다. 이를 Q에서 normalize
  Q <- matrix(sapply(v_list, normalize), nrow = 3)
  #Q는 orthnormal하니 inverse가 Transpose와 같다.
  R <- t(Q) %*% gvn_matrix #따라서 R은 t(Q)와 A의 내적값.
  result <- list(Q = round(Q, round_val), R = round(R, round_val))
  return(result)
}

#########################Test  Summary############################

q6_test_result <- gauss_34case(tm_6q)
q7_test_result <- gauss_pivot(tm_7q)
q8_test_result <- gs_iter(tm_8q, c(0, 0, 0))
q9_test_result <- QR_decomp(qr_test)
q9_test_result2 <- QR_decomp(qr_test2)

summary_a <- list(
    "Test data of Q6" = tm_6q,
    "Result of Q6----" = gauss_34case(tm_6q),
    "Test data of Q7" = tm_7q,
    "Result of Q7----" = gauss_pivot(tm_7q),
    "Test data of Q8" = tm_8q,
    "Result of Q8----" = gs_iter(tm_8q, c(0, 0, 0)),
    "Test data 1 of Q9" = qr_test,
    "Result of Q9_1----" = QR_decomp(qr_test),
    "Test data 2 of Q9" = qr_test2,
    "Result of Q9_2----" = QR_decomp(qr_test2)
)
summary_a


####################################################################

### Question 10
d <- c(10.9053, 6.0228, 3.8472)
# After SVD,
# the singular values are given as $d so
print("The singular values of A are")
print(d)
# rank(A) = 3 since there are 3 SVDs.
# However, U is given as 4x3 matrix,
# and V as a 3x3 matrix,
# meaning A is a 4 by 3 matrix.
print("Since A is a 4x3 matrix,")
print("we cannot obtain a determinant for A.")

### Question 11
d <- c(17.1189, 9.5683, 8.4494)
# U and V are identical, meaning that
# A is a symmetric matrix.
# In case a symmetric matrix is SVDed,
# It is simply identical to eigendecomposition.
# However, the singular values of A are
# absolute values of eigenvalues.
print("The absolute eigenvalues=singular values of A are,")
print(d)
print("which is identical to singular values.")
print(paste("The absolute determinant of A is the 
                product of its absolute eigenvalues:", prod(d)))
