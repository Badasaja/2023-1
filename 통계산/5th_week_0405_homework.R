### Question 6.
## The given function does not get the solution for the system
## and only provides the result of partial pivoting
## Function with Back-substitution is given in the answer for question 7

data <- c(4,2,2,6,3,4,3,5,-3,13,3,13)
tm_6q <- matrix(data, nrow = 3)
tm_6q

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
gauss_34case(tm_6q)


### Question 7.
## Write a R code for Gaussian Elimination with Partial Pivoting
#This function provides back-substitution and a solution for the given system. 

data <- c(3,-6,6,12,-13,4,-2,-8,9,1,2,6,3,18,4,10,-19,-34,16,26)
tm_7q <- matrix(data, nrow = 4)
tm_7q

#function for gauss_pivot
gauss_pivot <- function(gvn_matrix) {

    gvn_dim <- dim(gvn_matrix)[1]
    #function for row swap.
    swap_row <- function(gvn_matrix, victim, attack) {
        save <- gvn_matrix[victim, ]
        gvn_matrix[victim, ] <- gvn_matrix[attack, ]
        gvn_matrix[attack, ] <- save
        return(gvn_matrix)
    }
    #swap current row with row that contains largest absolute value
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
    #cleans the rows below(or above, if below = FALSE) the given row
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
        #with functions above, we iterate to create a matrix
        #suitable for back substitution
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
        #1.Pivot
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

#test with test_data
gauss_pivot(tm_7q)

###Question 8.

### Question 9.
##Write the R code for QR decomposition for any A.
#test input zone
qr_test <- matrix(c(2,5,1,3,5,1,9,-3,1),ncol = 3)
qr_test2 <- matrix(c(4,10,6,5,-2,3,2,5,-10),nrow=3)
#define QR_decomp
QR_decomp <- function(gvn_matrix) {
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
    v_list[[iter]] <- temp
    }
  #v_list에는 정규화되지 않은 직교기저들이 있다. 이를 Q에서 normalize
  Q <- matrix(sapply(v_list, normalize), nrow = 3)
  #Q는 orthnormal하니 inverse가 Transpose와 같다.
  R <- t(Q) %*% gvn_matrix #따라서 R은 t(Q)와 A의 내적값.
  result <- list(Q = round(Q, 3), R = round(R, 3))
  return(result)
}
