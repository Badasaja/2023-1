# 1. Gauss-Seidel method*****************************************
#문제를 보면 애초에 3x3 행렬로만 정의해야 할듯

#iter1. 
#individual funtions for solving each variable.
x_solve = function(gvn_mat, y,z){
  result = (1/gvn_mat[1,1])*((-1)*gvn_mat[1,2]*y+(-1)*gvn_mat[1,3]*z+gvn_mat[1,4])
  return(result)
  }
y_solve = function(gvn_mat, x,z){
  result = (1/gvn_mat[2,2])*((-1)*gvn_mat[2,1]*x+(-1)*gvn_mat[2,3]*z+gvn_mat[2,4])
  return(result)
}
z_solve = function(gvn_mat, x,y){
  result = (1/gvn_mat[3,3])*((-1)*gvn_mat[3,1]*x+(-1)*gvn_mat[3,2]*y+gvn_mat[3,4])
  return(result)
}
#def function 
Ax_b = matrix(c(4,2,8,6,-2,3,3,5,-6,25,13,-4),nrow=3)
Ax_b
guess = c(1,1,1)
#start_iter
count = 1
while(TRUE){
guess[1]=x_solve(Ax_b,guess[2],guess[3])
guess[2]=y_solve(Ax_b, guess[1],guess[3])
guess[3]=z_solve(Ax_b, guess[1],guess[2])
print(guess)
count = count + 1
if(count>100) break
}
guess
A = Ax_b[,1:3]
eigen(A)$values #이 중 최댓값이 1보다 작아야 한다. 
seidel(A, Ax_b[,4])



