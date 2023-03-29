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
Ax_b = matrix(c(4,2,8,6,-2,3,3,5,-6),nrow=3)
b = c(25,13,-4)
z<-optR(Ax_b, b, method = 'gaussseidel',iter = 500,
tol = 1e-7)

seidel(Ax_b,  b)

