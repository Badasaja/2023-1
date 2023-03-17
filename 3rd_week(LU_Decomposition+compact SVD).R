# 1. LU Decomposition*************************************************
A = matrix(c(4,2,8,6,-2,3,3,5,-6),nrow=3)
A

lu_dc = function(gvn_matrix){
  gvn_dim = dim(gvn_matrix)[1]
  #define operation_maker, which creates operation matrices
  #the operation matrices are !all! lower_triangular matrices.
  #also, only 스칼라배 더하기/빼기 row operation on lower rows are created. 
  action_maker = function(gvn_matrix, col_interest){
    dims=dim(gvn_matrix)[1]
    temp_mat = gvn_matrix[,col_interest]/diag(gvn_matrix)[col_interest]*(-1)
    action_1 = diag(1, nrow = dims)
    action_1[,col_interest] = temp_mat
    diag(action_1) = rep(1,gvn_dim)
    for(j in 1:(dims-1)){
      for(p in 1:(dims-1)){
        if (p>j){
          action_1[j,p]=0}}}
    return(action_1)}
  #create 1.list of operations, 2.the resulting upper mat from the operations
  for (iter in 1:(gvn_dim-1)){
    if(iter == 1){
      op_list = list(action_maker(gvn_matrix, iter))
      new_mat = op_list[[iter]]%*%gvn_matrix
    }
    else{
      op_list[[iter]] = action_maker(new_mat, iter) #append each operations to list 
      new_mat = op_list[[iter]]%*%new_mat #the resulting matrix after each operations
    }}
  #replace with inverse elements for list of operations
  for (iter in (gvn_dim-1):1){
    op_list[[iter]]=solve(op_list[[iter]])}
  
  #assign each matrices
  upper_mat = new_mat
  lower_mat = Reduce("%*%", op_list) #list의 matrices들에게 연산 전달 
  #make a list of the results
  result = list(L = lower_mat,U = upper_mat, row_ops_matrices = op_list)
  return(result)
}
#check_output
lu_dc(A) #정상적으로 나오는 것 확인. 
# 2. System solving with LU decomp.**************************************8
LU_solve = function(gvn_matrix){
  gvn_dim = dim(gvn_matrix)
  if(gvn_dim[1]==gvn_dim[2]){
      print('Provide an Augmented Matrix.')}
  else{
      A = gvn_matrix[1:gvn_dim[1], 1:gvn_dim[1]]
      A_dcomp = lu_dc(A) ;A_dcomp
      b_side = gvn_matrix[,gvn_dim[2]]
      result1 = backsolve(A_dcomp$L, x<-b_side, upper.tri = FALSE)
      result2 = backsolve(A_dcomp$U, x<-result1)
        }
  return(result2)}
# 3. Computing Inverse using LU_solve*************************
#test_input. Find inverse of A
A = matrix(c(4,2,8,6,-2,3,3,5,-6), nrow=3)
#Define Function
LU_inverse = function(gvn_matrix){
  gvn_dim = dim(gvn_matrix)[1] ;gvn_dim
  n = diag(1,gvn_dim)
  out_mat = c()
  for (iters in 1:gvn_dim){
    out_mat = c(out_mat,LU_solve(cbind(A,n[,iters])))
  }
  result = matrix(out_mat,nrow=3)
  return(result)
}
LU_inverse(A)


# 4. Compact SVD for symmetric/ non-symmetric case.
# a. check that symmetric case->singular value equals eigenvalue
A = matrix(c(4,8,10,8,10,-2,10,-2,2),nrow=3)
d =diag(svd(A)$d) 
u = svd(A)$u
v = svd(A)$v
u%*%d%*%t(v)

# b. non-symmetric case->singular value equals sqrt(e.val of mat%*%t(mat))
# compact mode. 
B = matrix(c(2,1,0,0,4,3,0,0),nrow = 4)
svd(B)

# Function definition zone
svd_compact = function(gvn_matrix){
  if(isSymmetric(gvn_matrix)==TRUE){
    U = eigen(gvn_matrix)$vectors
    D = diag(eigen(gvn_matrix)$values)
    V = U
    result = list(U=U,D=D,V=V)
    print('compact SVD for symmetric matrix.')
  }else{# n*m
    AAT = gvn_matrix%*%t(gvn_matrix) # n*n
    ATA = t(gvn_matrix)%*%gvn_matrix # m*m
    AAT_eg = eigen(AAT) 
    ATA_eg = eigen(ATA) 
    #non-zero singular value of non-symmetric A의 개수는 AAT의 non-zero eigenvector 개수와 같다. 
    A_rank = sum(AAT_eg$values!=0) #그러한 singular value는 A의 rank와 같다. 
    U = AAT_eg$vectors[1:A_rank,1:A_rank]
    D = diag(sqrt(AAT_eg$values), A_rank, A_rank)
    V = ATA_eg$vectors[1:A_rank,1:A_rank]
    result = list(U=U,D=D,V=V)
    print('compact SVD for non-symmetric matrix.')
  }
  return(result)
}
svd_compact(A)

# 5. QR Decomposition 



