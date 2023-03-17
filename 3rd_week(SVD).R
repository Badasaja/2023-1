# 1. symmetric case->singular value equals eigenvalue
A = matrix(c(4,8,10,8,10,-2,10,-2,2),nrow=3)
svd(A)

d =diag(
  svd(A)$d
) 
u = svd(A)$u
v = svd(A)$v
u%*%d%*%t(v)
# 2. non-symmetric case->singular value equals root(e.val of mat%*%t(mat))
# compact mode. 
B = matrix(c(2,1,0,0,4,3,0,0),nrow = 4)
svd(B)

# Function definition zone
gvn_matrix =B
# See if matrix is symmetric. 
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
    A_rank = sum(AAT_eg$values!=0) 
    U = AAT_eg$vectors[1:A_rank,1:A_rank]
    D = diag(sqrt(AAT_eg$values), A_rank, A_rank)
    V = ATA_eg$vectors[1:A_rank,1:A_rank]
    result = list(U=U,D=D,V=V)
    print('compact SVD for non-symmetric matrix.')
  }
  return(result)
}
svd_compact(A)
