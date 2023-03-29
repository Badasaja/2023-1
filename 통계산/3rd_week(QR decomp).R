#test input zone
A = matrix(c(2,5,1,3,5,1,9,-3,1),ncol = 3)

#define QR_decomp
QR_decomp=function(gvn_matrix){
  
  #정사영 함수 
  proj = function(a,b){
    a = as.vector(a)
    b = as.vector(b)
    return(as.vector((a%*%b/as.vector(a%*%a))%*%a))}
  
  #정규화 함수
  normalize = function(a){
    a=as.vector(a)
    return(a/sqrt(as.vector(a%*%a)))
  }
  
  #G-S Process
  p = dim(gvn_matrix)[2] #p개의 칼럼들. 
  v_list = list(A[,1])
  for (iter in 2:p){
    temp = gvn_matrix[,iter]
    for (sub_iter in 1:(iter-1)){ #이전의 v들을 이용해 정사영을 빼준다. 
         temp = temp-proj(v_list[[sub_iter]],gvn_matrix[,iter])}
    v_list[[iter]]=temp}
  
  #v_list에는 정규화되지 않은 직교기저들이 있다. 이를 Q에서 normalize
  Q = matrix(sapply(v_list, normalize),nrow=3)
  #Q는 orthnormal하니 inverse가 Transpose와 같다.
  R = t(Q)%*%A #따라서 R은 t(Q)와 A의 내적값. 
  result = list(Q=round(Q,3),R=round(R,3))
  return(result)
}

