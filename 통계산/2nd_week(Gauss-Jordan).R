#먼저 test_mat을 정의합니다. 
test_mat1 = matrix(c(4,2,2,6,3,4,3,5,-3,13,3,13), nrow = 3)
test_mat
test_mat2 = matrix(c(4,2,8,6,-2,3,3,5,-6,25,13,-4), nrow = 3)
test_mat2
test_mat3 = matrix(c(2,1,3,5,29,39),nrow=2)
test_mat3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rrf =function(gvn_mat){
  #1.스칼라배 연산 정의
  mult_row = function(gvn_mat, gvn_row) {
    temp <- gvn_mat[gvn_row,]
    cond = temp!=0
    unit = 1/temp[cond][1]
    gvn_mat[gvn_row,]<-temp*unit
    return(gvn_mat)}
  #2.행간연산 정의
  sub_row = function(gvn_matrix, victim, attack,multi, action='sub'){ 
    temp_matrix = matrix(0,nrow=dim(gvn_matrix)[1],ncol=dim(gvn_matrix)[2])
    temp_matrix[victim,]<-gvn_matrix[attack,]*multi
    if (action=='add'){
      result=gvn_matrix+temp_matrix
    }else{
      result=gvn_matrix-temp_matrix
    }
    return(result)}
  #3. swap정의
  swap_row= function(gvn_matrix,victim,attack){
    save = gvn_matrix[victim,]
    gvn_matrix[victim,] <-gvn_matrix[attack,]
    gvn_matrix[attack,] <-save
    return(gvn_matrix)
  }
  #####알고리즘 정의
  ###1. Forward-Elimination, Back Substitution으로. 
  lower_zeroer = function(gvn_mat,start){
    gvn_vec = mult_row(gvn_mat,start) #start은 어차피 0이 아닌 애의 위치에. 
    rep_vec = gvn_vec[start,];mults = gvn_vec[,start]
    if(gvn_mat[start,start]!=1){return(mult_row(gvn_mat-outer(mults,rep_vec),start))}  
    else{
      out= outer(mults, rep_vec)
      out[start,] <- rep(0,dim(gvn_mat)[2]) 
      return(gvn_mat - out)}  
  }
  check_unit = function(gvn_mat,gvn_col){
    cond = gvn_mat[,gvn_col]==1 
    if(all(cond==FALSE)){return(gvn_mat)}
    else{
      a = 1:length(cond)
      unit_row = a[cond] #row number with unit. 
      gvn_mat = swap_row(gvn_mat,gvn_col,unit_row)
      return(gvn_mat)
    }
  }
  ###정의된 함수들로 연산 진행 
  num=dim(gvn_mat)[1]
  result = lower_zeroer(gvn_mat,1)
  print(result)
  for(i in 2:num-1){
    result=check_unit(result,i)
    result=lower_zeroer(result,i)
    print(result)
  }
  print('last operation')
  result = mult_row(result,num) ;print(result)
  result = lower_zeroer(result,num) 
  return(result)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~