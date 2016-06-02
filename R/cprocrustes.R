cprocrustes <-
function(x,y){
  A = x%*%t(y)
  SVD = svd(A)
  uu = SVD$u
  s =diag(SVD$d)
  v =SVD$v
  
  R=uu%*%t(v)
  tmp = x-R%*%y

  
  d = sqrt( sum(colSums(tmp^2) ) )
  CPRO = list(R, d)
  return(CPRO)
}
