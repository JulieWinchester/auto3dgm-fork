principal_component_alignment <-
function(X_arg,Y_arg,M_arg){
  X = as.matrix(X_arg)
  Y = as.matrix(Y_arg)
  
  SVD.X = svd(X)
  u.X = SVD.X$u
  d.X =diag(SVD.X$d)
  v.X =SVD.X$v
  
  SVD.Y = svd(Y)
  u.Y = SVD.Y$u
  d.Y =diag(SVD.Y$d)
  v.Y =SVD.Y$v
  
  R = list()
  
  tmp = u.X%*%diag(c(1,1,1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(-1,1,1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(1,-1,1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(1,1,-1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(-1,-1,1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(1,-1,-1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(-1,1,-1))%*%t(u.Y)
  R = list.add(R, tmp)
  
  tmp = u.X%*%diag(c(-1,-1,-1))%*%t(u.Y)
  R = list.add(R, tmp)
    
  if (M_arg==0){
    R = Filter(function (x) {det(x) > 0}, R)
  }
  
  return(R)
}
