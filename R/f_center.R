f_center <-
function(X){
  #take row means
  row.mean = matrix(apply(X,1,mean), ncol=1)
  X.c = X - matrix(rep(row.mean, dim(X)[2]), ncol=dim(X)[2] )
  return(X.c)
}
