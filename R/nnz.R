nnz <-
function(A){
  n = dim(which(as.matrix(A)!=0, arr.ind=T))[1]
  return(n)
}
