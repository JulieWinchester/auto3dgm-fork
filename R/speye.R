speye <-
function(N){
  return(as.matrix(sparseMatrix(i=(1:N),j=(1:N), x=rep(1,N) ) ) )
}
