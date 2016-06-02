squareform <-
function(X){
  M = dim(X)[1]
  N = dim(X)[2]
  
  D = matrix(rep(0, M*M), nrow=M)
  
  for (ii in 1:M){
    for (jj in 1:M)
      if(ii==jj){
        next
      }else{
        D[ii,jj] = sqrt( sum((X[ii,]-X[jj,])^2) )
      }
  }
  
  return(D)
}
