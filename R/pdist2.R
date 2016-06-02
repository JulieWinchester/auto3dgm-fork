pdist2 <-
function(X,Y){
  #compute euclidean distance between two data points in X,Y pairwise.  
  #X is an MX-by-N matrix.  
  #Y is an MY-by-N matrix.  
  #D = pdist2(X,Y) is an MX-by-MY matrix.  D_i,j corresponds to the distance between observation
  # i in X and observation j in Y.  Observations are rows. 
  
  MX = dim(X)[1]
  MY = dim(Y)[1]
  N = dim(X)[2]
  if(dim(Y)[2]!=N){
    print("Matrices must have same number of columns")
    return()
  }else{
    
    D = matrix(rep(NA,MX*MY), nrow=MX)
    for (i in 1:MX){
      for (j in 1:MY){
        D[i,j] = sqrt( sum((X[i,]-Y[j,])^2) )
      }
    }
    
    return(D)
  }
  
}
