knnsearch <-
function(X,Y,k){
  #X is an MX-by-N matrix
  #Y is an MY-by-N
  MX = dim(X)[1]
  MY = dim(Y)[1]
  N = dim(X)[2]
  if(dim(Y)[2]!=N){
    return(print("Error:Matrices need same number of columns"))
  }else{
  
  #for each row in Y, find the nearest neighbor in X;
  dist = pdist2(X,Y)
  
  sorted.dist = sapply(1:dim(dist)[2], function(i) sort(dist[,i]) )
  sorted.index = sapply(1:dim(dist)[2], function(i) order(dist[,i]) )
  
  if(MX==1){
    knn.1 = sorted.index
    knn.2 = sorted.dist
  }else{
    knn.1 = sorted.index[1:k,]
    knn.2 = sorted.dist[1:k,]
  }
  KNN=list(knn.1, knn.2)

  return(KNN)
  }
}
