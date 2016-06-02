rangesearch <-
function(X1,Y1,epsilon1){
  Distance = pdist2(t(X1),t(Y1))
  
  IDX=list()
  D = list()
  for (i in 1:dim(Y1)[2]){
    indices = which((Distance[,i])<=epsilon1)
    tmp=cbind(Distance[indices,i], indices)
    n.row = dim(tmp)[1]
    tmp = matrix(tmp[order(tmp[,1]),], nrow=n.row)
    
    IDX=list.add(IDX,tmp[,2])
    D = list.add(D,tmp[,1])
  }
  
  IDX.D = list(IDX,D)
  return(IDX.D)
}
