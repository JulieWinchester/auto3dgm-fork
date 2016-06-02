globalize <-
function(pa, tree, base){
  ga = list(list(), list())
  names(ga)=c("R","P")
  n = dim(tree)[1]
  indices = which(tree!=0, arr.ind=T)
  r = indices[,1]
  c = indices[,2]
  
  mm = min(c(r[1], c[1]))
  MM = max(c(r[1], c[1]))
  N = dim(pa$P[[mm]][[MM]])[2]
  
  TREE = tree+t(tree)
  TREE <- graph.adjacency(as.matrix(TREE), weighted=T)
  for (ii in 1:n){
    dist = shortest.paths(TREE,ii,base)
    if(ii==base){
      pat = ii
    }else{
      pat = unlist(get.shortest.paths(TREE,ii,base))
    }
    
    R = diag(rep(1,3))
    P = sparseMatrix(i = 1:N, j = 1:N, x = rep(1, N))
    P = as.matrix(P)
    if(length(pat)>=2){
      for (jj in 2:length( pat ) ){
        if( pat[jj-1] > pat[ jj] ) {
          P = P %*% pa$P[[ pat[jj] ]][[pat[jj-1] ]]
          R = pa$R[[ pat[jj] ]][[ pat[jj-1] ]] %*% R
        }else{
          P = P %*% t(pa$P[[ pat[jj-1] ]][[ pat[jj] ]])
          R = t(pa$R[[ pat[jj-1] ]][[ pat[jj] ]]) %*% R
        }
      }
  }
    ga$R = list.add(ga$R, R)
    ga$P = list.add(ga$P, P)
  }
  
  
  return(ga)
}
