subsample <-
function(V, N, seed){
  
  if (isempty(seed) ){
    tmp = V[,sample(1:N,1)]
    D = pdist2(t(V),t(tmp) )
    tmpD = max(D)
    tmpind = which(D==tmpD)
    seed = V[,tmpind[1]]
  }
  
  ind_seed = knnsearch(t(V),t(seed),1)[[1]]
  if(norm(as.matrix(seed-V[,ind_seed]), "F")>1e-10 ){
    return(print("Error: Some seed point did not belong to the set of points to subsample from"))
  }
  n_seed = length(ind_seed)
  ind =c(ind_seed, rep(0,(N-n_seed) ) )
  KNN = knnsearch( t(V[,ind[1:n_seed]]), t(V),1 )
  tmpIDX = KNN[[1]]
  D = KNN[[2]]
  
  for (ii in (n_seed+1):N){
    tmp = max(D)
    ind[ii] = which(D==tmp)
    new_dist = pdist2(t(V),t(V[,ind[ii]]) )
    D = sapply(1:length(D), function(i) min(D[i], new_dist[i]) )
  }
  return(ind)
}
