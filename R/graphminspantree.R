graphminspantree <-
function(G_mat){
  G <- graph.adjacency(as.matrix(G_mat), weighted=T)

  N = dim(G_mat)[1]
  ## MST and plot 
  mst <- minimum.spanning.tree(G) 
  indices = which(as.matrix(get.adjacency(mst))!=0, arr.ind=T)
  entry = rep(NA, dim(indices)[1])
  N_indices = dim(indices)[1]
  for (i in 1:N_indices){
    entry[i] = G_mat[indices[i,1],indices[i,2]]
  }
  
  mst = sparseMatrix(i = indices[,1], j=indices[,2], x = entry, dims=c(N,N) )
  mst = as.matrix(mst)
  return(mst)
}
