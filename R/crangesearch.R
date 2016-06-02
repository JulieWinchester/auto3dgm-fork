crangesearch <-
function(X1,Y1,epsilon1){
  IDX.D = rangesearch(X1,Y1,epsilon1)
  tmpM=IDX.D[[1]]
  tmpMD=IDX.D[[2]]
  tmpind=tmpM
  for (kk in 1:dim(Y1)[2]){
    tmpind[[kk]]= kk*rep(1, length(tmpind[[kk]]))
  }
  M =  sparseMatrix(i=unlist(tmpM), j=unlist(tmpind), x=rep(1,length(unlist(tmpM))), dims=c(dim(X1)[2], dim(Y1)[2]))
  #M = as.matrix(M)
  MD2= sparseMatrix(i=unlist(tmpM), j=unlist(tmpind), x=unlist(tmpMD)^2, dims=c(dim(X1)[2], dim(Y1)[2]))
  #MD2 = as.matrix(MD2)
  M.MD2 =list(M,MD2)
  return(M.MD2)
}
