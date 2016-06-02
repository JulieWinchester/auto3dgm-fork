D2_sparse <-
function(XX,YY,AA){
  index = which(AA!=0, arr.ind=T)
  r = index[,1]
  c = index[,2]
  tmpD2 = apply( (XX[,r]-YY[,c])^2,2,sum)
  D2 = sparseMatrix(i=r, j=c, x=tmpD2)
  return(as.matrix(D2))
}
