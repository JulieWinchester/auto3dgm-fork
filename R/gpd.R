gpd <-
function(X_arg,Y_arg,L_arg){

  
  #Generalized Procrustes Distance
  #L: the number of samples from the ambiguity distribution
  # The first 8 samples are forced to be exactly the 8 elements of the ambiguity set
  # when the singular values are different. 
  
  N = dim(X_arg)[2]
  
  #Initialize tests
  tst = list()
  R_0 = 1
  mat = matrix()
  
  tst = list.add(tst, principal_component_alignment(X_arg,Y_arg))
  
  d = 2
  tst = list.add(tst, rep(0,L_arg))
  
  R=3
  R.prototype =list(); for (i in 1:L_arg){R.prototype = list.add(R.prototype, mat)}
  tst = list.add(tst, R.prototype)
  
  P=4
  P.prototype = list(); for (i in 1:L_arg){P.prototype =list.add(P.prototype, mat)}
  tst = list.add(tst, P.prototype)
  
  gamma = 5
  tst = list.add(tst, rep(0,L_arg))
  
  names(tst) = c("R_0", "d", "R", "P", "gamma")
  
  M_0 = matrix(rep(1,N*N),nrow=N, ncol=N, byrow=T)
  for (ii in 1:L_arg){
    GPD = locgpd(X_arg,Y_arg,tst$R_0[[ii]], M_0)
    tst$d[[ii]]=GPD[[1]]
    tst$R[[ii]]=GPD[[2]]
    tst$P[[ii]]=GPD[[3]]
    tst$gamma[[ii]]=GPD[[4]]
  }
  jmin = min(tst$d)
  jarg = which(tst$d==jmin)[1]
  
  # Return values
  d = tst$d[[jarg]]
  R = tst$R[[jarg]]
  P = tst$P[[jarg]]
  gamma = tst$gamma[[jarg]]
  
  GPD = list(d,R,P,gamma)
  return(GPD)
}
