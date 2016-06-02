locgpd <-
function(X_arg,Y_arg,R_0_arg,M_0_arg){
  X_arg = as.matrix(X_arg)
  Y_arg = as.matrix(Y_arg)
  
  N = dim(X_arg)[2]
  MD2_0 = D2_sparse(X_arg, R_0_arg%*%Y_arg, M_0_arg)
  #Now begin development of linassign.
  LINASSIGN = linassign(M_0_arg, MD2_0)
  P_0 = LINASSIGN[[1]]
  d2 = LINASSIGN[[2]]
  n_vars = LINASSIGN[[3]]
  
  #print something about the initial linear assignment.  Come back to this.
  
  PP = P_0; RR=R_0_arg; d = sqrt(d2); P_prev = matrix(rep(0,N*N), nrow=N);

  while( diff_perms(PP,P_prev)>1e-6 ){
    d_prev = d; P_prev= PP; R_prev = RR;
    CPRO = cprocrustes(X_arg, Y_arg%*%P_prev)
    RR = CPRO[[1]]
    d = CPRO[[2]]
    
    gamma = 1.5*ltwoinf(X_arg-RR%*%Y_arg%*%P_prev)
    M.MD2 = crangesearch(X_arg, RR%*%Y_arg, gamma)
    M = M.MD2[[1]]
    MD2 = M.MD2[[2]]
    
    LINASSIGN = linassign(M,MD2)
    PP = LINASSIGN[[1]]
    d2 = LINASSIGN[[2]]
    n_vars = LINASSIGN[[3]]
    
  }
  
  LOCGPD = list(d, RR, PP, gamma)
  return(LOCGPD)
}
