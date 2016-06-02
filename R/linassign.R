linassign <-
function(A,D){
  A = as.matrix(A)
  D = matrix(D,nrow=dim(D)[2])
  N = dim(A)[1]
  if (  all(A == matrix(rep(1,N*N), nrow=N) ) ){
    #Use Hungarian Algorithm, Buehren's implementation
    tmpP = solve_LSAP(D)
    d = sum(D[cbind(seq_along(tmpP), tmpP)])
    
    P = as.matrix(sparseMatrix( i=(1:N), j=tmpP, x=rep(1,N) ))
    P = t(P)
    n_vars = N * N
    LINASSIGN = list(P, d, n_vars)
    return(LINASSIGN)
  }else{
   tmpD = matrix(c(as.matrix(D)), nrow = dim(D)[1]*dim(D)[2], ncol=1)
   ivars = which(A!=0)
   n_vars = length(ivars)
   
   #Build equality constrains for all (useful and useless) variables, we will
   #reduce it later
    Aeq.1 = t(kronecker( speye(N), rep(1,N) ) )
    Aeq.2 = t(kronecker( rep(1,N) , speye(N) ) )
    Aeq = rBind(Aeq.1, Aeq.2)
   
   #Reduce the number of variables using ivars
   Aeq_red = Aeq[,ivars]
   dissimilarity_for_lp_red = tmpD[ivars]
   
   #Positivity constrain and rhs of equality constrains
   lb_red  = sparseMatrix(i=c(), j=c(), dims=c(length(ivars),1) )
   lb_red = as.matrix(lb_red)
   beq     = sparseMatrix(i=1:(2*N), j=rep(1,2*N), x=rep(1,2*N), dims=c(2*N,1) )
   b_eq = as.matrix(beq)
   #Fix lower bound problem.  beq = beq -Aeq*lb_red
   beq = beq - Aeq_red%*%lb_red
   P_red.d = solveLP(dissimilarity_for_lp_red,as.matrix(beq), as.matrix(Aeq_red), maximum=FALSE, const.dir = rep("==",dim(beq)[1]), lpSolve=TRUE )
   
   P_red = P_red.d$solution
   d = as.numeric(dissimilarity_for_lp_red%*%P_red)
   
   #remove spurious (close to zero) values
   P_red[P_red<=.1]=0
   P_red = sparseMatrix(i=which(P_red>0), j=rep(1,length(which(P_red>0)) ), x = P_red[which(P_red>0)], dims=c(length(P_red),1) )
   P_red = as.matrix(P_red)
   
   tmpP = sparseMatrix(i=ivars, j=rep(1,length(ivars)), x=c(P_red), dims=c(N*N,1))
   tmpP = as.matrix(tmpP)
   P = matrix(tmpP,nrow=N,ncol=N, byrow=F)
   P = t(P)
   LINASSIGN=list(P,d,n_vars)
   return(LINASSIGN)
   
  }
}
