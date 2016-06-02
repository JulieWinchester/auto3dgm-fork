reduce <-
function(ds, pa, n_jobs){
  d_prototype = matrix(rep(0, ds$n*ds$n), ncol = ds$n)
  gamma_prototype =  matrix(rep(0, ds$n*ds$n), ncol = ds$n)
  
  R_prototype = list()
  P_prototype = list()
  for (ii in 1:ds$n){
    R_prototype = list.add(R_prototype, list()) 
    P_prototype = list.add(P_prototype, list())
    for (jj in 1:ds$n){
      R_prototype[[ii]] = list.add(R_prototype[[ii]], matrix()) 
      P_prototype[[ii]] = list.add(P_prototype[[ii]], matrix()) 
    }
  }
  
  pa = list.add(pa, d_prototype)
  pa = list.add(pa, R_prototype)
  pa = list.add(pa, P_prototype)
  pa = list.add(pa, gamma_prototype)
  names(pa) = c(names(pa)[1:4], c("d","R","P","gamma"))
  
  for (kk in 1:n_jobs){
    KK = kk-1
    file_name = paste(pa$pfj, "ans_", num2str(KK,4), ".RData", sep="")
    if (!file.exists(file_name)){
      print(paste("Error: Answer for job #", num2str(KK,4), " does not exist", sep="" ) )
      return()
    }
    
    pa_tmp = NULL
    load(file_name)
    indices = which(as.matrix(pa_tmp$A)!=0, arr.ind=T)
    r = indices[,1]
    c = indices[,2]
    
    for (ll in 1:length(r)){
      pa$d[r[ll], c[ll]] = pa_tmp$d[r[ll], c[ll]]
      pa$R[[ r[ll] ]][[ c[ll] ]] = pa_tmp$R[[r[ll] ]][[ c[ll] ]]
      pa$P[[ r[ll] ]][[ c[ll] ]] = pa_tmp$P[[ r[ll] ]][[ c[ll] ]]
      pa$gamma[r[ll], c[ll]] = pa_tmp$gamma[r[ll], c[ll]]
    }
    
  }
return(pa)
}
