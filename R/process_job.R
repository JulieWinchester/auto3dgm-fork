process_job <-
function(ifn,ofn,ds,k, pa){
  if(!file.exists(ofn)){
    load(ifn)
    n=dim(pa_tmp$A)[2]
    indices = which(as.matrix(pa_tmp$A)!=0, arr.ind=T)
    r = indices[,1]
    c = indices[,2]
    d_prototype = matrix(rep(0,n*n), ncol=n)
    gamma_prototype = d_prototype
    R_prototype = list()
    P_prototype = list()
    for (ii in 1:n){
      R_prototype = list.add(R_prototype, list()) 
      P_prototype = list.add(P_prototype, list())
      for (jj in 1:n){
        R_prototype[[ii]] = list.add(R_prototype[[ii]], matrix()) 
        P_prototype[[ii]] = list.add(P_prototype[[ii]], matrix()) 
      }
    }
    pa_tmp = list.add(pa_tmp, d_prototype)
    pa_tmp = list.add(pa_tmp, R_prototype)
    pa_tmp = list.add(pa_tmp, P_prototype)
    pa_tmp = list.add(pa_tmp, gamma_prototype)
    names(pa_tmp) = c(names(pa_tmp)[1:4], c("d","R","P","gamma"))
    
    
    for (ii in 1:length(r)){
      message = paste("--------> Processing: ", num2str(r[ii],3), " and ", num2str(c[ii],3), sep="" )
      print(message)
      tmpGPD = f(r[ii],c[ii],ds,k,pa)
      pa_tmp$d[ r[ii], c[ii] ] = tmpGPD[[1]]
      pa_tmp$R[[ r[ii] ]][[c[ii] ]] = tmpGPD[[2]]
      pa_tmp$P[[ r[ii] ]][[c[ii] ]] = tmpGPD[[3]]
      pa_tmp$gamma[ r[ii], c[ii] ] = tmpGPD[[4]]
      
      #now create the 'transposes'
      pa_tmp$d[c[ii], r[ii] ] = pa_tmp$d[ r[ii], c[ii] ]
      pa_tmp$R[[ c[ii] ]][[ r[ii] ]] = pa_tmp$R[[ r[ii] ]][[c[ii] ]]
      pa_tmp$P[[ c[ii] ]][[ r[ii] ]] = pa_tmp$P[[ r[ii] ]][[c[ii] ]]
      pa_tmp$gamma[c[ii], r[ii] ] = pa_tmp$gamma[ r[ii], c[ii] ]
      
      
      
      
      #not really sure what to do from here with the matrix type construction.  
      
    }
    save(pa_tmp,file = ofn) 
  }else{
    message = paste("File: ", ofn, " already exists.  Skipping...", sep="")
    print(message)
  }
}
