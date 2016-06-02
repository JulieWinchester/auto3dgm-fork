map <-
function(pa, n_jobs){
  
  n = dim(pa$A)[1]
  indices = which(as.matrix(pa$A)!=0, arr.ind=T)
  r = indices[,1]
  c = indices[,2]
  v = rep(NA, length(r))
  for (i in 1:length(v)){ v[i] = pa$A[r[i], c[i]]}
  size_job = ceiling(nnz(pa$A)/n_jobs)
  pa_tmp = pa
  
  for (kk in 1:n_jobs){
  k = kk-1
  
  file_name = paste(pa$pfj,"job_", num2str(k,4), '.RData', sep="")
    if( !file.exists(file_name) ){
      start = (size_job * k + 1)
      end = min( size_job * ( k + 1 ) , nnz( pa$A)  )
    
      if(start>=end){
        inds = c()
      }else{
        inds = start:end      
      }
      pa_tmp$A = sparseMatrix(i=r[inds], j=c[inds],x=v[inds], dims=c(n,n))
      save(pa_tmp, file = file_name)
    }else{
      message = paste("The file: ", file_name, " already exists.  Skipping...")
      print(message)
    }
  }

}
