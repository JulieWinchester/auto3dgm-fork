compute_alignment <-
function(ds, k, pa, n_jobs, use_cluster){
  n=dim(pa$A)[2]
  map(pa, n_jobs) #save n_jobs files to be processed
  
  if(!use_cluster){
    for (kk in 1:n_jobs){
      KK = kk-1
      arg1 = paste(pa$pfj, "job_", num2str(KK,4), ".RData", sep="")
      arg2 = paste(pa$pfj, "ans_", num2str(KK,4), ".RData", sep="")

      process_job(arg1, arg2, ds, k, pa)
      
    }
  }else{
    #submit_jobs(pa, n_jobs)
    #this is the parallel computing option.  Not going to develop this right now. 
  }
  return(print("Pairwise-alignment computed!"))
}
