get_subsampled_shape <-
function(dir, id, NN){
  
  URL = F
  if(substring(dir, 1, 4)=="http"){URL=T}
  
  
  sub_off_fn = paste(dir,'/subsampled/', as.character(id), '.off', sep='')
  off_fn = paste(dir,'/', as.character(id), '.off', sep='')
  
  if(URL){
    if (url.exists(sub_off_fn) ){
      VF = read_off(sub_off_fn)
      X = VF[[1]]
      tmp = VF[[2]]
      n_subsampled_pts = dim(X)[2]   
    }else{
      print("Error.  The URL does not exist")
    } 
  }else{
    if (file.exists(sub_off_fn) ){
      VF = read_off(sub_off_fn)
      X = VF[[1]]
      tmp = VF[[2]]
      n_subsampled_pts = dim(X)[2]   
    }else{
      X = c()
      n_subsampled_pts=0
    }
    
    if(n_subsampled_pts < NN){
      VF = read_off(off_fn); V = VF[[1]]; Fa = VF[[2]];
      ind = subsample(V,NN,X)
      X = V[,ind]
      sub_dir = paste(dir,'subsampled',sep='/')
      if(!file.exists(sub_dir)){
        dir.create(sub_dir, showWarnings=T)
      }
      
      write_off(sub_off_fn,X,matrix(c(1,2,3),ncol=1),0) 
    }
  }
  return(X)
}
