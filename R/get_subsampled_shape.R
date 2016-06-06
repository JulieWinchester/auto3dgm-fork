get_subsampled_shape <-
function(dir, id, NN){
  
  URL = F
  if(substring(dir, 1, 4)=="http"){URL=T}
  
  id = as.character(id) 
  sub_off_fn = file.path(dir,'subsampled', paste(substr(id, 1, nchar(id)-4),".off",sep=""))
  mesh_fn = file.path(dir, id)
  
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
      mesh_fn_type = substring(tolower(mesh_fn), nchar(mesh_fn)-3, nchar(mesh_fn))
      
      if(mesh_fn_type == ".off"){
        VF = read_off(mesh_fn)
      }else if(mesh_fn_type == ".ply"){
        VF = read_ply(mesh_fn)
      }else{
        stop("Error. Unrecognized mesh file type.")
      }
      
      V = VF[[1]]; Fa = VF[[2]];
      ind = subsample(V,NN,X)
      X = V[,ind]
      sub_dir = file.path(dir,'subsampled')
      if(!file.exists(sub_dir)){
        dir.create(sub_dir, showWarnings=T)
      }
      
      write_off(sub_off_fn,X,matrix(c(1,2,3),ncol=1),0) 
    }
  }
  return(X)
}
