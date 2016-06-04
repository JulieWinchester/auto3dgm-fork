write_aligned_files <-
function(ds, ga, input_dir, output_dir, scale){
  #rotate the individual raw files.  Use the rotation matrices from global alignment.  
  for (kk in 1: ds$n){
    VF = read_off(file.path(input_dir,ds$ids[kk]))
    V = VF[[1]]
    FF = VF[[2]]
    
    #Scale according to highest resolution point cloud
    V = V - repmat(ds$shape[[kk]]$center,1,dim(V)[2])
    if(scale){
      V = V / (ds$shape[[kk]]$scale / sqrt(ds$N[ds$K]) )
    }
    
    #Rotate according to ga
    if (det(ga$R[[kk]]) <0){ FF = FF[c(2,1,3),]} 
    V = ga$R[[kk]]%*%V
    
    write_off(file.path(output_dir,ds$ids[kk]),V,FF,FALSE)

  }
  
}
