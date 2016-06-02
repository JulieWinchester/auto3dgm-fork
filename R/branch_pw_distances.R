branch_pw_distances <-
function(A_index,B_index, ds, pa, k ,fix_dir_AB){
  
  

  ## Compute rotation matrix for graphical outputs
  theta = pi/2
  rotation_matrix = matrix(c(cos(theta), -sin(theta), 0, sin(theta), cos(theta), 0, 0, 0, 1), nrow=3, byrow=T)
  rotation_matrix = rotation_matrix%*%matrix(c(0,0,1,0,-1,0,1,0,0), nrow=3, byrow=T)%*%t(ds$shape[[1]]$U_X[[k]])


  ##Store the pairwise rotation matrices in ga
  ga=list(R=list())
  for (i in 1:ds$n){
    ga$R = list.add(ga$R, diag(3))
  }
  M = length(A_index)*length(B_index)
  From = rep(A_index, each = length(B_index))
  To = rep(B_index, times = length(A_index))
  pairs = cbind(From, To)
  distance = rep(NA, M)
  for (ii in 1:M){
    hh = min(pairs[ii,])
    kk = max(pairs[ii,])
    distance[ii] = pa$d[hh,kk]
  }
  
  d = cbind(distance, pairs)
  d = d[order(distance),]
  
  ##Output a file with all pairwise comparisons.  The base will be in its typical position
  #and the other shapes will be pairwise aligned to the baise.  

  #Create a directory for all of these files
  unlink(fix_dir_AB, recursive=TRUE)
  dir.create(fix_dir_AB)

  #Now output individual mesh files for pairwise aligned shapes.  
  
  for (ii in 1:dim(d)[1]){
    From = d[ii,2]
    To = d[ii,3]
    
      if(From<To){
        pw_rotation = pa$R[[ From ]][[ To ]]
      }else{
        pw_rotation = solve( pa$R[[ To ]][[ From ]] )
      }
      
      ga$R[[ To ]] = pw_rotation
      varargin = list(c(From, To), ds$n, rotation_matrix,3.0,1)
      file = paste(fix_dir_AB,"/alignment_",ii,"_shapes_",ds$ids[From],"_",ds$ids[To], ".off", sep="" )
      write_off_global_alignment(file, ds, ga, varargin)
  }


  return(d)
}
