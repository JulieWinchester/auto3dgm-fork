write_morphologika <-
function(filename, ds, ga, varargin){
  
  nargin = 3 + length(varargin)
  remove = NULL
  #Need to check this.  Maybe the 
  if(nargin==4){remove=varargin[[1]]}
  ind = 1:ds$n
  if(!is.null(remove)){ ind = ind[-remove]}
 
  nn = length(ind)
  
  File=NULL
  File=add(File,'[Individuals]')
  File=add(File,nr(nn))
  File=add(File,'[landmarks]')
  File=add(File,nr(ds$N[ga$k]))
  File=add(File,'[dimensions]')
  File=add(File,nr(3))
  File=add(File,'[names]')
  
  for (ll in 1:nn){File= add(File,ds$names[[ ind[ll] ]] ) }
  File=addnl(File)
  File=add(File,'[rawpoints]')
  
  for (ll in 1:nn){
    File=addnl(File)
    message = paste("Adding bone ", format(ind[ll]), sep="")
    print(message)
    File=add(File,paste("'", ds$names[[ind[ll]]], sep="") )
    File=addnl(File)
    
    V = ga$R[[ ind[ll] ]] %*% ds$shape[[ ind[ll] ]]$X[[ga$k]] %*% ga$P[[ ind[ll] ]]
    
    for (vv in 1:ds$N[ga$k]){
      string = paste(nre(V[1,vv]), nre(V[2,vv]), nre(V[3,vv]), sep=" " )
      File=add(File,string)
    }
  }
  
  
  sink(filename)
  cat(File)
  sink()
  
  
}
