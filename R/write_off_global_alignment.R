write_off_global_alignment <-
function(filename, ds, ga, varargin){
  #parameters
  inds = 1:ds$n
  nargin = 3 + length(varargin) #this is a matlab translation.  3 fixed args + length of variable arg
  
  if(nargin>=4){
    inds = varargin[[1]]
  }
  
  per_row = floor(sqrt(length(inds)))
  
  if(nargin>=5){
    per_row =varargin[[2]]
  }
  
  view_rot = diag(c(1,1,1))
  
  if(nargin>=6){
    view_rot = varargin[[3]]
  }
  
  offset = 3.0
  
  if(nargin>=7){
    offset = varargin[[4]]
  }
  
  reference=1
  
  if(nargin>=8){
    reference = varargin[[5]]
  }
  
 #Create vertices
  V = NULL
  FF = NULL
  
  for (ii in 1:length( inds ) ){
    if ( det( ga$R[[ inds[ii] ]] ) < 0 ){ # Need to invert faces orientation
      FF = cbind(FF, ds$shape[[ inds[ii] ]]$lowres$FF[c(2,1,3),] + size(V,2)  )
      }else{
      FF =  cbind(FF,  ds$shape[[ inds[ii] ]]$lowres$FF + size(V,2)  );
      }
    
    r = floor( (ii-1) / per_row )
    c = (ii-1) - r * per_row
    V = cbind( V,  view_rot%*%ga$R[[ inds[ii] ]] %*% ds$shape[[ inds[ii] ]]$lowres$V + repmat( matrix(c(offset*c, 0, offset*r), ncol=1) , 1, size( ds$shape[[ inds[ii] ]]$lowres$V , 2 )) )

  }
  
  if(reference){
    c_mat =t( matrix(c(1,1,1,-1,1,-1,-1,-1,1, 1, -1, -1), nrow = 4, byrow=TRUE) )
    tmpV = f_center(c_mat)
    tmpFF = t(matrix(c(0,1,2,0,2,3,0,3,1,3,2,1), nrow=4, byrow=TRUE))+1
  
    FF = cbind(FF, (tmpFF+size(V,2)) )
    V = cbind(V, (tmpV - repmat(matrix(c(offset, 0, offset), ncol=1), 1, size(tmpV,2) )))
    FF = cbind(FF, (tmpFF+size(V,2)) )
    V = cbind(V, (.5*tmpV - repmat(matrix(c(0, 0, offset), ncol=1), 1, size(tmpV,2) )))
  
  }
  #renormalize is FALSE
  write_off(filename,V,FF, FALSE)
}
