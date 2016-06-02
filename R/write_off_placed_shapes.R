write_off_placed_shapes <-
function(filename, coords, ds, ga, varargin){
  nargin = 4 + length(varargin)
  if(size(coords,1)==2){
    coords = rbind(coords, rep(0,size(coords,2)))
  }  
  
  view_rot = diag(rep(1,3))
  if(nargin>=5){view_rot = varargin[[1]]}
  
  dbp = squareform(t(coords))
  mst = graphminspantree(dbp)
  tree = mst
  
  if(nargin>=6){tree = varargin[[2]]}
  
  #compute adequate scale for resizing the lowres meshes
  tree = (tree>0)*dbp
  RC = which(tree!=0, arr.ind=T)
  r = RC[,1]
  c = RC[,2]
  v = rep(NA, length(r))
  
  for (jj in 1:length(r)){
    v[jj] = tree[r[jj],c[jj]]
  }
  sc = mean(v)/8
  
  #create vertices and faces of resulting OFF file
  
  V = NULL; FF = NULL;
  
  for (ii in 1:ds$n){
      if( det(ga$R[[ii]])<0 ){
        FF = cbind(FF, ( ds$shape[[ii]]$lowres$FF[c(2,1,3),] + size(V,2) ) )
      }else{
        FF = cbind(FF, ( ds$shape[[ii]]$lowres$FF + size(V,2) ) )
      }
    
    V = cbind(V, (sc*view_rot %*% ga$R[[ii]] %*% ds$shape[[ii]]$lowres$V + repmat(as.matrix(coords[,ii]), 1, size(ds$shape[[ii]]$lowres$V,2) ) ) )
  }
  
  
  g = .125*sc
  Vl = diag(c(g,g,1)) %*% matrix(c(1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, -1, 1, 1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1), byrow=T, nrow=3)
  Fl = matrix(c(1, 4, 3, 3, 3, 6, 2, 2, 4, 8, 8, 7, 6, 3, 5, 1, 5, 5, 4, 8, 7, 2, 6, 5), byrow=T, nrow=3)

  for (kk in 1:length(r)){
    s = coords[,r[kk]]
    e = coords[,c[kk]]
    Vedge = diag(c(1,1,v[kk])) %*% Vl
    tmpv = (e-s)/v[kk]
    QR = qr(cbind(tmpv, matrix(rnorm(6, 0, 1), nrow=3) ) )
    Rot = qr.Q(QR)
    tmp = qr.R(QR)
    
    if(Rot[1,1]/tmpv[1] < 0){Rot = -Rot}
    if( det(Rot)<0 ){Rot = Rot %*% matrix(c(1,0,0,0,0,1,0,1,0), nrow = 3, byrow=T) }
    Rot = Rot %*% matrix( c(0,0,1,0,1,0,1,0,0), nrow=3, byrow=T)
    
    Vedge = Rot %*% Vedge + repmat(as.matrix(s - c(0,0,-.5*g) ), 1, size(Vedge,2))
    FF = cbind(FF, Fl + size(V,2))
    V = cbind(V, Vedge)
  }
  
  write_off(filename, V, FF, FALSE)
}
