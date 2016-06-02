f <-
function(ii,jj, ds, LEVEL, pa){
  if(LEVEL==1){
    gpd(  ds$shape[[ii]]$X[[LEVEL]] , ds$shape[[jj]]$X[[LEVEL]], pa$L, pa$M )
  }else{
    locgpd(  ds$shape[[ii]]$X[[LEVEL]] , ds$shape[[jj]]$X[[LEVEL]], pa$R[[ii]][[jj]], matrix(rep(1, ds$N[LEVEL]^2), nrow=ds$N[LEVEL]) ) 
  }
  
}
