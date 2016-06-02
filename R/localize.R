localize <-
function(ga){
  n = size(ga$R, 2)
  
  pa = list(R = list(), P = list())
  for (ii in 1:n){
    pa$R = list.add(pa$R, list())
    pa$P = list.add(pa$P, list())
    for (jj in 1:n){
      pa$R[[ii]] = list.add(pa$R[[ii]],t(ga$R[[ii]])%*%ga$R[[jj]] )
      pa$P[[ii]] = list.add(pa$P[[ii]],ga$P[[jj]]%*%t(ga$P[[ii]]) )
      
    }
  }
  return(pa)
}
