ltwoinf <-
function(XX){
  d = sqrt(max(apply(XX^2,2,sum)))
  return(d)
}
