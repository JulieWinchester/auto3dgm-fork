diff_perms <-
function(P1,P2){
  return(sum(colSums(abs(P1-P2),c(1,2))) )
}
