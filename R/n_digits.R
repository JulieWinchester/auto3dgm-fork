n_digits <-
function(x){
  x = as.character(x)
  n = length(unlist(strsplit(x, split="") ) )
  return(n)
}
