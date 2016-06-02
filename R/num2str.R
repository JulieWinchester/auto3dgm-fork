num2str <-
function(x, n){
  leading_zeros = rep(0,n-n_digits(x))
  leading_zeros = as.character(leading_zeros)
  leading_zeros = paste(leading_zeros, collapse="")
  str = paste(leading_zeros, as.character(x), sep="")
  return(str)
}
