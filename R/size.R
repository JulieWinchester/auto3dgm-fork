size <-
function(A,margin){
  if(is.null(dim(A)) ){
    n = length(A)
  }else{
    n = dim(A)[margin]
  }
  return(n)
}
