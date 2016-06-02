upper_triangle <-
function(nn){ 
  row_index = list()
  col_index = list()
  for (row in 1:(nn-1) ){
    row_index = list.add(row_index, rep(row, (nn-row)) )
    col_index = list.add(col_index, (row+1):nn )
  }
  
  row_index = unlist(row_index)
  col_index = unlist(col_index)
  entries = rep(1, length(row_index))
  A = sparseMatrix(i=row_index, j=col_index, x=entries, dims=c(nn,nn))
  return(A)
}
