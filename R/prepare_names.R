prepare_names <-
  function(Names, Ids){
    if (is.null(Names)){
      return(substr(Ids, 1, nchar(Ids)-4))
    } else{
      if (length(Names) == length(Ids)){
        return(Names)
      } else{
        stop("Error. Length of Names vector and length of Ids vector not equal")
      }
    }
  }