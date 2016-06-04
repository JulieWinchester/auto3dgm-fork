prepare_ids <-
  function(Ids, Data_dir){
    if (is.null(Ids)){
      if (substring(Data_dir, 1, 4)=="http"){
        stop("Error. When loading data from a URL, Ids must be provided")
      }else{ # Get list of files if Data_dir is not URL (i.e., local dir)
        if (file.exists(Data_dir) ){
          Ids = list.files(path = Data_dir, pattern = "\\.[oO][fF][fF]$")
          return(Ids)
        }else{
          stop("Error. The local data directory does not exist")
        }  
      }
    }else{ # Process list of files if Ids vector is provided
      Ids = sapply(Ids, function(x) ifelse(substr(tolower(x), nchar(x)-3, nchar(x)) == ".off", 
                                               x, paste(x,".off",sep="")), USE.NAMES = FALSE)
      return(Ids)
    }
  }