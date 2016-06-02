addnl <-
function(File){
  #note tht the <<- notation declares file as a global variable
  File =paste(File,"\n",sep="")
  return(File)
}
