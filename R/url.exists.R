url.exists <-
function(URL, suppressMsgError = F) {
  out <- tryCatch(
{
  test = url(URL, open = "", blocking = TRUE,encoding = getOption("encoding"))
  close(test)
  return(TRUE)
},
error=function(cond) {
  if (suppressMsgError == F) {
    message(paste("URL does not exist:", URL))
    message("Original error message:")
    message(cond)
  }
  # Choose a return value in case of error
  return(FALSE)
},
warning=function(cond) {
  if (suppressMsgError == F) {
    message(paste("URL produced a warning:", URL))
    message("Original warning message:")
    message(cond)
  }
  return(NULL)
}
  )    
  return(out)
}
