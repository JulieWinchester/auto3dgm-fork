# Error handler that suppresses messages while returning errors and warnings
# Intended for use with url.exists()
message_error_handler <-
function(func) {
  suppressMessages(tryCatch({
    func
  }, warning = function(w) {
    return(w)
  }, error = function(e) {
    return(e)
  }))
}


