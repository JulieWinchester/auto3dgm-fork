sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
       if(trace) cat(nm,":")           
       source(file.path(path, nm), ...)
       if(trace) cat("\n")
    }
}

require(MASS)
require(clue)
require(igraph)
require(Matrix)
require(linprog)

sourceDir(file.path(dirname(sys.frame(1)$ofile),"R"))
remove(sourceDir)
