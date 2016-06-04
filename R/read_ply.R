read_ply <-
function(filename){ # In-progress function to read ply files and convert to auto3dgm data
  file = suppressWarnings(read.table(filename, sep="" , stringsAsFactors=F, fill=T, colClasses = rep('character',4) ) )
  
  # Check for valid ASCII PLY file
  if(file[1,1] != "ply"){stop("Not a valid ASCII PLY file")}
  
  end_header_line = which(file[,1] == "end_header")
  
  header = file[1:end_header_line,]
  data = file[end_header_line+1:dim(file)[1],]
  
  elements = header[which(header[,1] == "element"),]
  n.V = as.numeric(elements[which(elements[,2] == "vertex"),3])
  n.F = as.numeric(elements[which(elements[,2] == "face"),3])

  Verts = data.matrix(file[(end_header_line+1):(end_header_line+n.V),(1:3)])
  Faces = data.matrix(file[(end_header_line+n.V+1):(end_header_line+n.V+n.F),2:4])
  
  Faces = Faces + 1;
  
  Faces = t(Faces)
  Verts = t(Verts)
  VF = list(Verts, Faces)
  return(VF)
}