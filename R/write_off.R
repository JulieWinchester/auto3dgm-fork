write_off <-
function(filename, vertex, face, renormalize){

if(dim(vertex)[2]!=3){vertex = t(vertex)}
if(dim(vertex)[2]!=3){return("Vertex matrix is not the correct format")}

if (dim(face)[2]!=3){face = t(face)}
if (dim(face)[2]!=3){return("Face matrix is not the correct format")}

Verts = vertex
Faces = face

Faces = Faces - 1


n.V = dim(Verts)[1]
n.F = dim(Faces)[1]


#If renormalize is TRUE

if(as.numeric(renormalize)==1){
m = apply(Verts, 1, mean)
std = apply(Verts,1, sd)
Verts = Verts - m/std
}

Verts =formatC(Verts, digits = 6, format="f")

sink(filename)
cat("OFF\n")
cat(as.character(c(n.V, n.F, "0")) )
cat("\n")
for (i in 1:n.V) {
  cat(paste(Verts[i,],collapse=" ")) 
  cat("\n")
}

if(n.F>1){
  for (i in 1:(n.F-1)) {
    cat(paste("3",paste(Faces[i,],collapse=" "),sep=" ") )
    cat("\n")
  }
}

cat(paste("3",paste(Faces[n.F,],collapse=" "),sep=" ") )
sink()

return( print(paste("File successfully written to: ", filename, sep="")) )
}
