read_off <-
function(filename){
  file = suppressWarnings(read.table(filename, sep="" , stringsAsFactors=F, fill=T, colClasses = rep('character',4) ) )
  names(file)=c("V1", "V2", "V3", "V4")
  #check to see that this is a valid OFF file
  line1 = file[1,]
  line2 = file[2,]
  
  file = file[3:dim(file)[1],]
  if(line1[1]!="OFF" & line1[2]!="OFF" & line1[3]!="OFF" & line1[4]!="OFF"){
    print("Not a valid OFF file")
    return()
  }
  
  n.V = as.numeric(line2[1])
  n.F = as.numeric(line2[2])

  Verts = data.matrix(file[(1:n.V),1:3])
  Faces = data.matrix(file[(n.V+1):(n.V+n.F),2:4])
  
  
  Faces = Faces + 1;
  
  Faces = t(Faces)
  Verts = t(Verts)
  VF = list(Verts, Faces)
  return(VF)
}
