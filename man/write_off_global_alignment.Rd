\name{write_off_global_alignment}
\alias{write_off_global_alignment}
\title{
write_off_global_alignment
}
\description{
Writes a global alignment file that includes all shapes globally aligned in '.off' format. 
}
\usage{
write_off_global_alignment(filename, ds, ga, varargin)
}
\arguments{
  \item{filename}{
  Name of file to write.  
}
  \item{ds}{
The dataset, ds, returned by the align_shapes function.
}
  \item{ga}{
The global alignment, ga, of the dataset.  
}
  \item{varargin}{
This is a list of advanced graphical parameters. Specifying varargin= list(1:ds$n, 10, rotation_matrix,3.0,1)  The individual elements are (in order): (1) the indices of the shapes in the dataset to output, (2) The number of shapes per row, (3) a rotation_matrix to change the orientation of the data for visual display, (4) a graphical offset to separate the shapes, and (5) the index of a reference shape.  
}
}

\examples{

Data_dir = "http://stat.duke.edu/~sayan/auto3dgm/data/meshes/teeth_dataset"
Output_dir = "/Users/christopherglynn/Dropbox/Output"
Levels=c(64,128)
Ids = c('001','002','003')
Names = c('a01','a02','a03')

Output = align_shapes(Data_dir, Output_dir, Levels, Ids, Names)


ds = Output[[1]]
ga = Output[[2]]

k = 2;
theta = pi/2
rotation_matrix = matrix(
c(cos(theta), -sin(theta), 0, sin(theta), cos(theta), 0,
0, 0, 1), nrow=3, byrow=TRUE)

rotation_matrix = rotation_matrix%*%
  matrix(c(0,0,1,0,-1,0,1,0,0), nrow=3,byrow=TRUE)%*%
  t(ds$shape[[1]]$U_X[[k]])



fix_dir = paste(ds$ms$output_dir,"/Alignment_Fix", sep="")
unlink(fix_dir, recursive=TRUE)
dir.create(fix_dir)

varargin = list(1:ds$n, 10, rotation_matrix,3.0,1)
file = paste(fix_dir,"/fixed_alignment.off", sep="" )
write_off_global_alignment(file, ds, ga, varargin)

}

