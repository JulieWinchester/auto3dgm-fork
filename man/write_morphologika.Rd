\name{write_morphologika}
\alias{write_morphologika}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
write_morphologika
}
\description{
This function writes a text file of globally aligned shapes to be opened with Morphologika}
\usage{
write_morphologika(filename, ds, ga, varargin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
The name of the file to be written.
}
  \item{ds}{
The dataset,ds, as returned by the align_shapes function
}
  \item{ga}{
The global alignment, ga, as returned by the align_shapes function
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

varargin = list(1:ds$n, 10, rotation_matrix,3.0,1)
file = paste(ds$output_dir,"/morphologika.txt", sep="")
write_morphologika(file, ds, ga, varargin)
}

