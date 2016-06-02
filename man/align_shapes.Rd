\name{align_shapes}
\alias{align_shapes}
\title{
align_shapes
}
\description{
This function implements a fully-automated algorithm for aligning 3-dimensional shapes. The function returns the dataset constructed from the shape files, the global alignment matrices, and the pairwise alignment matrices.   Outputs such as a globally aligned file containing all the shapes, a jpeg of the Minimum Spanning Tree connecting the shapes, and individually aligned shape-files are written to the output directory. 
}
\usage{
align_shapes(Data_dir, Output_dir, Levels, Ids, Names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data_dir}{
The path to mesh files.  They must be in the '.off' format.  This directory also includes subdirectories for low resolution files and subsampled files.       
}
  \item{Output_dir}{
The path to the folder where all the program outputs will be written.    	
}
  \item{Levels}{
  An integer vector of two subsampling levels for shape landmarks.    
}
  \item{Ids}{
  A character vector of identifiers for the shape files in the dataset directory Data directory.  The identifiers must correspond exactly to the file names.  For two files in the directory "001.off" and "001.off", Ids = c('001', '002'). 
}
  \item{Names}{
  A character vector of names for the shapes represented by the shape files.
}
}

\examples{

Data_dir = "http://stat.duke.edu/~sayan/auto3dgm/data/meshes/teeth_dataset"

Output_dir = "/Users/christopherglynn/Dropbox/Output"
Levels=c(64,128)
Ids = c('001','002','003')
Names = c('a01','a02','a03')

Output = align_shapes(Data_dir, Output_dir, Levels, Ids, Names)


}
