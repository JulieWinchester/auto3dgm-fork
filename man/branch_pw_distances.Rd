\name{branch_pw_distances}
\alias{branch_pw_distances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
branch_pw_distances
}
\description{
branch_pw_distances returns the procrustes distance between each pair of elements in two subsets of the shapes. The outputs include a csv file of the procrustes distances and an alignment file for each corresponding pair. This function helps the user identify the best pairwise alignments between one element of a branch and remaining tree.  
}
\usage{
branch_pw_distances(A_index, B_index, ds,pa,k, fix_dir_AB)
}
\arguments{
  \item{A_index}{
The index of elements from the Names vector that are in Branch A.  
}
  \item{B_index}{
The index of elements from the Names vector that are in Branch B.
}
  \item{ds}{
The dataset containing shapes to to compare.    
}
  \item{pa}{
The pairwise alignment data object
  }
\item{k}{
Level of subsampling (1 or 2)
}
  \item{fix_dir_AB}{
The path of a directory where output files should be written.  It is recommended to create a subdirectory of the output directory: "/output/fix_dir_AB"
}
}


\examples{

Data_dir = "http://stat.duke.edu/~sayan/auto3dgm/data/meshes/teeth_dataset"
Output_dir = "/Users/christopherglynn/Dropbox/Output"

Levels=c(64,128)
Ids = c('001','002','003')
Names = c('a01','a02','a03')

Output = align_shapes(Data_dir, Output_dir, Levels, Ids, Names)


ds = Output[[1]]  #The full dataset
ga_full=Output[[2]] #The global alignment
pa=Output[[3]]  #The pairwise alignments

fix_dir = paste(ds$ms$output_dir,"/Alignment_Fix", sep="")
unlink(fix_dir, recursive=TRUE)
dir.create(fix_dir)


A_index =c(1)
B_index = c(2,3)

fix_dir_AB = paste(fix_dir,"AB",sep="/")
k=1
AB = branch_pw_distances(A_index,B_index,ds,pa, k, fix_dir_AB)
}

