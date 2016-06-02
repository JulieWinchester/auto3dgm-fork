rm(list=ls())
library(auto3dgm)

Data_dir = "http://stat.duke.edu/~sayan/auto3dgm/data/meshes/teeth_dataset"
Output_dir = "/Users/christopherglynn/Dropbox/Output"
Levels=c(64,128)
Ids = c('001','002','003')
Names = c('a01','a02','a03')

Output = align_shapes(Data_dir, Output_dir, Levels, Ids, Names)

