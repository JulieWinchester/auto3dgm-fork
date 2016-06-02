align_shapes <-
function(Data_dir, Output_dir, Levels, Ids, Names, Mirror=1){

##############################################################################################
# R Code for Shape Alignment
# Chris Glynn, Jesus Puente, Doug Boyer,Sayan Mukherjee, Ingrid Daubechies, and Justin Gladman
# Departments of Statistical Science, Mathematics, and Evolutionary Anthropology
# Duke University
# September 6, 2013
##############################################################################################

#------------------------------------------------------------------------------------------

ds = list(N=c(), ids=c(), names=c(), n = NA, K = NA, msc = list(mesh_dir=NA, output_dir=NA), shape=list())

ds$N = Levels
ds$ids = Ids
ds$names = Names




#-------------YOU DO NOT NEED TO MODIFY ANYTHING AFTER THIS POINT ------------------------#

#Variables not to be changed

ds$n = length(ds$ids)
ds$K = length(ds$N) 

#ds.msc.general_dir


ds$msc$mesh_dir = Data_dir
ds$msc$output_dir = Output_dir

if(!file.exists(ds$msc$output_dir)){
  dir.create(ds$msc$output_dir)
}


#Initialization

#Fill in X with subsampled shapes
#Center and Standardize them
#Compute Singular Value Decompositions

mat = matrix(nrow=1, ncol=1)
shape.prototype = list(list(mat,mat), matrix(nrow=3, ncol=1), NA, c(NA, NA, NA), list(mat,mat), list(mat,mat), list(mat,mat), list(mat), list(V = mat, FF = mat)  )
names(shape.prototype) = c("X", "center", "scale", "epsilon", "U_X", "D_X", "V_X", "neigh", "lowres")


for (ii in 1:ds$n){
  
  ds$shape=list.add(ds$shape,shape.prototype)
  
  
  ds$shape[[ii]]$X[[ ds$K ]] = get_subsampled_shape(ds$msc$mesh_dir, ds$ids[ii], ds$N[ ds$K ])
  ds$shape[[ii]]$center = matrix(apply(ds$shape[[ii]]$X[[ ds$K ]], 1, mean), nrow=3, ncol=1)
  ds$shape[[ii]]$scale=f_scale(ds$shape[[ii]]$X[[ ds$K ]])
  ds$shape[[ii]]$epsilon = rep(0, ds$K)
  
  for (kk in 1:ds$K){
    ds$shape[[ii]]$X[[kk]] = ds$shape[[ii]]$X[[ ds$K ]][ , (1:ds$N[kk]) ]
    ds$shape[[ii]]$X[[kk]] = f_center(ds$shape[[ii]]$X[[kk]])/f_scale(ds$shape[[ii]]$X[[kk]])
    SVD = svd(ds$shape[[ii]]$X[[kk]])
    ds$shape[[ii]]$U_X[[kk]] = SVD$u
    tmpD_X = SVD$d
    tmpV_X = SVD$v
    ds$shape[[ii]]$D_X[[kk]] = diag(tmpD_X)
    ds$shape[[ii]]$V_X[[kk]] = tmpV_X[,1:3] 
  }
  
  for (kk in 2:ds$K){
    ds$shape[[ii]]$epsilon[kk] = 1.0001*hausdorff(ds$shape[[ii]]$X[[kk]][, (1:ds$N[kk-1]) ], ds$shape[[ii]]$X[[kk]] )[[1]]
    M.MD2= crangesearch(ds$shape[[ii]]$X[[kk]][, (1:ds$N[kk-1])],ds$shape[[ii]]$X[[kk]],ds$shape[[ii]]$epsilon[kk]) 
    ds$shape[[ii]]$neigh[kk] = M.MD2[[1]]  
  }
}

ds_unscaled = ds
for (ii in 1:ds$n){
  for (kk in 1:ds$K){
    ds_unscaled$shape[[ii]]$X[[kk]] = (ds$shape[[ii]]$scale / sqrt( ds$N[ ds$K ] )) * ds$shape[[ii]]$X[[kk]]
  }
}
##########################################################
#Read the low resolution files.  For display purposes only.  

for (ii in 1:ds$n){
  #Read the files
  lowres_off_fn = paste(ds$msc$mesh_dir, "/lowres/", ds$ids[ii], ".off", sep="")
  if (file.exists( lowres_off_fn ) || url.exists(lowres_off_fn) ){
    tmpVF= read_off(lowres_off_fn)
    ds$shape[[ii]]$lowres$V = tmpVF[[1]] 
    ds$shape[[ii]]$lowres$FF = tmpVF[[2]]
    
    ds$shape[[ii]]$lowres$V = ds$shape[[ii]]$lowres$V - repmat(ds$shape[[ii]]$center,1,dim(ds$shape[[ii]]$lowres$V)[2]);
    ds$shape[[ii]]$lowres$V = ds$shape[[ii]]$lowres$V / ( ds$shape[[ii]]$scale / sqrt( ds$N[ ds$K ] ) );
    
    
  }else{
    print(paste("Cannot find low resolution file: ", lowres_off_fn,sep="") )
  }
  
  
}


# Alignment
# 'pa' stands for pairwise alignment
# 1. Compute a pairwise alignment of all pairs, then compute minimum
#    spanning tree
pa = list()
pa = list.add(pa, upper_triangle(ds$n) )  # a 1 entry in this matrix indicates the pairwise distance should be computed
pa = list.add(pa,8) # Number of positions to test, the first 8 are the 8 possibilities for aligning the principal axes
pa = list.add(pa,paste(ds$msc$output_dir, "/jobs/", sep="") )
pa = list.add(pa, Mirror) # Boolean, 1 for 8 alignment possibilities (may mirror aligned specimens) or 0 for 4 (no mirror) 
names(pa)=c("A","L","pfj","M")



k = 1;

# Break up all the pairwise distances into a a bunch of different
# computations, to be computed either in the same machine or in different
# ones
# Remember to remove all previous jobs in the output/jobs folder!
unlink(paste(ds$msc$output_dir,"/jobs", sep=""), recursive=TRUE)
dir.create(paste(ds$ms$output_dir,"/jobs", sep=""))

compute_alignment(ds, k, pa , 1, FALSE )

pa = reduce( ds, pa, 1 );
pw_rotations = pa;

mst = graphminspantree(pa$d + t(pa$d))
ga = globalize(pa, mst + t(mst), 2)
ga = list.add(ga, k)
names(ga)=c(names(ga)[1:2],"k")

plot_tree(mst, ds$names, "Minimum Spanning Tree")

#now output the graph
jpeg(paste(ds$msc$output_dir,"/MST.jpg",sep=""), height=625, width=625)
plot_tree(mst, ds$names, "Minimum Spanning Tree")
dev.off()


theta = pi/2
rotation_matrix = matrix(c(cos(theta), -sin(theta), 0, sin(theta), cos(theta), 0, 0, 0, 1), nrow=3, byrow=T)
rotation_matrix = rotation_matrix%*%matrix(c(0,0,1,0,-1,0,1,0,0), nrow=3, byrow=T)%*%t(ds$shape[[1]]$U_X[[k]])

#This will write the aligned files
unlink(paste(ds$msc$output_dir, "/", "Aligned_Shapes", sep=""), recursive=TRUE)
dir.create(paste(ds$msc$output_dir, "/", "Aligned_Shapes", sep=""))
write_aligned_files(ds, ga, ds$msc$mesh_dir, paste(ds$msc$output_dir, "/", "Aligned_Shapes", sep=""), FALSE )

#create the variable argument for write_off_global_alignment
varargin = list(1:ds$n, 10, rotation_matrix,3.0, 1 )
write_off_global_alignment(paste(ds$msc$output_dir,"/alignment.off", sep="" ), ds, ga, varargin)

varargin= list()
write_morphologika(paste(ds$msc$output_dir, "/morphologika.txt", sep=""), ds, ga, varargin)
write_morphologika(paste(ds$msc$output_dir, "/morphologika_unscaled.txt", sep=""), ds_unscaled, ga, varargin)



save(ds, pa, ga, mst, file = paste(ds$msc$output_dir, "/session.RData", sep=""))

##--------------------------------------------------------------------------------
##--------------------------------------------------------------------------------
##--------------------------------------------------------------------------------
##--------------------------------------------------------------------------------
##--------------------------------------------------------------------------------


#Compute the edges in the MST with higher number of points.

pa_tmp = localize(ga)
pa$R = pa_tmp$R

k = 2 #Which level to run next
pa$A = upper_triangle(ds$n)
pa$pfj = paste(ds$msc$output_dir, '/jobs/', sep="")
tmpR = pa$R
tmpP = pa$P

#Remember to remove all previous jobs in the output/jobs folder
#Auto delete files from output/jobs folder
unlink(paste(ds$msc$output_dir,"/jobs", sep=""), recursive=TRUE)
dir.create(paste(ds$msc$output_dir,"/jobs", sep=""))

compute_alignment(ds, k, pa , 1, FALSE )
pa = reduce(ds, pa, 1)


#mst is same as before.
ga = globalize(pa, mst, 1)

ga = list.add(ga, k)
names(ga)=c(names(ga)[1:2],"k")

#output higher resolution
#create the variable argument for write_off_global_alignment
varargin = list(1:ds$n, 10, rotation_matrix,3.0, 1 )
write_off_global_alignment(paste(ds$msc$output_dir,"/alignment_2.off", sep="" ), ds, ga, varargin)

varargin= list()
write_morphologika(paste(ds$msc$output_dir, "/morphologika_2.txt", sep=""), ds, ga, varargin)
write_morphologika(paste(ds$msc$output_dir, "/morphologika_2_unscaled.txt", sep=""), ds_unscaled, ga, varargin)

save(ds, pa, ga, mst, file = paste(ds$msc$output_dir, "/session_2.RData", sep=""))

#Compute all pairwise Procrustes distances

proc_d = matrix(rep(0, ds$n^2), nrow=ds$n)

for (ii in 1:ds$n){
  for (jj in ii:ds$n){
    if(ii==jj){next}
    PRO = cprocrustes(ds$shape[[ii]]$X[[k]] %*% ga$P[[ii]], ds$shape[[jj]]$X[[k]] %*% ga$P[[jj]] )
    tmpR = PRO[[1]]
    proc_d[ii,jj]=PRO[[2]]
  }
}

mst_proc_d = graphminspantree(proc_d + t(proc_d) )
plot_tree(mst_proc_d, ds$names, "Minimum Spanning Tree")

proc_d = .5*(proc_d + t(proc_d))

#very close to what Matlab gives but not exact.  Not sure why.
#non-metric MDS in each case.  Need to take the negative to get it close.    
coords = -t(isoMDS(proc_d, k=2, maxit=200, tol=1e-4)$points)

varargin = list(diag(c(1,1,1)), mst_proc_d)
filename = paste(ds$msc$output_dir, "/map.off", sep="")
write_off_placed_shapes(filename, coords, ds, ga, varargin)



Retx = list(ds, ga, pa)
return(Retx)
}
