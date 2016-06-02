plot_tree <-
function(tree, names, title){
#This is slightly different than the different methods that Jesus' code allows. 
tree = tree+t(tree)
G = graph.adjacency(as.matrix(tree), weighted=T)
if(dim(tree)[1]>20){
  plot(G, vertex.label.dist=1.5, edge.width=1, main=title, edge.arrow.size=.1 , vertex.size=5)
}else{
  plot(G, vertex.label = names, vertex.label.dist=1.5, edge.width=1, main=title, edge.arrow.size=.1 , vertex.size=5)
}
return()
}
