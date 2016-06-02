hausdorff <-
function(X,Y){
  #compute the Hausdorff distance using Euclidean distances between 
  #points whose coordinates are the columns of X and Y
  
  KNN1 = knnsearch(t(X),t(Y),1)
  ind1 = KNN1[[1]]
  D1 = KNN1[[2]]
  
  KNN2 = knnsearch(t(Y),t(X),1)
  ind2 = KNN2[[1]]
  D2 = KNN2[[2]]
  
  m1 = max(D1)
  argm1 = which(D1==m1)
  
  m2 = max(D2)
  argm2 = which(D2==m2)
  
  if(m1>m2){
    d = m1
    iargX = ind1[argm1]
    iargY = argm1
  }else{
    d = m2
    iargX = argm2
    iargY = ind2[argm2]
  }
  
  H = list(d, iargX, iargY)
  return(H)
}
