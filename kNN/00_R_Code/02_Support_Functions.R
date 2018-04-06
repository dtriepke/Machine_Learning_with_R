




# k nearest neighbor function
kNN <- function(train , test_point, class, k){
  
  
  if( k > nrow(train)) { stop("k to large")}
  
  dist_vec <- rep(0, nrow(train) )
  
  #i = 1
  for (i in c(1: nrow(train))){
    
    tmp <- rbind( train[i,], test_point)
    dist_vec[i] <- dist(tmp, method = "euclidean")
  }
  
  
  v <- data.frame(dist_vec, class)
  v_ordered <- v[order(dist_vec) ,]
  
  
  return( v_ordered[c(1:k), "class"] ) 
}
