#=================================================================
# Autor: Dennis Triepke
# Datum: 28.11.2016
# purpose: support functions for the k-mean algorithm
#=================================================================


# SQUARED EUCLIDEAN DISTANCE FUNCTION =======================
# computes the squared euclidean distance between two points
distance = function (point1, point2){
  sum((point1 - point2)^2)
}



# K-MEAN FUNCTION =======================================
# the function is solely intended for data frames with at least rank scaled attributes 
k_means = function (data, k){
  
  # randomly choose k poins as start centroide
  # generate k random cluster centers from the data
  index = sample(1:nrow(data), k)
  centroids = data[index,]
  
  # set prior_centroids to something different such that the algorithm does not immediatly converge
  prior_centroids = centroids - 1
  
  # run until convergance
  while( any(prior_centroids != centroids) ){
    
    # initialice vector that gonna map each data point () to a cluster
    grouping = vector(length = nrow(data))
    
    
    # for every data point i 
    for (i in 1:nrow(data)){
      
      # initialice a new distance vector (for each data point i as intermediate storage)
      distance_vec = vector(length = nrow(centroids))
      
      # and every centroid j
      for (j in 1:k) {
        
        # compute distance between each datapoint i and the each centroid j
        distance_vec[j] = distance( data[i,], centroids[j,])
      }
      
      
      # find nearest centroid by min
      grouping[i] = which(distance_vec == min(distance_vec))[[1]]
    }
    
    # first save old centroid
    prior_centroids = centroids
    
    # now compute k new centroides 
    # for centroid j and w.r.t. the attributes, calc the mean of all instances i which belongs to cluster j 
    for (j in 1:k) {
      centroids[j,] = colMeans( data[grouping == j,] )
    }
    
  }
  
  # after non cetroides changed the variable 'grouping' is a vector with the length of the data frame 
  # which includes the cluster membership for each in data point/ instances
  return(grouping)
  
}
