#=================================================================
# Autor: Dennis Triepke
# Datum: 28.11.2016
# purpose: Launcher to run the k-mean Algorithm 
#=================================================================



# Local Initialisation ===================================
pathImp <- "ImportData/"


# load functions =========================================
source("01_Support_Functions.R")



# Analysis of car data with k mean =======================

# 1. Import car data-----------------------------------------
filename <- "car.data"
data <- read.csv(paste0(pathImp,filename), header = FALSE, sep = ",")
colnames(data) <- c("buying", "maint", "doors", "persons", "low_boot", "safety", "class_values")
# The car data has categorical variables. However, k-means only works for continious variables,
# Therefore we have to change the factors to numbers.
data= within(data,{
  buying=ordered(buying,levels=c("low","med","high","vhigh"),labels=c(1,2,3,4))
  maint=ordered(maint,levels=c("low","med","high","vhigh"),labels=c(1,2,3,4))
  doors=ordered(doors,levels=c("2","3","4","5more"),labels=c(2,3,4,5))
  persons=ordered(persons,levels=c("2","4","more"),labels=c(2,4,6))
  low_boot=ordered(low_boot,levels=c("small","med","big"),labels=c(1,2,3))
  safety=ordered(safety,levels=c("low","med","high"),labels=c(1,2,3))
})
data[1:6] <- lapply(data[1:6], function(x) as.numeric(as.character(x)))


# 2. Apply the k_mean algorithm to detect the mean---------
# The last column of the car data "class_values" has been excluded during the cluster building, 
# because this is the real class we actually don't now but we need later for the validation how good we classified.
# The output from the function is a vector with the length of the input dataframe which includes the cluster informations.
# Therefore we add the output directly as a column on the car data.
# The Cluster size is set to  4.
data$cluster <- k_means(data[1:6], 4)


# 3. Error rate just for car data-------------------------
# The error rate can be calculated when the real cluster value (class value) is known. If the the values are known, 
# it is no longer a unsupervised learning like k mean. Therefore is the error rate claculation seperatly 
# done rather than in the k_mean function included.

# pre-proseccings: empty variables for clac purpose
cluster_elements = vector(mode="list", length = 4)
cluster_decision = vector( length = 4)
error_rate = 0

for (j in 1:4) {
  # Assume that all objects of a cluster will be classiﬁed with the same class. 
  # This class is the majority class of the training examples of that cluster.
  # An error is difinded where there are elements in an cluster which is not a decision cluster.
  cluster_elements[[j]] = sort(table(data[data$cluster == j, "class_values"]), decreasing = TRUE)
  cluster_decision[[j]] = names(sort(table(data[data$cluster == j, "class_values"]), decreasing = TRUE)[1])
  error_rate = error_rate + sum( cluster_elements[[j]][ names(cluster_elements[[j]]) != cluster_decision[[j]] ] )  
}

error_rate = error_rate / nrow(data)
print(error_rate)






