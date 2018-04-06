#================================================================
# Autor: Dennis Triepke
# Datum: 22.11.2016
# purpose: naive Bayes for classification
#===============================================================


# Init ======================================================
source("01_Support_Functions.R")


# Input data ================================================

filename <- "car.data"
data <- read.csv2(paste0("ImportData/", filename), header = FALSE, sep = ",", 
                  colClasses = rep("character", 7) )
colnames(data) <- c("buying", "maint", "doors", "persons", "low_boot", "safety", "class_values")



#----------------------------------------------------------------------
# 1) estimate for each instances in the test data the class and compute 
# the error rate of the classiﬁe
#----------------------------------------------------------------------

# split data in training and test data
set.seed(1807)
train <- sample(1:nrow(data), nrow(data) * 2/3 , replace = FALSE)
test <- -train

data_test <- data[test,]
data_train <- data[train,]


# select each row of the test data frame and estimate for each row based on the training data 
# the class with the naiveBayes function

data_test$estimated.class_values <- 0 # add new column for store the estimated class

for (j in row.names(data_test)){
  # data pre-processing: 
  
  # select the attribute columns (not the class columns)
  instances_j <- data_test[j, !(colnames(data_test) %in% c("class_values","estimated.class_values")) ]
  instances_j <- as.character(instances_j) # test data needs to be a vector 
  
  # run the naive Bayes function in order to estimate the class value and store the result in a 
  # new column in the test data frame
  data_test[j, "estimated.class_values"] <-  naiveBayes( training_data = data_train , new_data = instances_j) 
}

# error rate
mean(data_test$class_values != data_test$estimated.class_values)
# 0.1388889

# confusion matrix
(confusion_matrix <- xtabs(~ class_values + estimated.class_values, data_test) )



#------------------------------------------------------------------------------------
# 2) Determine the mean error rate over 100 diﬀerent random samples of training data. 
#------------------------------------------------------------------------------------

# To compute 100 error rate simplie run the code in 1) 100 times without the set.seed() argument
# in the loop but run both (loop and set.seed) if you wish to have the same result for the 
# mean error rate.


# First an empty vector for storing the 100 single error rates
error_rates <- rep(0,100)

# before running the loop below: take care it takes a long time
set.seed(1807); for (k in 1:100){
  
  # split data in training and test data
  #set.seed(1807)
  train <- sample(1:nrow(data), nrow(data) * 2/3 , replace = FALSE)
  test <- -train
  
  data_test <- data[test,]
  data_train <- data[train,]
  
  
  # select each row of the test data frame and estimate for each row based on the training data 
  # the class with the naiveBayes function
  
  data_test$estimated.class_values <- 0 # add new column for store the estimated class
  
  for (j in row.names(data_test)){
    # data pre-processing: 
    
    # select the attribute columns (not the class columns)
    instances_j <- data_test[j, !(colnames(data_test) %in% c("class_values","estimated.class_values")) ]
    instances_j <- as.character(instances_j) # test data needs to be a vector 
    
    # run the naive Bayes function in order to estimate the class value and store the result in a 
    # new column in the test data frame
    data_test[j, "estimated.class_values"] <-  naiveBayes( training_data = data_train , new_data = instances_j) 
  }
  
  # error rate 
  error_rates[k] <- sum( ( data_test[,"class_values"] != data_test[, "estimated.class_values"] )*1 ) / nrow(data_test)
  
}

# mean error rate
mean(error_rates)
# 0.1538889


