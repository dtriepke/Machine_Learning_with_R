#================================================================
# Autor: Dennis Triepke
# Datum: 22.11.2016
# purpose: naive Bayes for classification
#===============================================================

# Default training data ======================================

# training data
#outlook <- c("sunny", "sunny", "overcast", rep("rainy",3), "overcast", "sunny", "sunny", "rainy", "sunny", "overcast", "overcast", "rainy")
#temperature <- c(rep("hot", 3), "mild", rep("cool",3), "mild", "cool", "mild", "mild", "mild", "hot", "mild")
#humidity <- c(rep("high",4), rep("normal",3), "high", rep("normal",3), "high", "normal", "high")
#windy <- c("FALSE", "TRUE", rep("FALSE",3), "TRUE", "TRUE", rep("FALSE",3), "TRUE", "TRUE", "FALSE", "TRUE")
#play <- c("no", "no", rep("yes", 3), "no", "yes", "no", rep("yes", 5), "no")
#data_train <- data.frame(outlook, temperature, humidity, windy, play)

#new_data <- c("sunny", "hot", "high", "FALSE")


#new_data <- mydata
#training_data =  data_train

# Function that compute the naive Bayes classification =======
naiveBayes <- function(training_data, new_data){
  
  # detact class values for split
  class_values <- unique(training_data[ , ncol(training_data)])
  
  # decicion table for saving results
  posteriori_prob_tbl <- data.frame(class_values, condition = "new_data" , posterior_probabilities = 0)
  
  # compute prior probability for target classes and save in a table
  priors <- table(training_data[, ncol(training_data)]) /  nrow(training_data) 
  
  
  #class_value = "no"
  
  for (class_value in class_values){
    
    # split test data w.r.t. the class values
    splited_data <- split.data.frame(training_data, f = training_data[, ncol(training_data)], drop = FALSE)
    class_table <- splited_data[[class_value]]
    
    # likelihood table (is refreshed in each iteration) 
    likelihood_prob_tbl <- data.frame(new_data, condition = class_value , conditional_probability = 0)
    
    #i = 4
    
    # loop for calculation the conditional probabilities for each attribute given the class_value
    for (i in 1:length(new_data)){
     
      # for each attribute value in new_data compute probability under the condition of the class value 
      
      # to avoit 0 probabilities (zero frequency) during the calculation of the 
      # maximum likelihood: "Laplance k-Smooting" 
      k <- 1 # smooting constant
      d <- length(unique(training_data[,i]) ) # size of the considered attribute domaine  
      count <- as.numeric( tapply(class_table[,ncol(class_table)], class_table[,i], length)[new_data[i]] )
      count <- ifelse(is.na(count), 0, count) # ifelse function to avoit NA's
      
      # store each in a intermediate table 
      likelihood_prob_tbl[i, 3] <-  (count + k) / ( nrow(class_table) + k * d )
    }
 

    # help function for columns multiplication:
    colMult <- function(x){
      j <- 0
      p <- 1
      while(j < length(x)){
        j <- j + 1
        p <- p * x[j] }
      return(p)
    }
    
    
    # compute the likelihood probability for the test data (new_data)   
    likelihood_prob <- colMult(likelihood_prob_tbl$conditional_probability)
  
    # compute the posteriori probability by rule maximum likelihood x prior
    posteriori_prob_tbl[posteriori_prob_tbl[,1] ==  class_value, 3] <- priors[class_value] * likelihood_prob
    
  }
  
  # decide by the class with the highest posterior probability
  decicion <- as.character( posteriori_prob_tbl[ which.max(posteriori_prob_tbl$posterior_probabilities) , 1] )
  p <- as.numeric(posteriori_prob_tbl[which.max(posteriori_prob_tbl$posterior_probabilities),3]   )
  
  # function output
  #return(paste0("<class ",decicion, " posteriori probability ",p, ">") )
  #return(posteriori_prob_tbl)
  
  return(decicion)
}