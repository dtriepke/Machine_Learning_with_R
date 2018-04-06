#=================================================================
# Autor: Dennis Triepke
# Datum: 28.11.2016
# purpose: Launcher to run the k-NN Algorithm 
#=================================================================

rm( list = ls(all = TRUE))

## Local Initialisation ===================================
pathImp <- "ImportData/"


## load functions =========================================
source("02_Support_Functions.R")


## load data ==============================================
filename <- "car.data"
data <- read.csv(paste0(pathImp,filename), header = FALSE, sep = ",")
colnames(data) <- c("buying", "maint", "doors", "persons", "low_boot", "safety", "class_values")

# pre-processings

# transform data into nummeric varibale and nomalize
  # The car data has categorical variables. However, kNN only works for continious variables,
  # Therefore we have to change the factors to numbers.
  data= within(data,{
    buying=ordered(buying,levels=c("low","med","high","vhigh"),labels=c(1,2,3,4))
    maint=ordered(maint,levels=c("low","med","high","vhigh"),labels=c(1,2,3,4))
    doors=ordered(doors,levels=c("2","3","4","5more"),labels=c(2,3,4,5))
    persons=ordered(persons,levels=c("2","4","more"),labels=c(2,4,6))
    low_boot=ordered(low_boot,levels=c("small","med","big"),labels=c(1,2,3))
    safety=ordered(safety,levels=c("low","med","high"),labels=c(1,2,3))
  })
  
  data[1:6] <- lapply(data[1:6], function(x) as.numeric(as.character(x)) )
  #data[1:6] <- lapply(data[1:6], normalize)

## split data in test and traings data
set.seed(1807)
train <- sample(c(1:nrow(data)), size = 2/3 * nrow(data) )
test <- -train
data_train <- data[train,]
data_test <- data[test,]

#p <- row.names(data_test)[1]
#library(class)
for (p in row.names(data_test)){
 
  data_test[p, "estimated_class"] <- 
    #knn(train = data_train[,1:6], test = data_test[p,1:6], cl = data_train[,7], k = 1 )
    kNN(train = data_train[1:6], test_point = data_test[p, 1:6], class = data_train[,7], k = 1)
}



## check how the kNN is doing
# error rate
mean(data_test[,"class_values"] != data_test[,"estimated_class"]) # 0.2708333
# confusion matrix
xtabs( ~ class_values + estimated_class, data = data_test)




