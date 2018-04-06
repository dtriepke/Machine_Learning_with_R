#=================================================================
# Autor: Dennis Triepke
# Datum: 09.11.2016
# purpose: Launcher to run the ID3 Decicion Tree Algorithm
#=================================================================
rm(list = ls(all = TRUE));


## local initialization =================================
source("01_Initialization.R")


## Load Function =======================================
source("01_Support_Functions.R")


## Load data ===========================================
# car data runs without any changes, but to run the code for other data, changes needs to be done here 
# e.g. the filename, seperation creteria etc. ..
filename <- "car.data"
data <- read.csv2(paste0(pathImp, filename), header = FALSE, sep = ",")
# just for the car data, the column names need to be renamed
colnames(data) <- c("buying", "maint", "doors", "persons", "low_boot", "safety", "class_values")


## Build tree ==========================================
# before run the the code, ensure that the class informations are the the LAST column, otherwise change this
# the decision tree will be created with respect to the ID3 algorithm
tree_model <- decision_tree_ID3(data) 
print(tree_model);


## Export tree in XML format ===========================
filename_export <- "tree_car"
export_tree(tree_model)

