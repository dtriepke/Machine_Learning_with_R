#==========================================
# Autor: Dennis Triepke
# Datum: 09.11.2016
# purpose: ID3 algorithm for decision tree
#==========================================



## Entropy ================================
print("load entropy function")
# Generate Function for entropy to see how much information is contained in each node
# Use formula for non-binary problems, with more than 2 classes
entropy <- function(x # x - crosstable format or tupel
){
  
  x <- as.numeric(x)
  s <- length(x) # s = sample of training examples 
  entr <- 0
  
  for (i in 1:s) {
    entr <- ifelse(x[i] == 0, entr - 0, entr -( x[i] / sum(x) * log(x[i] / sum(x), base = s )) )
  };
  return(entr)  
}



## Information Gain ========================
print("load information gain function")
# Function for information gain table 
# Note: in order to apply for each iteration sequence
# sets "classes" as tree class
information_gain_table <- function(df, classes = names(df)[ncol(df)]  ){
  
  # pre-processing 
  # set the other values as attributes and create a gain list with default
  attributes.ig <- names(df)[-grep(classes, names(df))]
  gain_list <- data.frame(Attributes = attributes.ig, 
                          Information_Gain = 1)
  
  # fill the list with IG values per attribute
  for (attribute in attributes.ig){
    
    # create a crosstable in order to gather information about the distribution of attributes and classes
    tbl <- xtabs(~ df[,attribute] + df[,classes])
    
    # use these informations to fill the 'gain_list' with Information Gain values for all attributes in df
    gain_list[ grep(attribute, gain_list$Attributes), 2] <- entropy( colSums(tbl)) - sum( rowSums(tbl) / sum(tbl) * apply(tbl, 1, entropy) )
    
  }
  return(gain_list)
}




# ID 3 ======================================

# main function to build a ID3 decision tree and output it in XML format.
# the function recursivly calls itself until the tree has perfectly split the data

print("load function for decision tree with ID3")
# define function for data set 
ID3 <- function(df, 
                classes = names(df)[ncol(df)], # last column gives the class information
                tree = c("begin") # empty vector in order to fill later with inforamtion in XML format
){
  
  # pre-processing 
  # identify possible splitting attributes /  splitting candidates
  attributes <- names(df)[-grep(classes, names(df)) ]
  
  # identify classes ( number)
  tbl_classes <- aggregate(df[classes], by=list(df[,classes][drop = TRUE]), FUN = length  )
  num_classes <- nrow(tbl_classes)
  
  # check if first time calling function. If so, build xml root
  if (tree[[1]] %in% c("begin")){
    
    class_information <- paste(tbl_classes[,1], tbl_classes[,2], sep = ":", collapse = ",")
    
    # than store information about all classes, how often they appear and the entropy in the 'tree' vector
    tree = c('<?xml version="1.0" encoding="UTF-8" standalone="no"?>', 
             paste0("<tree", 
                    ' classes="', class_information,'"', 
                    ' entropy="', round( entropy( table( df[classes])), digits = 3 ) , '">'))
  }
  
  # if just one class remains, return the class as end node (leaf)
  if( num_classes == 1 || colSums(tbl_classes[2]) == 1){ 
    tree = c(tree, as.character( tbl_classes[1,1][drop = TRUE] ) ) }
  
  # if no further splitting is possible (due to no attributes), return the class as end node (leaf)
  if( length(attributes) == 0 && !( tbl_classes[,2] == 1 ) ){ 
    tree = c(tree, as.character( tbl_classes[which.max(tbl_classes[,2]),1][drop =TRUE] ) )
  } # return
  
  # otherwise  split data frame by the best attribute and build subtree(s)
  if( !(num_classes == 1 | length(attributes) == 0) ){
    
    # first determine best attribute based on max information gain criteria
    tbl_IG <- information_gain_table(df, classes = classes)
    best_attribute <-  as.character(tbl_IG$Attributes[which.max(tbl_IG$Information_Gain)][drop = TRUE])
    
    # now split current data frame by the values of the best attribute ...
    subtree <- split.data.frame(df, f = df[,best_attribute], drop = TRUE)
    # ... and omit best attribute column
    for (i in 1:length(names(subtree))){
      x <- subtree[[i]]
      subtree[[i]] <- x[!(names(x) %in% best_attribute)]
    } # [note: subtree is a object over v tables, were v stands for the number of values of the best attribute]
    
    
    # finally analyse each value v from best attribute 
    attribute_values <- names(subtree)
    for (v in attribute_values){
      
      # pre-processing: 
      tbl_classes_subtree <- aggregate(subtree[[v]][classes], by=list(subtree[[v]][,classes]), FUN = length  )
      class_values <- as.character(unique(subtree[[v]][,classes]))
      
      # store information about attribute / node v in vector 'tree' in XML format
      class_information <- paste(tbl_classes_subtree[,1], tbl_classes_subtree[,2], sep = ":", collapse = ",")
      
      tree = c(tree, paste0("<node ",
                            best_attribute, '="',v ,'"',
                            ' classes="', class_information,'"',
                            ' entropy_', v,'="', 
                            round(entropy(table( subtree[[v]][,classes])), digits = 3),'"',
                            ' informationgain_', best_attribute, '="', round(tbl_IG[grep(best_attribute, tbl_IG[,1]),2], digits = 3),
                            '">')
      )
      
      # in order to buildt the sub tree recursively, run the ID3 function for each value v from the best attribute
      tree = c(ID3( df = subtree[[v]], classes = classes, tree=tree ))
      tree = c(tree,"</node>") # close the node
    }
  }
  tree
}

# close the tree, if the ID3 function is done
decision_tree_ID3 <- function(data){
  tree = ID3(data) 
  tree = c(tree,'</tree>') 
}


# export function ===================================
print("load function for export in a *.xml file")
export_tree <- function(x){
  write(x, paste0(pathExp, filename_export,".xml"))
  print(paste0("wrote tree in: ", paste0(pathExp, filename_export, ".xml") ))
}


print("all functions loaded")
