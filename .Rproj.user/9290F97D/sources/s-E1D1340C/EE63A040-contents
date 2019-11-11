#' @title Identify in tree build by randomForest package to which node an observation with features *x_vector* belongs to 
#' @param tree tree built by randomForest method
#' @param x_vector feature vector
find_line_index_rfpkg <- function(tree, x_vector){
  x_vector <- as.numeric(x_vector)
  line_index <- 1
  keep <- TRUE
  while(keep){
    split_variable <- tree[line_index, 'split var']
    split_threshold <- tree[line_index, 'split point']
    test <- x_vector[split_variable] <= split_threshold
    if(test){
      line_index <- tree[line_index, 'left daughter']
    }else{
      line_index <- tree[line_index, 'right daughter']
    }
    keep <- (tree[line_index, 'left daughter'] > 0)
  }
  return(line_index)
}


#' @title Compute task-driven prediction for a given tree
#' @param tree tree built by randomForest method
#' @param x_train a matrix, representing the features of observations
#' @param y_train a vector, representing the target of observations
#' @param customized_loss_foo loss function user-specified, which is a function of the y vector in daugther node, subsequent to the split
add_new_prediction_to_tree <- function(tree, x_train, y_train, customized_loss_foo){
  # recover node indexes of training samples
  x_indexes_in_tree <- apply(x_train, 1, function(x) find_line_index_rfpkg(tree = tree, x_vector = x))  
  # building predictions
  unique_indexes <- unique(x_indexes_in_tree)
  search_y <- seq(from = min(y_train), to = max(y_train), length.out = 20)
  tree[,"prediction"] <- NA
  for(index in unique_indexes){
    losses <- sapply(search_y, function(y){
        customized_loss_foo(y = y_train[x_indexes_in_tree == index], y_pred = y)
    })
    value <- search_y[which.min(losses)]
    tree[index,"prediction"] <- value
  }
  # return tree w/ changed column for prediction 
  return(tree)
}


#' @title For each tree in the randomForest object, compute task-driven predictions
#' @param rfobj output of randomForest method
#' @param x_train a matrix, representing the features of observations
#' @param y_train a vector, representing the target of observations
#' @param customized_loss_foo loss function user-specified, which is a function of the y vector in daugther node, subsequent to the split
recalibrate_forest_predictions <- function(rfobj, x_train, y_train, customized_loss_foo){
  tree_list <- list()
  for(k in 1:rfobj$forest$ntree){
    new_tree <- add_new_prediction_to_tree(tree = getTree(rfobj = rfobj, k = k), x_train = x_train, y_train = y_train, customized_loss_foo = customized_loss_foo)
    tree_list[[k]] <- new_tree
  }
  return(tree_list)
}


#' @title Compute predictions for every observations in *x_test* based on customized forest
#' @param myforest output of recalibrate_forest_predictions
#' @param x_test a matrix, representing the features of observations
#' @description potential improvement / simplification : could we just modify the trees in the forest kept and apply the predict function as usual ?
predict_from_new_forest <- function(myforest, x_test){
  x_test_prediction <- array(NA, dim = c(nrow(x_test), length(myforest)))
  for(k in 1:length(myforest)){
    tree <- myforest[[k]]
    x_indexes_in_tree <- apply(x_test, 1, function(x) find_line_index_rfpkg(tree = tree, x_vector = x))  
    x_test_prediction[,k] <- tree[x_indexes_in_tree, "prediction"]
  }
  return(x_test_prediction)
}