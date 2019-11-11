#' @title This function computes greedily the best split for samples (x, y) for a customized loss function
#' @param x a matrix, representing the features of parent node observations
#' @param y a vector, representing the target of parent node observations
#' @param customized_loss_foo loss function user-specified, which is a function of the y vector in daugther node, subsequent to the split
#' @param nb_points to find the best split, we analyze every feature variable and discretize the search grid from min to max by a specified number of points
#' @param nb_points_y number of points to find optimal y value
find_best_split <- function(x, y, customized_loss_foo, nb_points, nb_points_y){
  threshold_res <- lapply(X = 1:ncol(x), FUN = function(j){
    threshold_lower <- min(x[,j], na.rm = TRUE)
    threshold_upper <- max(x[,j], na.rm = TRUE)
    threshold_vec <- seq(from = threshold_lower, to = threshold_upper, length.out = nb_points)
    search_y <- seq(from = min(y), to = max(y), length.out = nb_points_y)
    res <- sapply(X = threshold_vec, FUN = function(threshold_xj){
      split_lower <- (x[,j] <= threshold_xj)
      split_upper <- (x[,j] > threshold_xj)
      loss_split_lower <- min(sapply(search_y, function(search_yi){
        customized_loss_foo(y = y[split_lower], y_pred = search_yi)
      }))
      loss_split_upper <- min(sapply(search_y, function(search_yi){
        customized_loss_foo(y = y[split_upper], y_pred = search_yi)
      }))
      sum(c(loss_split_lower, loss_split_upper))
    })
    cbind(j, threshold_vec, res)
  })
  threshold_res <- do.call(rbind.data.frame, threshold_res)
  return(threshold_res[which.min(threshold_res$res),])
}


#' @title Build decision tree, relying on method *find_best_split*
#' @param x a matrix, representing the features of observations
#' @param y a vector, representing the target of observations
#' @param max_depth maximum depth of the tree
#' @param customized_loss_foo loss function user-specified, which is a function of the y vector in node, subsequent to the split
#' @param nb_points  to find the best split, we analyze every feature variable and discretize the search grid from min to max by a specified number of points
#' @param nb_points_y number of points to find optimal y value
#' @param min_data_size minimum number of points to attempt split
#' @description unaware of how this method is done in classic R packages such as *rpart*, we built it from scratch
#' without considering other codes. In later versions, we will try to adjust to standard paradigms, if any does exist.
#' Details must be added to this function. For now, it is quite long and heavy.
build_tree <- function(x, y, max_depth, customized_loss_foo, nb_points, nb_points_y, min_data_size){
  
  #################################################
  # compute tree structure for binary decision tree
  
  temp <- NULL
  parent_nodes <- 'root'
  for(depth in 1:max_depth){
    new_parent_nodes <- NULL
    for(parent_node in parent_nodes){
      child_node_lower <- paste0(parent_node, '_lower')
      child_node_upper <- paste0(parent_node, '_upper')
      temp <- rbind(temp, cbind(depth, parent_node, child_node_lower, child_node_upper))
      new_parent_nodes <- c(new_parent_nodes, child_node_lower, child_node_upper)
    }
    parent_nodes <- new_parent_nodes
  }
  temp <- as.data.frame(temp, stringsAsFactors = FALSE)
  temp$depth <- as.numeric(temp$depth)
  node_levels <- unique(c(temp$parent_node, temp$child_node_lower, temp$child_node_upper))
  temp$parent_node <- factor(x = temp$parent_node, levels = node_levels)
  temp$child_node_lower <- factor(x = temp$child_node_lower, levels = node_levels)
  temp$child_node_upper <- factor(x = temp$child_node_upper, levels = node_levels)
  
  #################################################
  # compute split sequentially

  temp$split_variable <- NA
  temp$split_threshold <- NA
  
  xy_indicator <- array(data = "NA", dim = c(nrow(x), 1 + max_depth))
  xy_indicator[,1] <- "root"
  
  last_leaf <- array(data = max_depth, dim = nrow(x))
  
  for(depth in 1:max_depth){
    u <- unique(xy_indicator[,depth])
    u <- u[u != "NA"]
    if(length(u) == 0) next 
    for(i in 1:length(u)){
      # if(grepl(pattern = '_stop', x = u[i])) next
      xy_cond <- (xy_indicator[,depth] == u[i])
      stop_cutting <- FALSE
      if(sum(xy_cond) > min_data_size){
        best_split <- find_best_split(x[xy_cond,], y[xy_cond], customized_loss_foo = customized_loss_foo, nb_points = nb_points, nb_points_y = nb_points_y)
        if(nrow(best_split) == 0) stop_cutting <- TRUE
      }else{
        stop_cutting <- TRUE
      }
      if(stop_cutting == TRUE){
        last_leaf[xy_cond] <- depth - 1
        temp_row_index <- which(temp$parent_node == u[i])
        temp$split_variable[temp_row_index] <- NA
        temp$split_threshold[temp_row_index] <- NA
        temp$child_node_lower[temp_row_index] <- NA
        temp$child_node_upper[temp_row_index] <- NA
      }else{
        j <- best_split$j
        thresh <- best_split$threshold_vec
        xy_cond_lower <- xy_cond & (x[,j] <= thresh)
        xy_cond_upper <- xy_cond & (x[,j] > thresh)
        xy_indicator[xy_cond_lower, depth+1] <- paste0(u[i], '_lower')
        xy_indicator[xy_cond_upper, depth+1] <- paste0(u[i], '_upper')
        temp_row_index <- which(temp$parent_node == u[i])
        temp$split_variable[temp_row_index] <- j
        temp$split_threshold[temp_row_index] <- thresh
      }
    }
  }
  
  #
  input_data_leaf_vec <- apply(xy_indicator, 1, function(x){ rev(x[x != "NA"])[1] })
  temp <- temp[!is.na(temp$split_variable),]
  
  #
  l <- list('tree_structure' = temp, 
            'last_leaf_index' = last_leaf, 
            'input_data' = list('x' = x, 'y' = y), 
            'input_data_leaf' = xy_indicator,
            'input_data_leaf_vec' = input_data_leaf_vec)
  return(l)
  
}


#' @title Build random forest, classic bagging on top of the custom decision tree
#' @param x a matrix, representing the features of observations
#' @param y a vector, representing the target of observations
#' @param max_depth maximum depth of the tree
#' @param customized_loss_foo loss function user-specified, which is a function of the y vector in node, subsequent to the split
#' @param nb_points  to find the best split, we analyze every feature variable and discretize the search grid from min to max by a specified number of points
#' @param min_data_size minimum number of points to attempt split
#' @param bootstrap_prop proportion of complete data sampled to build decision tree
#' @description For each tree, we sample some proportion of the dataset, build a decision tree and parse results in a list.
build_rf <- function(x, y, n_trees, max_depth, customized_loss_foo, nb_points, nb_points_y, min_data_size, bootstrap_prop){
  lapply(X = 1:n_trees, FUN = function(tree_index){
    row_index_sample <- sample(x = 1:nrow(x), size = ceiling(nrow(x) * bootstrap_prop))
    x_train_sample <- x[row_index_sample,]    
    y_train_sample <- y[row_index_sample]
    build_tree(x = x_train_sample, y = y_train_sample, max_depth = max_depth, 
               customized_loss_foo = customized_loss_foo, nb_points = nb_points, 
               nb_points_y = nb_points_y, min_data_size = min_data_size)
  })
}


#' @title "Descend" the tree hierarchy to give prediction of x_vector
#' @param tree custom Tree built 
#' @param x_vector vector of features
#' @param nb_points_y number of pointsd for the search grid of y
predict_from_tree <- function(tree, x_vector, nb_points_y, customized_loss_foo){
  node <- 'root'
  keep <- TRUE
  while(keep){
    j <- tree$tree_structure$split_variable[tree$tree_structure$parent_node == node]
    thres <- tree$tree_structure$split_threshold[tree$tree_structure$parent_node == node]
    if(x_vector[j] <= thres){
      node <- paste0(node, '_lower')
    }else{
      node <- paste0(node, '_upper')
    }
    keep <- (sum(tree$tree_structure$parent_node == node) == 1)
  }
  cond <- (tree$input_data_leaf_vec == node)
  search_y <- seq(from = min(tree$input_data$y[cond]), to = max(tree$input_data$y[cond]), length.out = nb_points_y)
  losses <- sapply(search_y, function(search_yi){
      customized_loss_foo(y = tree$input_data$y[cond], y_pred = search_yi)
  })
  yopt <- search_y[which.min(losses)]
  return( yopt )
}


#' @title Predict from custom random forest (returns individual tree predictions, no aggregations yet)
#' @param rf custom Random Forest built 
#' @param x_vector vector of features
#' @param nb_points_y number of pointsd for the search grid of y
predict_from_rf <- function(rf, x_vector, nb_points_y, customized_loss_foo){
  sapply(X = 1:length(rf), FUN = function(i){
    new_tree <- rf[[i]]
    predict_from_tree(tree = new_tree, x_vector = x_vector, customized_loss_foo = customized_loss_foo, nb_points_y = nb_points_y)
  })
}

