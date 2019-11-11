source('R/recalibrating_rf_from_rf_pkg.R')
source('R/custom_random_forest.R')
source('R/loss_function_examples.R')

### LOAD DATASET ---------------------------------------------------------------------------------

# read data (we shoud have a target and some features)
temp <- readRDS(file = "./data/some_simulated_data.RDS")
target <- temp$target
features <- temp$features
rm(temp)

# viz features
str(features)
head(features)

# build proper dataframe to work with
mat <- cbind.data.frame(target, features)
n <- nrow(mat)
mat$contains_na <- rowSums(is.na(mat) > 0)
mat$is_train <- TRUE
mat$is_train[(floor(n/2)+1):n] <- FALSE
mat_col_index_target <- 1
mat_col_index_features <- 1 + 1:ncol(features)

# erase NAs
mat <- na.omit(mat)

### BUILD CUSTOM LOSS FUNCTION -----------------------------------------------------------------

sum_custom_errors <- function(y, y_pred){
  pen_positive_error <- function(e){ abs(e) } # under estimated true value
  pen_negative_error <- function(e){ e^2 } # over estimated true value
  error <- (y - y_pred)
  pen <- numeric(length(y))
  pen[error >= 0] <- pen_positive_error(e = error[error >= 0])
  pen[error < 0] <- pen_negative_error(e = error[error < 0])
  sum(pen)
}
plot_error_function(error_foo = sum_custom_errors, from = +5, to = -5)

### FIT MODELS ON TRAINING DATA -----------------------------------------------------------------

# random forest hyper-parameters
N_TREES <- 10
MAX_DEPTH <- 8

# fit agnostic random forest
library(randomForest)
agnostic_rf <- randomForest(target ~ ., data = mat[mat$is_train, c(mat_col_index_target, mat_col_index_features)], mtry = 2, ntree = N_TREES)
agnostic_rf

# note : it would be cool to have Out-of-bag estimates of the error function we will get on a test set !!

# question : should we average the predictions selected for every tree final node OR
# should we gather the set of ys in each final node and from there construct a global estimate ?
# what did the authors of the quantile random forest choose to do ???

# redo predictions based on initial partition proposed by classic variance splitting
my_forest <- recalibrate_forest_predictions(rfobj = agnostic_rf, 
                                            x_train = mat[mat$is_train, mat_col_index_features], 
                                            y_train = mat[mat$is_train, mat_col_index_target],
                                            customized_loss_foo = sum_custom_errors)

# fit customized random forest
goal_driven_rf <- build_rf(y = mat[mat$is_train, mat_col_index_target], 
                           x = mat[mat$is_train, mat_col_index_features], 
                           customized_loss_foo = sum_custom_errors, 
                           min_data_size = 10, 
                           nb_points = 200, 
                           nb_points_y = 20,
                           n_trees = N_TREES, 
                           max_depth = MAX_DEPTH, 
                           bootstrap_prop = 2/3)

# get the number of trees
length(goal_driven_rf)

# taking a peak at tree number 2 out of 10
str(goal_driven_rf[[2]], 1)

### PREDICT ON TEST SET -----------------------------------------------------------------

# randomForest baseline, interesting to amke comparisons
agnostic_rf_pred <- predict(agnostic_rf, mat[mat$is_train == FALSE, mat_col_index_features])

# recalibrated randomForest, compute predictions for each tree and aggregate with mean
agnostic_to_goal_rf_pred <- predict_from_new_forest(myforest = my_forest, 
                                                    x_test = mat[mat$is_train == F, mat_col_index_features]) 
agnostic_to_goal_rf_pred_mean <- rowMeans(agnostic_to_goal_rf_pred)

# goal driven splits, but classic prediction metric
goal_driven_rf_classic_pred <- t(apply(X = mat[mat$is_train == FALSE, mat_col_index_features], MARGIN = 1, FUN = function(x){
  predict_from_rf(rf = goal_driven_rf, x_vector = x, nb_points_y = 50, customized_loss_foo = sum_squared_errors)
}))
goal_driven_rf_classic_pred_mean <- rowMeans(goal_driven_rf_classic_pred)

# goal driven splits and goal driven prediction
goal_driven_rf_pred <- t(apply(X = mat[mat$is_train == FALSE, mat_col_index_features], MARGIN = 1, FUN = function(x){
  predict_from_rf(rf = goal_driven_rf, x_vector = x, nb_points_y = 50, customized_loss_foo = sum_custom_errors)
}))
goal_driven_rf_pred_mean <- rowMeans(goal_driven_rf_pred)

## note that we can also get standard deviations over predictions !
## apply(goal_driven_rf_pred, 1, sd)

# combine all the predictions in a big dataframe
ypred_test <- cbind.data.frame(
  "oracle" = mat[mat$is_train == F, mat_col_index_target],
  "agnostic_rf" = agnostic_rf_pred,
  "agnostic_to_goal_rf" = agnostic_to_goal_rf_pred_mean,
  "goal_driven_rf_classic" = goal_driven_rf_classic_pred_mean,
  "goal_driven_rf" = goal_driven_rf_pred_mean
)

### ANALYZE PREDICTIONS -----------------------------------------------------------------

# correlation matrix between predictions -> it may be perfectly correlated, but there might shifts in constants !!
cor(ypred_test)

# plot test set (pred vs reality)
color_vector <- rainbow(n = ncol(ypred_test))
plot(x = ypred_test$oracle, y = ypred_test$oracle)
for(pred_index in 2:ncol(ypred_test)){
  points(x = ypred_test$oracle, y = ypred_test[,pred_index], col = color_vector[pred_index])
}
legend("bottom", legend = colnames(ypred_test), col = color_vector, lty = 1, lwd = 2)

# nicer plot
library(ggplot2)
temp <- data.table::melt(ypred_test, id.vars = "oracle")
ggplot(data = temp, mapping = aes(x = oracle, y = value, col = variable)) + 
  geom_point(alpha = 0.3)  +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = 'loess')

# compute losses
rmse_loss <- numeric(ncol(ypred_test))
names(rmse_loss) <- colnames(ypred_test)
goal_loss <- rmse_loss
for(pred_index in 1:ncol(ypred_test)){
  goal_loss[pred_index] <- sum_custom_errors(y = ypred_test$oracle, y_pred = ypred_test[,pred_index])
  rmse_loss[pred_index] <- sum_squared_errors(y = ypred_test$oracle, y_pred = ypred_test[,pred_index])
}

# quick look
sort(goal_loss)
sort(rmse_loss)

# normalized plots
normalize <- function(x){ (x - min(x)) / (max(x) - min(x)) }
scores <- cbind(goal_loss, rmse_loss)
scores <- scores[order(scores[,1]),]
savePlot <- FALSE
if(savePlot) png(file = 'model_performances.png')
par(mfrow = c(2,1))
barplot(normalize(scores[,1]), col = 'blue', main = "Task loss")
barplot(normalize(scores[,2]), col = 'red', main = "RMSE")
par(mfrow = c(1,1))
if(savePlot) dev.off()
