#' @title Sum of squared errors
#' @param y Vector of true values
#' @param y_pred Scalar, prediction
sum_squared_errors <- function(y, y_pred){
  sum( (y - y_pred)^2 )
}

#' @title Sum of absolute errors
#' @param y Vector of true values
#' @param y_pred Scalar, prediction
sum_absolute_errors <- function(y, y_pred){
  sum( abs(y - y_pred) )
}

#' @title Sum of custom errors
#' @param y Vector of true values
#' @param y_pred Scalar, prediction
#' @description custom loss, asymetric, just to show how one can define their own loss function
sum_custom_errors <- function(y, y_pred){
  pen_positive_error <- function(e){ abs(e) } # under estimated true value
  pen_negative_error <- function(e){ e^2 } # over estimated true value
  error <- (y - y_pred)
  pen <- numeric(length(y))
  pen[error >= 0] <- pen_positive_error(e = error[error >= 0])
  pen[error < 0] <- pen_negative_error(e = error[error < 0])
  sum(pen)
}

#
plot_error_function <- function(error_foo, from, to = from){
  error_vector <- seq(from = from, to = to, length.out = 100)
  pen_vector <- sapply(error_vector, function(y) error_foo(y = y, y_pred = 0))
  plot(x = e_vector, y = pen_vector, type = 'o', pch = 20)
}
