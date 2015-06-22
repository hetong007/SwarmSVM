#' Gater function for mixture SVMs
#' 
#' @param x the nxp training data matrix. Could be a matrix or a sparse matrix object.
#' @param y a response vector for prediction tasks with one value for each of the n rows of \code{x}. 
#'     For classification, the values correspond to class labels and can be a 1xn matrix, 
#'     a simple vector or a factor. For regression, the values correspond to the values to predict, 
#'     and can be a 1xn matrix or a simple vector.
#' @param S the prediction matrix from experts
#' 
#' 
#' @export
#' 
gater = function(x, y, S) {
  m = ncol(S)
  weights = rep(1,m)
  res = list(weights = weights,
             m = m)
  res = structure(res, class = "gater")
  return(res)
}

#' 
#' Predictions for Gater function
#' 
#' The function applies a model produced by the 
#'  \code{gaterSVM} function to every row of a data matrix and returns the model predictions.
#' 
#' @param object Object of class "gater", created by \code{gater}.
#' @param newdata An n x p matrix containing the new input data. Could be a matrix or a sparse matrix object.
#' @param ... parameters for future usage.
#' 
#' @method predict gater
#' 
#' @export
#' 
predict.gater = function(object, newdata, ...) {
  res = matrix(1, nrow(newdata), object$m)
  return(res)
}
