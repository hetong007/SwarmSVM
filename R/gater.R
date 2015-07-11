#' Gater function for mixture SVMs
#' 
#' @param x the nxp training data matrix. Could be a matrix or a sparse matrix object.
#' @param y a response vector for prediction tasks with one value for each of the n rows of \code{x}. 
#'     For classification, the values correspond to class labels and can be a 1xn matrix, 
#'     a simple vector or a factor. For regression, the values correspond to the values to predict, 
#'     and can be a 1xn matrix or a simple vector.
#' @param S the prediction matrix from experts
#' @param hidden the number of neurons in the hidden layer 
#' @param learningrate the learningrate for the back propagation
#' @param threshold neural network stops training once all gradient is below the threshold
#' @param verbose a logical value indicating whether to print information of training.
#' @param ... other parameters passing to \code{neuralnet}
#' 
#' @export
#' 
gater = function(x, y, S, hidden, learningrate = 0.01, threshold = 0.01,
                 verbose = verbose, ...) {
  m = ncol(S)
  n = nrow(x)
  p = ncol(x)
  out.num = 1
  
  x = as.data.frame(x)
  colnames(x) = paste0('X',1:ncol(x))
  S = as.data.frame(S)
  colnames(S) = paste0('S',1:ncol(S))
  data = data.frame(S, x)
  formula = paste0(paste(colnames(S),collapse='+'),
                   '~',
                   paste(colnames(x),collapse='+'))
  net = neuralnet(formula, data, hidden = hidden, true.response = y,
                  algorithm = 'backprop', act.fct = 'tanh',
                  learningrate = learningrate, threshold = threshold,
                  linear.output = FALSE, verbose = verbose, ...)
  # weights = net$weights[[1]]
  res = list(net = net)
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
  res = compute(object$net, newdata)$net.result
  return(res)
}
