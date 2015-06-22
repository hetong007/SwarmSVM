#' Mixture SVMs with gater function
#' 
#' 
#' @param x the nxp training data matrix. Could be a matrix or a sparse matrix object.
#' @param y a response vector for prediction tasks with one value for each of the n rows of \code{x}. 
#'     For classification, the values correspond to class labels and can be a 1xn matrix, 
#'     a simple vector or a factor. For regression, the values correspond to the values to predict, 
#'     and can be a 1xn matrix or a simple vector.
#' @param m the number of experts
#' @param c a positive constant controlling the upper bound of 
#'     the number of samples in each subset.
#' @param max.iter the number of iterations
#' 
#' @export
#' 
gaterSVM = function(x, y, m, c = 1, max.iter) {
  n = nrow(x)
  S = matrix(0, n, m)
  
  shuf = sample(n)
  sub.ind = vector(m, mode = 'list')
  for (i in 1:(m-1)) {
    sub.ind[[i]] = shuf[1:m]
    shuf = setdiff(shuf,sub.ind[[i]])
  }
  sub.ind[[m]] = shuf
  
  stopCondition = FALSE
  iter = 1
  
  while (!stopCondition) {
    expert = vector(m, mode = 'list')
    for (i in 1:m) {
      expert[[i]] = e1071::svm(x[sub.ind,], y[sub.ind])
      S[,i] = predict(expert[[i]], x)
    }
    
    # Train weight
    gater.model = gater(x, y, S)
    W = predict(gater.model, x)
    
    # Re-arrange vectors
    sub.assign = rep(0,n)
    sub.num = rep(0, m)
    sub.avail = rep(1,m)
    for (i in 1:n) {
      ind = which.max(sub.avail*W[i,])
      sub.assign[i] = ind
      sub.num[ind] = sub.num[ind]+1
      if (sub.num[ind]>n/m+c)
        sub.avail[ind] = 0
    }
    for (i in 1:m) {
      sub.ind[[i]] = which(sub.assign==i)
    }
    
    iter = iter+1
    stopCondition = iter>max.iter
  }
  result = list(expert = expert,
                gater = gater.model)
  result = structure(result, class = "gaterSVM")
  return(result)
}

#' Prediction for Gater SVM
#' 
#' The function applies a model produced by the 
#'  \code{gaterSVM} function to every row of a data matrix and returns the model predictions.
#' 
#' @param object An object of class \code{gaterSVM}
#' @param newdata newdata An n x p matrix containing the new input data. 
#'     Could be a matrix or a sparse matrix object.
#' @param ... parameters for future usage.
#' 
#' @method predict gaterSVM
#' 
#' @export
#' 
predict.gaterSVM = function(object, newdata, ...) {
  n = nrow(newdata)
  m = length(object$expert)
  S = matrix(0, n, m)
  for (i in 1:m) {
    S[,i] = predict(object$expert[[i]], newdata)
  }
  W = predict(object$gater, newdata)
  pred = sign(tanh(colSums(W*S)))
  return(pred)
}
