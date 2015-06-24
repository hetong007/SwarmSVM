#' Divide-and-Conquer kernel SVM (DC-SVM)
#' 
#' Implementation of Divide-and-Conquer kernel SVM (DC-SVM) by Cho-Jui Hsieh, Si Si, and Inderjit S. Dhillon 
#' 
#' @param x the nxp training data matrix. Could be a matrix or a sparse matrix object.
#' @param y a response vector for prediction tasks with one value for each of the n rows of \code{x}. 
#'     For classification, the values correspond to class labels and can be a 1xn matrix, 
#'     a simple vector or a factor. For regression, the values correspond to the values to predict, 
#'     and can be a 1xn matrix or a simple vector.
#' @param k the number of sub-problems divided
#' @param m the number of sample for kernel kmeans
#' @param kernel the kernel type: 0 for linear, 1 for polynomial, 2 for gaussian
#' @param max.levels the maximum number of level
#' @param early whether use early prediction
#' @param seed the random seed. Set it to \code{NULL} to randomize the model.
#' @param ... other parameters passed to \code{e1071::svm}
#' 
#' @examples
#' data(svmguide1)
#' svmguide1.t = as.matrix(svmguide1[[2]])
#' svmguide1 = as.matrix(svmguide1[[1]])
#' dcsvm.model = dcSVM(x = svmguide1[,-1], y = svmguide1[,1], 
#'                     k = 2, max.levels = 2,
#'                     kernel = 2,early = FALSE, m = 100)
#' preds = predict(dcsvm.model, svmguide1.t[,-1])
#' 
#' @export
#' 
dcSVM = function(x, y, k = 4, m, kernel = 3, max.levels, early = 0, 
                 seed = NULL, ...) {
  if (!is.null(seed))
    set.seed(seed)
  n = nrow(x)
  x = as.matrix(x)
  y = as.factor(y)
  support = rep(0,n)
  num.lvls = length(levels(y))
  alpha = matrix(0,n,num.lvls-1)
  
  checkmate::assertInt(kernel, lower = 1, upper = 3)
  # kernlab.kernel = c('vanilladot','polydot','rbfdot')[kernel]
  svm.kernel = c('linear','polynomial','radial')[kernel]
  
#   kpar = list(...)
#   if (kernel==3) {
#     if (!is.null(kpar$gamma))
#       kpar$sigma = 1/kpar$gamma
#   } else if (kernel==2) {
#     if (!is.null(kpar$coef0))
#       kpar$offset = kpar$coef0
#     if (!is.null(kpar$gamma))
#       kpar$scale = 1/kpar$gamma
#   }
  
  for (lvl in max.levels:1) {
    kl = k^lvl
    if (lvl == max.levels) {
      ind = sample(1:n,min(n,m))
    } else {
      ind = sample(support,min(length(support),m))
    }
    
#     if (length(kpar)>0) {
#       kkmeans.res = kernlab::kkmeans(as.matrix(x[ind,]), centers = kl, kpar = kpar,
#                                      kernel = kernlab.kernel)
#     } else {
#       kkmeans.res = kernlab::kkmeans(as.matrix(x[ind,]), centers = kl,
#                                      kernel = kernlab.kernel)
#     }
    kmeans.res = cluster.fun.mlpack(as.matrix(x[ind,]),centers = kl)
    
    # cluster.label = kern.predict(kkmeans.res, x)
    cluster.label = kmeans.res$cluster
    
    # Train svm for each cluster
    new.alpha = matrix(0,n,num.lvls-1)
    new.support = rep(FALSE,n)
    svm.models = list()
    for (clst in 1:kl) {
      ind = which(cluster.label == clst)
      if (length(ind)>1) {
        # train the svm with given support vectors
        if (length(unique(y[ind]))>1) {
          if (lvl == max.levels) {
            svm.model = svm(x = x[ind,], y = y[ind], kernel = svm.kernel, ...)
          } else {
            svm.model = svm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                            alpha = alpha[ind,], ...) 
          }
        } else {
          class_rank = which(unique(y[ind])==levels(y))
          svm.model = oneclass.svm(x[ind,], num.lvls, class_rank)
        }
        svm.models[[clst]] = svm.model
        sv.ind = ind[svm.model$index]
        new.support[sv.ind] = TRUE
        new.alpha[sv.ind,] = svm.model$coefs
      }
    }
    support = which(new.support)
    alpha = new.alpha
    if (early<=lvl)
      break
  }
  if (early == 0){
    # Refine
    ind = support
    svm.models = svm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                     alpha = alpha[ind,], ...)
    support = ind[svm.model$index]
    alpha[support,] = svm.model$coefs
    
    # Final
    svm.models = svm(x = x, y = y, kernel = svm.kernel, alpha = alpha, ...)
  }
  # Result structure
  result = list(svm = svm.models,
                kmeans.res = kmeans.res,
                early = early,
                cluster.fun = cluster.fun.mlpack)
  result = structure(result, class = "dcSVM")
  return(result)
}

#' 
#' Predictions with Divide-Conquer Support Vector Machines
#' 
#' The function applies a model produced by the 
#'  \code{dcSVM} function to every row of a data matrix and returns the model predictions.
#' 
#' @param object Object of class "dcSVM", created by \code{dcSVM}.
#' @param newdata An n x p matrix containing the new input data. Could be a matrix or a sparse matrix object.
#' @param ... other parameters passing to \code{predict.svm}
#' 
#' @method predict dcSVM
#' 
#' @export
#' 
predict.dcSVM = function(object, newdata, ...) {
  
  checkmate::assertClass(object, 'dcSVM')
  
  if (missing(newdata))
    return(fitted(object$svm))
  
  newdata = as.matrix(newdata)
  
  # Assign label
  if (object$early > 0) {
    new.result = object$cluster.fun(object$kmeans.res,newdata)
    new.result = new.result$cluster
    k = max(new.result)
    preds = rep(0, nrow(newdata))
    for (i in 1:k) {
      ind = which(new.result == i)
      preds[ind] = predict(object$svm[[i]], newdata[ind,], ...)
    }
  } else {
    preds = predict(object$svm, newdata, ...)
  }
  return(preds)
}
