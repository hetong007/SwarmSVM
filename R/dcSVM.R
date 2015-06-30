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
#'                     k = 4, max.levels = 4, seed = 0,
#'                     kernel = 3,early = 0, m = 500)
#' preds = predict(dcsvm.model, svmguide1.t[,-1])
#' 
#' @export
#' 
dcSVM = function(x, y, k = 4, m, kernel = 3, max.levels, early = 0, 
                 seed = NULL, ...) {
  if (!is.null(seed))
    set.seed(seed)
  n = nrow(x)
  if (m>n) {
    warning("m larger than number of data points. It is adjusted to n.")
    m = n
  }
  x = as.matrix(x)
  y = as.factor(y)
  support = rep(0,n)
  num.lvls = length(levels(y))
  alpha = matrix(0,n,num.lvls-1)
  cluster.fun = cluster.fun.mlpack
  
  checkmate::assertInt(kernel, lower = 1, upper = 3)
  # kernlab.kernel = c('vanilladot','polydot','rbfdot')[kernel]
  svm.kernel = c('linear','polynomial','radial')[kernel]
  
  # clustering tree process
  sampleX = as.matrix(x[sample(1:n,m),])
  sample.cluster.ind = matrix(0, nrow(sampleX), max.levels+1)
  sample.cluster.ind[,1] = 1
  cluster.ind = matrix(0, n, max.levels+1)
  cluster.ind[,1] = 1
  min.cluster = ceiling(m/(k^max.levels))
  #min.cluster = 100
  for (i in 1:max.levels) {
    num.clust = max(sample.cluster.ind[,i])
    center.list = list()
    sample.nowcid = 0
    nowcid = 0
    for (cid in 1:num.clust) {
      # train on samples
      ind = which(sample.cluster.ind[,i]==cid)
      kmeans.res = cluster.fun(sampleX[ind,],centers = k)
      res = kmeans.res$cluster
      res = as.numeric(as.factor(res))
      if (min(table(res))<min.cluster) {
        sample.cluster.ind[ind,i+1] = sample.nowcid+1
        sample.nowcid = sample.nowcid+1
        
        # predict on the entire data set
        ind = which(cluster.ind[,i]==cid)
        cluster.ind[ind,i+1] = 1+nowcid
        nowcid = nowcid + 1
      } else {
        sample.cluster.ind[ind,i+1] = res+sample.nowcid
        current.centers = kmeans.res$centers
        sample.nowcid = sample.nowcid+max(res)
        
        # predict on the entire data set
        ind = which(cluster.ind[,i]==cid)
        res = cluster.fun(as.matrix(x[ind,]),centers = current.centers)$cluster
        res = as.numeric(as.factor(res))
        cluster.ind[ind,i+1] = res+nowcid
        nowcid = nowcid + max(res)
      }
      # cat(i,cid,'\n')
    }
    sample.cluster.ind[,i+1] = as.numeric(as.factor(sample.cluster.ind[,i+1]))
    cluster.ind[,i+1] = as.numeric(as.factor(cluster.ind[,i+1]))
    if (length(unique(cluster.ind[,i+1]))==length(unique(cluster.ind[,i]))) {
      cluster.ind = cluster.ind[,1:i]
      max.levels = i-1
      warning("max.levels reduced.")
      break
    }
  }
  if (max.levels<1)
    stop('no cluster applied.')
  for (lvl in max.levels:1) {
    # cluster.label = kern.predict(kkmeans.res, x)
    # cluster.label = cluster.fun(x, kmeans.res$centers)$cluster
    cluster.label = cluster.ind[,lvl+1]
    
    # Train svm for each cluster
    new.alpha = matrix(0,n,num.lvls-1)
    new.support = rep(FALSE,n)
    svm.models = list()
    kl = max(cluster.label)
    for (clst in 1:kl) {
      ind = which(cluster.label == clst)
      if (length(ind)>1) {
        # train the svm with given support vectors
        #if (length(unique(y[ind]))>1) {
          if (lvl == max.levels) {
            BBmisc::suppressAll({
              svm.model = alphasvm(x = x[ind,], y = y[ind], kernel = svm.kernel, ...)
            })
          } else {
            BBmisc::suppressAll({
              svm.model = alphasvm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                            alpha = alpha[ind,], ...) 
            })
          }
        #} else {
        #  cat('one!\n')
        #  class_rank = which(unique(y[ind])==levels(y))
        #  svm.model = oneclass.svm(x[ind,], num.lvls, class_rank)
        #}
        svm.models[[clst]] = svm.model
        sv.ind = ind[svm.model$index]
        if (length(sv.ind)>0) {
          new.support[sv.ind] = TRUE
          new.alpha[sv.ind,] = svm.model$coefs
        }
      }
    }
    support = which(new.support)
    alpha = new.alpha
    if (early>0 && early<=lvl)
      break
  }
  if (early == 0){
    # Refine
    ind = support
    BBmisc::suppressAll({
      svm.models = alphasvm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                       alpha = alpha[ind,], ...)
    })
    support = ind[svm.models$index]
    alpha = matrix(0,n,num.lvls-1)
    alpha[support,] = svm.models$coefs
    
    # Final
    BBmisc::suppressAll({
      svm.models = alphasvm(x = x, y = y, kernel = svm.kernel, alpha = alpha, ...)
    })
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
    new.result = object$cluster.fun(newdata,object$kmeans.res)
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
