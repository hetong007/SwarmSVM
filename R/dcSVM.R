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
#' @param cluster.method The clusterign algorithm to use. Possible choices are 
#' \itemize{
#'     \item "kmeans" Algorithm from \code{stats::kmeans}
#'     \item "mlKmeans" Algorithm from \code{RcppMLPACK::mlKmeans}
#'     \item "kernkmeans" Algorithm from \code{kernlab::kkmeans}
#' }
#' If \code{cluster.fun} and \code{cluster.predict} are provided, \code{cluster.method} doesn't work anymore.
#' @param cluster.fun The function to train cluster labels for the data based on given number of centers. 
#'     Customized function is acceptable, as long as the resulting list contains two fields named as \code{cluster} and \code{centers}.
#' @param cluster.predict The function to predict cluster labels for the data based on trained object. 
#'     Customized function is acceptable, as long as the resulting list contains two fields named as \code{cluster} and \code{centers}.
#' @param ... other parameters passed to \code{e1071::svm}
#' 
#' @examples
#' data(svmguide1)
#' svmguide1.t = as.matrix(svmguide1[[2]])
#' svmguide1 = as.matrix(svmguide1[[1]])
#' dcsvm.model = dcSVM(x = svmguide1[,-1], y = svmguide1[,1],
#'                     k = 4, max.levels = 4, seed = 0, cost = 32, gamma = 2,
#'                     kernel = 3,early = 0, m = 800)
#' preds = predict(dcsvm.model, svmguide1.t[,-1])
#' table(preds, svmguide1.t[,1])
#' sum(diag(table(preds,svmguide1.t[,1])))/nrow(svmguide1.t)
#' 
#' @export
#' 
dcSVM = function(x, y, k = 4, m, kernel = 3, max.levels, early = 0, 
                 seed = NULL, cluster.method = 'kmeans', 
                 cluster.fun = NULL, cluster.predict = NULL, ...) {
  
  # parameter check
  if (!testNull(seed))
    set.seed(seed)
  n = nrow(x)
  if (m>n) {
    warning("m larger than n, the number of data points. It is adjusted to n.")
    m = n
  }
  assertMatrix(x, min.rows = 1, min.cols = 1)
  assertVector(y, len = nrow(x))
  
  if (testNull(cluster.fun) && testNull(cluster.predict)) {
    assertCharacter(cluster.method)
    if (cluster.method == 'kmeans') {
      cluster.fun = stats::kmeans
      cluster.predict = kmeans.predict
    } else if (cluster.method == 'mlKmeans') {
      cluster.fun = cluster.fun.mlpack
      cluster.predict = cluster.predict.mlpack
    } else if (cluster.method == 'kernkmeans') {
      cluster.fun = cluster.fun.kkmeans
      cluster.predict = cluster.predict.kkmeans
    } else {
      stop("Unknow cluster.method.")
    }
  }
  assertFunction(cluster.fun)
  assertFunction(cluster.predict)
  
  assertInt(kernel, lower = 1, upper = 3)
  svm.kernel = c('linear','polynomial','radial')[kernel]
  
  assertInt(k, lower = 1)
  assertInt(max.levels, lower = 1)
  assertInt(early, lower = 0, upper = max.levels)
  
  support = rep(FALSE, n)
  num.lvls = length(unique(y))
  assertInt(num.lvls, lower = 2, upper = 16)
  alpha = matrix(0, n, num.lvls-1)
  
  # clustering tree process
  sampleX.ind = sample(1:n,m)
  sampleX = as.matrix(x[sampleX.ind,])
  sample.cluster.ind = matrix(0, nrow(sampleX), max.levels+1)
  sample.cluster.ind[,1] = 1
  cluster.ind = matrix(0, n, max.levels+1)
  cluster.ind[,1] = 1
  # In MATLAB here is ceiling(m/(k^max.levels*5))
  min.cluster = ceiling(5*m/(k^max.levels))
  if (min.cluster>m)
    stop("Too few data points or too many clusters assigned.")
  assertInt(min.cluster, lower = 1)
  #min.cluster = 100
  
  cluster.tree = list()
  
  for (i in 1:max.levels) {
    num.clust = max(sample.cluster.ind[,i])
    center.list = list()
    sample.nowcid = 0
    nowcid = 0
    cluster.object.list = list()
    
    for (cid in 1:num.clust) {
      # train on samples
      ind = which(sample.cluster.ind[,i]==cid)
      if (length(ind)<min.cluster) {
        cluster.object = cluster.fun(sampleX[ind,], centers = 1)
        sample.cluster.ind[ind,i+1] = sample.nowcid+1
        sample.nowcid = sample.nowcid+1
        
        # predict on the entire data set
        ind = which(cluster.ind[,i]==cid)
        cluster.ind[ind,i+1] = 1+nowcid
        nowcid = nowcid + 1
      } else {
        cluster.object = cluster.fun(sampleX[ind,],centers = k)
        res = cluster.object$cluster
        res = as.numeric(as.factor(res))
        
        sample.cluster.ind[ind,i+1] = res+sample.nowcid
        current.centers = cluster.object$centers
        sample.nowcid = sample.nowcid+max(res)
        
        # predict on the entire data set
        ind = which(cluster.ind[,i]==cid)
        res = cluster.predict(as.matrix(x[ind,]),cluster.object)
        res = as.numeric(as.factor(res))
        cluster.ind[ind,i+1] = res+nowcid
        nowcid = nowcid + max(res)
      }
      cluster.object.list[[cid]] = cluster.object
      # cat(i,cid,'\n')
    }
    cluster.tree[[i]] = cluster.object.list
    sample.cluster.ind[,i+1] = as.numeric(as.factor(sample.cluster.ind[,i+1]))
    cluster.ind[,i+1] = as.numeric(as.factor(cluster.ind[,i+1]))
    if (length(unique(cluster.ind[,i+1]))==length(unique(cluster.ind[,i]))) {
      cluster.ind = cluster.ind[,1:i]
      max.levels = i-1
      if (early>max.levels)
        early = max.levels
      warning("max.levels reduced.")
      break
    }
  }
  if (max.levels<1)
    stop('no cluster applied.')
  
  assertInt(early, lower = 0, upper = max.levels)
  
  # SVM train
  for (lvl in max.levels:1) {
    # cluster.label = kern.predict(kkmeans.res, x)
    # cluster.label = cluster.fun(x, kmeans.res$centers)$cluster
    assertLogical(support, len = n)
    assertMatrix(alpha, nrows = n, ncols = num.lvls-1)
    cluster.label = as.integer(cluster.ind[, lvl+1])
    assertInteger(cluster.label, len = n, lower = 1)
    cat('Begin level', lvl, '\n')
    
    # Train svm for each cluster
    new.alpha = matrix(0, n, num.lvls-1)
    new.support = rep(FALSE, n)
    svm.models = list()
    kl = max(cluster.label)
    for (clst in 1:kl) {
      ind = which(cluster.label == clst)
      if (length(ind)>1) {
        # train the svm with given support vectors
        if (lvl == max.levels || sum(support[ind])==0) {
          #BBmisc::suppressAll({
            svm.model = alphasvm(x = x[ind,], y = y[ind], kernel = svm.kernel, ...)
          #})
        } else {
          #BBmisc::suppressAll({
            svm.model = alphasvm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                                 alpha = alpha[ind,], ...) 
          #})
        }
        svm.models[[clst]] = svm.model
        sv.ind = ind[svm.model$index]
        if (length(sv.ind)>0) {
          new.support[sv.ind] = TRUE
          new.alpha[sv.ind,] = svm.model$coefs
        }
      }
      cat(clst,'\r')
    }
    support = new.support
    alpha = new.alpha
    if (early>0 && early<=lvl)
      break
  }
  if (early == 0){
    # Refine
    ind = which(support)
    #BBmisc::suppressAll({
      svm.models = alphasvm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                            alpha = alpha[ind,], ...)
    #})
    sv.ind = ind[svm.models$index]
    alpha = matrix(0,n,num.lvls-1)
    alpha[sv.ind,] = svm.models$coefs
    
    # Final
    #BBmisc::suppressAll({
      svm.models = alphasvm(x = x, y = y, kernel = svm.kernel, alpha = alpha, ...)
    #})
  }
  # Result structure
  result = list(svm = svm.models,
                early = early,
                cluster.tree = cluster.tree,
                cluster.fun = cluster.fun,
                cluster.predict = cluster.predict)
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
  
  assertMatrix(newdata, min.rows = 1)
  
  # Assign label
  if (object$early > 0) {
    new.result = rep(1, nrow(newdata))
    for (i in 1:object$early) {
      #i = object$early
      cluster.object.list = object$cluster.tree[[i]]
      new.label = rep(0,nrow(newdata))
      k = 0
      for (cid in 1:length(cluster.object.list)) {
        ind = which(new.result == cid)
        if (length(ind) > 0) {
          new.label[ind] = k + object$cluster.predict(newdata[ind,, drop = FALSE], 
                                                  cluster.object.list[[cid]])
          k = max(new.label[ind])
        }
      }
      new.result = new.label
    }
    # new.result = object$cluster.predict(newdata,object$cluster.object)
    k = max(new.result)
    preds = rep(0, nrow(newdata))
    for (i in 1:k) {
      ind = which(new.result == i)
      if (length(ind)>0)
        preds[ind] = predict.alphasvm(object$svm[[i]], newdata[ind,, drop = FALSE], ...)
    }
  } else {
    preds = predict(object$svm, newdata, ...)
  }
  return(preds)
}
