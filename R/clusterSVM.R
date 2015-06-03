setClass("clusterSVM")

csvmTransform = function(x, lambda, cluster.label, sparse = TRUE) {
  n = nrow(x)
  m = ncol(x)
  k = max(cluster.label)
  if (k == 1) {
    warning('Only one cluster for the data, no transform is performed.')
    if (sparse) {
      return(as(x,'matrix.csr'))
    } else {
      return(as.matrix(x))
    }
  }
  
  if (sparse){
    row.index = NULL
    col.index = NULL
    val = NULL
    for (i in 1:k) {
      row.index = c(row.index, rep(which(cluster.label == i),times = m))
      col.index = c(col.index, rep((1 + (i-1)*m):(i*m),each = sum(cluster.label == i)))
      val = c(val, as.vector(x[which(cluster.label == i),]))
    }
    tilde.x = spMatrix(n, k*m, i = row.index, j = col.index, x = val)
    tilde.x = cBind(x / sqrt(lambda), tilde.x)
    tilde.x = as(tilde.x,'dgCMatrix')
    # LiblineaR only support sparse matrix of the class "matrix.csr"
    tilde.x = as(tilde.x, 'matrix.csr')
  } else {
    x = as(x,'matrix')
    tilde.x = matrix(0,n,k*m)
    for (i in 1:k) {
      row.index = which(cluster.label == i)
      col.index = (1 + (i-1)*m):(i*m)
      tilde.x[row.index, col.index] = x[row.index,]
    }
    tilde.x = cbind(x / sqrt(lambda), tilde.x)
  }
  return(tilde.x)
}

eucliDist= function(x, centers) {
  if (nrow(centers)>1) {
    result = apply(centers, 1, function(C) colSums( (t(x)-C)^2 ))
  } else {
    result = colSums((t(x)-as.vector(centers))^2)
  }
  return(result)
}

#' Clustered Support Vector Machine
#' 
#' Implementation of Gu, Quanquan, and Jiawei Han. "Clustered support vector machines."
#' 
#' @param x the nxp training data matrix. Could be a matrix or a sparse matrix object.
#' @param y a response vector for prediction tasks with one value for each of the n rows of \code{x}. 
#'     For classification, the values correspond to class labels and can be a 1xn matrix, 
#'     a simple vector or a factor. For regression, the values correspond to the values to predict, 
#'     and can be a 1xn matrix or a simple vector.
#' @param cluster.label an 1xn integer vector containing the cluster label of each sample of \code{x}
#' @param lambda the weight for the global l2-norm
#' @param sparse indicating whether the transformation results in a sparse matrix or not
#' @param valid.x the mxp validation data matrix.
#' @param valid.y if provided, it will be used to calculate the validation score with \code{valid.metric}
#' @param valid.metric the metric function for the validation result. By default it is the accuracy for classification
#'     or RMSE for regression. Customized metric is acceptable.
#' @param type the type of the mission for \code{LiblineaR}.
#' @param cost cost of constraints violation (default: 1). 
#'     Rules the trade-off between regularization and correct classification on data. 
#'     It can be seen as the inverse of a regularization constant. 
#'     See details in \code{LiblineaR}. 
#' @param epsilon set tolerance of termination criterion for optimization. 
#'     If NULL, the LIBLINEAR defaults are used, which are:
#' @param svr_eps set tolerance margin (epsilon) in regression loss function of SVR. Not used for classification methods.
#' @param bias if bias is \code{TRUE} (default), instances of data becomes [data; 1].
#' @param wi a named vector of weights for the different classes, 
#'     used for asymmetric class sizes. Not all factor levels have to be supplied (default weight: 1). 
#'     All components have to be named according to the corresponding class label. 
#'     Not used in regression mode.
#' @param verbose if set to 0, no information is printed. 
#'     If set to 1 (default), the running time and validation score (if applicable) will be printed.
#'     If set to 2, the running time ,validation score (if applicable) and the \code{LiblineaR} information will be printed.
#' @param seed the random seed. Set it to \code{NULL} to randomize the model.
#' @param cluster.FUN set to \code{kmeans} by default. Customized function is acceptable, 
#'     as long as the resulting list contains two fields named as \code{cluster} and \code{centers}.
#' @param ... additional parameters passing to \code{cluster.FUN}.
#' 
#' @examples
#' data(iris)
#' x=iris[,1:4]
#' y=factor(iris[,5])
#' train=sample(1:dim(iris)[1],100)
#' 
#' xTrain=x[train,]
#' xTest=x[-train,]
#' yTrain=y[train]
#' yTest=y[-train]
#' 
#' csvm.obj = clusterSVM(x = xTrain, y = yTrain, sparse = FALSE,
#'     centers = 2, iter.max = 1000, 
#'     valid.x = xTest,valid.y = yTest)
#' pred = predict(csvm.obj, xTest)
#' 
#' @export
#' 
clusterSVM = function(x, y, cluster.label = NULL, lambda = 1, sparse = TRUE, 
                      valid.x = NULL, valid.y = NULL, valid.metric = NULL,
                      type = 1, cost = 1, epsilon = NULL, svr_eps = NULL, 
                      bias = TRUE, wi = NULL, verbose = 1, seed = NULL,
                      cluster.FUN = stats::kmeans, ...) {
  
  if (lambda <= 0)
    stop("Invalid lambda. It must be greater than 0.")
  if (!is.null(seed))
    set.seed(seed)
  
  total.time.point = proc.time()
  time.point = proc.time()
  if (is.null(cluster.label)) {
    cluster.result = cluster.FUN(x, ...)
    if (is.null(cluster.result$cluster) || is.null(cluster.result$centers))
      stop("The result of the cluster function must be a list with
           two fields named as cluster and centers.")
    cluster.label = cluster.result$cluster
    cluster.centers = cluster.result$centers
    # cluster.label = match(cluster.label, unique(cluster.label))
    k = max(cluster.label)
    if (k==1)
      cluster.centers = matrix(cluster.centers,nrow=1)
  } else {
    # cluster.label = match(cluster.label, unique(cluster.label))
    k = max(cluster.label)
    cluster.centers = matrix(0,k,length(cluster.label))
    for (i in 1:k) {
      index = which(cluster.label == i)
      cluster.centers[i,] = colMeans(x[index,])
    }
  }
  if (verbose>0)
    cat('Time for Clustering:',(proc.time()-time.point)[3],'secs\n')
  time.point = proc.time()
  
  tilde.x = csvmTransform(x, lambda, cluster.label, sparse = sparse)
  
  if (verbose>0)
    cat('Time for Transforming:',(proc.time()-time.point)[3],'secs\n')
  time.point = proc.time()
  
  svm.result = LiblineaR(data = tilde.x, target = y, type = type, cost = cost, 
                         epsilon = epsilon, svr_eps = svr_eps, bias = bias,
                         wi = wi, cross = 0, verbose = (verbose>=2))
  
  if (verbose>0) {
    cat('Time for Liblinear:',(proc.time()-time.point)[3],'secs\n')
  }
  
  cluster.svm.result = list(svm = svm.result, 
                            lambda = lambda,
                            sparse = sparse,
                            label = cluster.label, 
                            centers = cluster.centers)
  cluster.svm.result = structure(cluster.svm.result, class = 'clusterSVM')
  
  if (!is.null(valid.x)) {
    time.point = proc.time()
    
    if (is.null(valid.y)) {
      cluster.svm.result$valid.pred = predict(cluster.svm.result, valid.x)$predictions
    } else {
      if (is.null(valid.metric)) {
        if (type<=7) {
          valid.metric = function(pred, truth) list(score = sum(pred==truth)/length(truth),
                                                    name = 'Accuracy')
        } else {
          # rmse
          valid.metric = function(pred, truth) list(score = sqrt(mean((pred-truth)^2)),
                                                    name = 'RMSE')
        }
      }
      cluster.svm.result$valid.pred = predict(cluster.svm.result, valid.x)$predictions
      valid.result = valid.metric(cluster.svm.result$valid.pred, valid.y)
      cluster.svm.result$valid.score = valid.result$score
      cluster.svm.result$valid.metric.name = valid.result$name
    }
    
    if (verbose>0)
      cat('Time for Validation:',(proc.time()-time.point)[3],'secs\n')
  }
  
  if (verbose>0) {
    cat('\n')
    cat('Total Time:',(proc.time()-total.time.point)[3],'secs\n')
    if (!is.null(cluster.svm.result$valid.score))
      cat(cluster.svm.result$valid.metric.name, 'Score:', cluster.svm.result$valid.score, '\n')
  }
  
  # cluster.svm.result = structure(cluster.svm.result, class = 'clusterSVM')
  return(cluster.svm.result)
}

#' Predictions with Clustered Support Vector Machines
#' 
#' The function applies a model (classification or regression) produced by the 
#'  \code{clusterSVM} function to every row of a data matrix and returns the model predictions.
#' 
#' @param object Object of class "clusterSVM", created by \code{clusterSVM}.
#' @param newdata An n x p matrix containing the new input data. Could be a matrix or a sparse matrix object.
#' @param ... other parameters passing to \code{predict.LiblineaR}
#' 
#' @export
#' 
setMethod("predict", signature = "clusterSVM",
          definition = function(object, newdata, ...) {
  
  if (class(object)!='clusterSVM')
    stop('Please predict with the model from clusterSVM.')
  
  if (missing(newdata))
    return(fitted(object$svm))
  
  # Assign label
  k = nrow(object$centers)
  if (k==1) {
    new.label = rep(1,nrow(newdata))
  } else {
    dist = eucliDist(newdata, object$centers)
    new.label = max.col(-dist)
  }
  
  # Transformation
  tilde.newdata = csvmTransform(newdata, object$lambda, new.label, object$sparse)
  
  # Make prediction
  preds = predict(object$svm, tilde.newdata, ...)
  return(preds)
})
