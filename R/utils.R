#' Euclidean Distance calculation
#' 
#' @param x the data matrix
#' @param centers the matrix of centers
#' 
eucliDist= function(x, centers) {
  assertInt(nrow(x), lower = 1)
  assertInt(ncol(x), lower = 1)
  assertMatrix(centers, ncols = ncol(x))
  if (nrow(centers)>1) {
    result = apply(centers, 1, function(C) colSums( (t(x)-C)^2 ))
  } else {
    result = colSums((t(x)-as.vector(centers))^2)
    result = matrix(result, length(result), 1)
  }
  assertMatrix(result, nrows = nrow(x), ncols = nrow(centers))
  return(result)
}

#' Euclidean Distance based clustering prediction
#' 
#' @param x the data matrix
#' @param cluster.object the matrix of centers
#' 
kmeans.predict = function(x, cluster.object) {
  assertMatrix(x)
  centers = cluster.object$centers
  assertMatrix(centers)
  
  euclidean.dist = eucliDist(x, centers)
  result = list()
  result = max.col(-euclidean.dist)
  assertInteger(result, lower = 1, upper = nrow(centers), len = nrow(x))
  return(result)
}

#' Kmeans Clustering from RcppMLPACK
#' 
#' The Kmeans algorithm from \code{RcppMLPACK}.
#' 
#' @param x The input data for the clustering algorithm.
#' @param centers A number indicating the number of clustering centers.
#' @param ... arguments for future use.
#' 
cluster.fun.mlpack = function(x, centers, ...) {

  assertInt(nrow(x), lower = 1)
  assertInt(ncol(x), lower = 1)
  assertInt(centers, lower = 1, upper = nrow(x))
  
  BBmisc::suppressAll({
    result = RcppMLPACK::mlKmeans(t(as.matrix(x)),centers)
  })
  result$cluster = result$result+1
  result$cluster= as.integer(result$cluster)
  k = max(result$cluster)
  cluster.centers = matrix(0,k,ncol(x))
  for (i in 1:k) {
    index = which(result$cluster == i)
    cluster.centers[i,] = colMeans(x[index,,drop = FALSE])
  }
  
  result$centers = cluster.centers
  result$clusters = NULL
  result$result = NULL
  assertMatrix(result$centers, min.rows = 1, ncols = ncol(x))
  assertInteger(result$cluster, lower = 1, upper = nrow(result$centers), 
                len = nrow(x))
  return(result)
}

cluster.predict.mlpack = function(x, cluster.object) {
  return(kmeans.predict(x, cluster.object))
}

sendMsg = function(..., verbose) {
  if (verbose)
    message(...)
}

#' Wrapper function for kernal kmeans
#' 
#' @param x the input data
#' @param centers the number of centers
#' @param ... other parameters passing to \code{kernlab::kkmeans}
#' 
cluster.fun.kkmeans = function(x, centers, ...) {
  # x = as.matrix(x)
  assertMatrix(x)
  # due to a wierd namespace problem i add this line
  tmp = kernlab::kkmeans(as.matrix(iris[,-5]), centers, ...)
  kernl.result = kernlab::kkmeans(x, centers, ...)
  result = list()
  result$cluster = kernl.result@.Data
  result$centers = kernl.result@centers
  result$kkmeans.res = kernl.result
  
  return(result)
}

#' Predict function for kernel kmeans
#' 
#' @param x The data to make prediction
#' @param cluster.object The result object from \code{kernlab::kkmeans}
#' 
cluster.predict.kkmeans = function(x, cluster.object) {
  kkmeans.res = cluster.object$kkmeans.res
  assertClass(kkmeans.res,'specc')
  
  kern.fun = kkmeans.res@kernelf@.Data
  center.mat = kkmeans.res@centers
  kern.mat = kernlab::kernelMatrix(kern.fun, x, center.mat)
  kern.c = diag(kernelMatrix(kern.fun,
                             kkmeans.res@centers,
                             kkmeans.res@centers))
  dist.mat = t(kern.c-2*t(kern.mat))

  result = list()
  result = max.col(-dist.mat)
  assertInteger(result, lower = 1, upper = nrow(center.mat), len = nrow(x))
  return(result)
}

#' Directly assign alpha to svm 
#' 
#' @param x the data matrix
#' @param nclass the total number of classes
#' @param class_rank the rank of class that the input data matrix has
#' 
oneclass.svm = function(x, nclass, class_rank) {
  n = nrow(x)
  y = sample(nclass, n, replace = TRUE)
  y = as.factor(y)
  model = alphasvm(x, y, scale = FALSE)
  model$index = 1:n
  model$SV = x
  model$tot.nSV = n
  if (nclass == 2) {
    if (class_rank==1) {
      model$coefs = rep(model$cost,n)
    } else {
      model$coefs = rep(-model$cost,n)
    }
  } else {
    if (class_rank==nclass) {
      model$coefs = matrix(0, n, nclass-1)
    } else {
      model$coefs = matrix(-model$cost, n, nclass-1)
      model$coefs[, class_rank] = model$cost
    }
  }
  return(model)
}


#' svmguide1
#' 
#' An astroparticle application from Jan Conrad of Uppsala University, Sweden. 
#' 
#' @docType data
#' @keywords datasets
#' @name svmguide1
#' @usage data(svmguide1)
#' @format A list of two data objects \code{svmguide1} and \code{svmguide1.t}. 
#'    The first column is the target variable.
#' 
NULL


