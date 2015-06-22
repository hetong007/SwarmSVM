#' @importClassesFrom SparseM matrix.csr
#' @importFrom SparseM t
#' @importFrom kernlab kkmeans
#' @importFrom kernlab kernelMatrix
#' @importFrom RcppMLPACK mlKmeans
#' @import Matrix
#' @import LiblineaR
#' @import methods
#' @import checkmate
#' @importFrom BBmisc suppressAll
#' @useDynLib SwarmSVM
NULL

#' Euclidean Distance calculation
#' 
#' @param x the data matrix
#' @param centers the matrix of centers
#' 
eucliDist= function(x, centers) {
  if (nrow(centers)>1) {
    result = apply(centers, 1, function(C) colSums( (t(x)-C)^2 ))
  } else {
    result = colSums((t(x)-as.vector(centers))^2)
  }
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
  if (is.matrix(centers)) {
    result = list()
    result$centers = centers
    k = nrow(centers)
    if (k==1) {
      result$cluster = rep(1,nrow(x))
    } else {
      dist = eucliDist(x, centers)
      result$cluster = max.col(-dist)
    }
  } else {
    BBmisc::suppressAll({
      result = mlKmeans(t(as.matrix(x)),centers[1])
    })
    result$cluster = result$result+1
    k = max(result$cluster)
    cluster.centers = matrix(0,k,ncol(x))
    for (i in 1:k) {
      index = which(result$cluster == i)
      cluster.centers[i,] = colMeans(x[index,,drop = FALSE])
    }
    result$centers = cluster.centers
    result$clusters = NULL
    result$result = NULL
  }
  return(result)
}

sendMsg = function(..., verbose) {
  if (verbose)
    message(...)
}

#' Predict function for kernel kmeans
#' 
#' @param kkmeans.res The result object from \code{kernlab::kkmeans}
#' @param x The data to make prediction
#' 
kern.predict = function(kkmeans.res, x) {
  kern.fun = kkmeans.res@kernelf@.Data
  center.mat = kkmeans.res@centers
  kern.mat = kernlab::kernelMatrix(kern.fun, x, center.mat)
  kern.c = diag(kernelMatrix(kern.fun,
                             kkmeans.res@centers,
                             kkmeans.res@centers))
  dist.mat = t(kern.c-2*t(kern.mat))
  result = max.col(-dist.mat)
  return(result)
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


