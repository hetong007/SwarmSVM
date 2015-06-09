#' @importClassesFrom SparseM matrix.csr
#' @importFrom kernlab kkmeans
#' @importFrom RcppMLPACK mlKmeans
#' @import Matrix
#' @import LiblineaR
#' @import e1071
#' @import methods

cluster.fun.mlpack = function(x, centers, ...) {
  result = mlKmeans(t(as.matrix(x)),centers[1])
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
  return(result)
}

sendMsg = function(..., verbose) {
  if (verbose)
    message(...)
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


