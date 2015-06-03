require(EnsembleSVM)

## Load the demo data
data(svmguide1)
svmguide1.t = svmguide1[[2]]
svmguide1 = svmguide1[[1]]
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 8, iter.max = 1000, seed = 512, verbose = 0,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])

# Dense Matrix
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], sparse = FALSE,
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])


# Self-Defined clustering algorithm
# The requirement of the function is having two fields named as $cluster and $centers 
# in the result.
cluster.fun = function(x, centers, ...) {
  x = as.matrix(x)
  kernl.result = kernlab::kkmeans(x, centers, ...)
  result = list()
  result$cluster = kernl.result@.Data
  result$centers = kernl.result@centers
  return(result)
}
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], seed = 512,
                      cluster.FUN = cluster.fun, centers = 8, 
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])

# Only one cluster, the result is the same as LiblineaR on the original data
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 1, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
