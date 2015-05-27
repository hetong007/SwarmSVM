require(LiblineaR)
require(Matrix)
require(SparseM)
require(kernlab)
source('../R/clusterSVM.R')

## SVMGUIDE1

load('../data/svmguide1.RData')
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 8, iter.max = 1000, 
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])

# Dense Matrix
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], sparse = FALSE,
                      centers = 8, iter.max = 1000, 
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])

# Self-Defined clustering algorithm
cluster.fun = function(x, centers, ...) {
  x = as.matrix(x)
  kernl.result = kkmeans(x, centers, ...)
  result = list()
  result$cluster = kernl.result@.Data
  result$centers = kernl.result@centers
  return(result)
}
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      cluster.FUN = cluster.fun, centers = 8, 
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])



## IJCNN1

load('../data/ijcnn1.RData')
csvm.obj = clusterSVM(x = ijcnn1[,-1], y = ijcnn1[,1],
                      centers = 8, iter.max = 1000, 
                      valid.x = ijcnn1.t[,-1],valid.y = ijcnn1.t[,1])



## USPS
load('../data/usps.RData')
csvm.obj = clusterSVM(x = usps[,-1], y = usps[,1],
                      centers = 8, iter.max = 1000, 
                      valid.x = usps.t[,-1],valid.y = usps.t[,1])



## MNIST
load('../data/mnist.RData')

# 3 vs 8
csvm.obj = clusterSVM(x = mnist38[,-1], y = mnist38[,1],
                      centers = 8, iter.max = 1000, 
                      valid.x = mnist38.t[,-1],valid.y = mnist38.t[,1])

# 4 vs 9
csvm.obj = clusterSVM(x = mnist49[,-1], y = mnist49[,1],
                      centers = 8, iter.max = 1000, 
                      valid.x = mnist49.t[,-1],valid.y = mnist49.t[,1])

# O vs E
csvm.obj = clusterSVM(x = mnistoe[,-1], y = mnistoe[,1],
                      centers = 8, iter.max = 1000, 
                      valid.x = mnistoe.t[,-1],valid.y = mnistoe.t[,1])



