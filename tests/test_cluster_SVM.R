require(EnsembleSVM)

## SVMGUIDE1
data(svmguide1)
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
# Time for Clustering: 0.019 secs
# Time for Transforming: 0.012 secs
# Time for Liblinear: 0.116 secs
# Time for Validation: 0.031 secs
# 
# Total Time: 0.179 secs
# Accuracy Score: 0.75775 

# Dense Matrix
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], sparse = FALSE,
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
# Time for Clustering: 0.021 secs
# Time for Transforming: 0.003 secs
# Time for Liblinear: 0.142 secs
# Time for Validation: 0.017 secs
# 
# Total Time: 0.186 secs
# Accuracy Score: 0.75775 

# Self-Defined clustering algorithm
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
# Time for Clustering: 10.963 secs
# Time for Transforming: 0.012 secs
# Time for Liblinear: 0.118 secs
# Time for Validation: 0.03 secs
# 
# Total Time: 11.123 secs
# Accuracy Score: 0.69025 

# Only one cluster
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 1, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
# Time for Clustering: 0.003 secs
# Time for Transforming: 0.003 secs
# Time for Liblinear: 0.089 secs
# Time for Validation: 0.007 secs
# 
# Total Time: 0.103 secs
# Accuracy Score: 0.79125 



## IJCNN1
data(ijcnn1)
csvm.obj = clusterSVM(x = ijcnn1[,-1], y = ijcnn1[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = ijcnn1.t[,-1],valid.y = ijcnn1.t[,1])
# Time for Clustering: 0.203 secs
# Time for Transforming: 0.311 secs
# Time for Liblinear: 1.738 secs
# Time for Validation: 1.507 secs
# 
# Total Time: 3.769 secs
# Accuracy Score: 0.8907536 



## USPS
data(usps)
csvm.obj = clusterSVM(x = usps[,-1], y = usps[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = usps.t[,-1],valid.y = usps.t[,1])
# Time for Clustering: 1.64 secs
# Time for Transforming: 0.587 secs
# Time for Liblinear: 0.783 secs
# Time for Validation: 0.475 secs
# 
# Total Time: 3.491 secs
# Accuracy Score: 0.8330842 



## MNIST
data(mnist)

# 3 vs 8
csvm.obj = clusterSVM(x = mnist38[,-1], y = mnist38[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = mnist38.t[,-1],valid.y = mnist38.t[,1])
# Time for Clustering: 28.124 secs
# Time for Transforming: 2.122 secs
# Time for Liblinear: 2.168 secs
# Time for Validation: 0.78 secs
# 
# Total Time: 33.199 secs
# Accuracy Score: 0.8165323 

# 4 vs 9
csvm.obj = clusterSVM(x = mnist49[,-1], y = mnist49[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = mnist49.t[,-1],valid.y = mnist49.t[,1])
# Time for Clustering: 8.806 secs
# Time for Transforming: 1.873 secs
# Time for Liblinear: 2.063 secs
# Time for Validation: 0.756 secs
# 
# Total Time: 13.501 secs
# Accuracy Score: 0.9211452

# O vs E
csvm.obj = clusterSVM(x = mnistoe[,-1], y = mnistoe[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = mnistoe.t[,-1],valid.y = mnistoe.t[,1])
# Time for Clustering: 64.188 secs
# Time for Transforming: 11.14 secs
# Time for Liblinear: 12.517 secs
# Time for Validation: 4.066 secs
# 
# Total Time: 91.925 secs
# Accuracy Score: 0.7936 



