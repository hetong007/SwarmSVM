require(EnsembleSVM)

## SVMGUIDE1
data(svmguide1)
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 8, iter.max = 1000, seed = 512, verbose = 0,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
# Time for Clustering: 0.022 secs
# Time for Transforming: 0.026 secs
# Time for Liblinear: 0.202 secs
# Time for Validation: 0.049 secs
# 
# Total Time: 0.299 secs
# Accuracy Score: 0.80425 

# Dense Matrix
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], sparse = FALSE,
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
# Time for Clustering: 0.022 secs
# Time for Transforming: 0.003 secs
# Time for Liblinear: 0.128 secs
# Time for Validation: 0.018 secs
# 
# Total Time: 0.173 secs
# Accuracy Score: 0.80425

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
# Time for Clustering: 11.925 secs
# Time for Transforming: 0.023 secs
# Time for Liblinear: 0.144 secs
# Time for Validation: 0.034 secs
# 
# Total Time: 12.127 secs
# Accuracy Score: 0.81325 

# Only one cluster
csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                      centers = 1, iter.max = 1000, seed = 512,
                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
# Time for Clustering: 0.002 secs
# Time for Transforming: 0.005 secs
# Time for Liblinear: 0.093 secs
# Time for Validation: 0.008 secs
# 
# Total Time: 0.108 secs
# Accuracy Score: 0.79125 



## IJCNN1
data(ijcnn1)
csvm.obj = clusterSVM(x = ijcnn1[,-1], y = ijcnn1[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = ijcnn1.t[,-1],valid.y = ijcnn1.t[,1])
# Time for Clustering: 0.2 secs
# Time for Transforming: 0.306 secs
# Time for Liblinear: 1.972 secs
# Time for Validation: 2.059 secs
# 
# Total Time: 4.547 secs
# Accuracy Score: 0.9425742 



## USPS
data(usps)
csvm.obj = clusterSVM(x = usps[,-1], y = usps[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = usps.t[,-1],valid.y = usps.t[,1])
# Time for Clustering: 1.699 secs
# Time for Transforming: 1.03 secs
# Time for Liblinear: 1.334 secs
# Time for Validation: 0.608 secs
# 
# Total Time: 4.676 secs
# Accuracy Score: 0.9531639



## MNIST
data(mnist)

# 3 vs 8
csvm.obj = clusterSVM(x = mnist38[,-1], y = mnist38[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = mnist38.t[,-1],valid.y = mnist38.t[,1])
# Time for Clustering: 11.777 secs
# Time for Transforming: 2.925 secs
# Time for Liblinear: 2.531 secs
# Time for Validation: 0.74 secs
# 
# Total Time: 17.976 secs
# Accuracy Score: 0.984375 

# 4 vs 9
csvm.obj = clusterSVM(x = mnist49[,-1], y = mnist49[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = mnist49.t[,-1],valid.y = mnist49.t[,1])
# Time for Clustering: 8.846 secs
# Time for Transforming: 2.156 secs
# Time for Liblinear: 2.305 secs
# Time for Validation: 0.73 secs
# 
# Total Time: 14.041 secs
# Accuracy Score: 0.9804119

# O vs E
csvm.obj = clusterSVM(x = mnistoe[,-1], y = mnistoe[,1],
                      centers = 8, iter.max = 1000, seed = 512,
                      valid.x = mnistoe.t[,-1],valid.y = mnistoe.t[,1])
# Time for Clustering: 62.051 secs
# Time for Transforming: 11.932 secs
# Time for Liblinear: 13.056 secs
# Time for Validation: 4.122 secs
# 
# Total Time: 91.175 secs
# Accuracy Score: 0.9611 



### Replicated Experiments
rep.len = 10

score = rep(0,rep.len)
for (i in 1:rep.len) {
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                        centers = 8, iter.max = 1000, seed = i, verbose = 0,
                        valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  score[i] = csvm.obj$valid.score
}
cat(mean(score),'+',sd(score))
# 0.81355 + 0.009937863

score = rep(0,rep.len)
for (i in 1:rep.len) {
  csvm.obj = clusterSVM(x = ijcnn1[,-1], y = ijcnn1[,1],
                        centers = 8, iter.max = 1000, seed = i, verbose = 0,
                        valid.x = ijcnn1.t[,-1],valid.y = ijcnn1.t[,1])
  score[i] = csvm.obj$valid.score
}
cat(mean(score),'+',sd(score))
# 0.9446353 + 0.001768465

score = rep(0,rep.len)
for (i in 1:rep.len) {
  csvm.obj = clusterSVM(x = usps[,-1], y = usps[,1],
                        centers = 8, iter.max = 1000, seed = i, verbose = 0,
                        valid.x = usps.t[,-1],valid.y = usps.t[,1])
  score[i] = csvm.obj$valid.score
}
cat(mean(score),'+',sd(score))
# 0.9566019 + 0.0009232315

score = rep(0,rep.len)
for (i in 1:rep.len) {
  csvm.obj = clusterSVM(x = mnist38[,-1], y = mnist38[,1],
                        centers = 8, iter.max = 1000, seed = i, verbose = 0,
                        valid.x = mnist38.t[,-1],valid.y = mnist38.t[,1])
  score[i] = csvm.obj$valid.score
}
cat(mean(score),'+',sd(score))
# 0.9849294 + 0.001800931

score = rep(0,rep.len)
for (i in 1:rep.len) {
  csvm.obj = clusterSVM(x = mnist49[,-1], y = mnist49[,1],
                        centers = 8, iter.max = 1000, seed = i, verbose = 0,
                        valid.x = mnist49.t[,-1],valid.y = mnist49.t[,1])
  score[i] = csvm.obj$valid.score
}
cat(mean(score),'+',sd(score))
# 0.9805625 + 0.001441175

score = rep(0,rep.len)
for (i in 1:rep.len) {
  csvm.obj = clusterSVM(x = mnistoe[,-1], y = mnistoe[,1],
                        centers = 8, iter.max = 1000, seed = i, verbose = 0,
                        valid.x = mnistoe.t[,-1],valid.y = mnistoe.t[,1])
  score[i] = csvm.obj$valid.score
}
cat(mean(score),'+',sd(score))
# 0.96069 + 0.000953881
