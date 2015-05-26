require(LiblineaR)
require(Matrix)
require(SparseM)
require(kernlab)
source('../R/clusterSVM.R')

## SVMGUIDE1

load('../data/svmguide1.RData')
system.time({
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                        centers = 8, iter.max = 1000)
})
preds = predict.clusterSVM(csvm.obj, svmguide1.t[,-1])
sum(preds$predictions==svmguide1.t[,1])/nrow(svmguide1.t)

# Dense Matrix
system.time({
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                        centers = 8, iter.max = 1000, sparse = FALSE)
})
preds = predict.clusterSVM(csvm.obj, svmguide1.t[,-1])
sum(preds$predictions==svmguide1.t[,1])/nrow(svmguide1.t)

# Self-Defined clustering algorithm
cluster.fun = function(x, centers, ...) {
  x = as.matrix(x)
  kernl.result = kkmeans(x, centers, ...)
  result = list()
  result$cluster = kernl.result@.Data
  result$centers = kernl.result@centers
  return(result)
}
system.time({
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], sparse = FALSE,
                        cluster.FUN = cluster.fun, centers = 8)
})
preds = predict.clusterSVM(csvm.obj, svmguide1.t[,-1])
sum(preds$predictions==svmguide1.t[,1])/nrow(svmguide1.t)



## IJCNN1

load('../data/ijcnn1.RData')
system.time({
  csvm.obj = clusterSVM(x = ijcnn1[,-1], y = ijcnn1[,1],
                        centers = 8, iter.max = 1000)
})
preds = predict.clusterSVM(csvm.obj, ijcnn1.t[,-1])
sum(preds$predictions==ijcnn1.t[,1])/nrow(ijcnn1.t)



## USPS
load('../data/usps.RData')
system.time({
  csvm.obj = clusterSVM(x = usps[,-1], y = usps[,1],
                        centers = 8, iter.max = 1000)
})
preds = predict.clusterSVM(csvm.obj, usps.t[,-1])
sum(preds$predictions==usps.t[,1])/nrow(usps.t)

