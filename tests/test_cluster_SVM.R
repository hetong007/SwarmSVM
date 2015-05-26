require(LiglineaR)
require(Matrix)
require(SparseM)
require(AUC)
source('../R/clusterSVM.R')
source('../R/utils.R')

# svmguide1 = read.libsvm('../data/svmguide1')
# svmguide1.t = read.libsvm('../data/svmguide1.t')
# save(svmguide1,svmguide1.t, file='../data/svmguide1.RData')
load('../data/svmguide1.RData')

set.seed(1024)
# Dense Matrix
system.time({
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                        centers = 5, iter.max = 1000, sparse = FALSE)
})
preds = predict.clusterSVM(csvm.obj, svmguide1.t[,-1])
sum(preds$predictions==svmguide1.t[,1])/nrow(svmguide1.t)

# Sparse Matrix
system.time({
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                        centers = 5, iter.max = 1000, sparse = TRUE)
})
preds = predict.clusterSVM(csvm.obj, svmguide1.t[,-1])
sum(preds$predictions==svmguide1.t[,1])/nrow(svmguide1.t)

