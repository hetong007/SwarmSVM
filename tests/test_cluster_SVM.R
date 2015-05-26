require(e1071)
require(Matrix)
require(AUC)

# svmguide1 = read.libsvm('../data/svmguide1')
# svmguide1.t = read.libsvm('../data/svmguide1.t')
# save(svmguide1,svmguide1.t, file='../data/svmguide1.RData')
load('../data/svmguide1.RData')

system.time({
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1],
                        centers = 5, iter.max = 1000)
})
preds = predict.clusterSVM(csvm.obj, svmguide1.t[,-1])
auc(roc(preds,as.factor(svmguide1.t[,1])))
