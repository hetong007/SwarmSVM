require(LiblineaR)
require(e1071)
require(SwarmSVM)

######## Data Preparation
local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/svmguide1.RData",local.file.name)
load(local.file.name)
svmguide1.t = svmguide1[[2]]
svmguide1 = svmguide1[[1]]

local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/ijcnn1.RData",local.file.name)
load(local.file.name)
ijcnn1.t = ijcnn1[[2]]
ijcnn1 = ijcnn1[[1]]

local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/usps.RData",local.file.name)
load(local.file.name)
usps.t = usps[[2]]
usps = usps[[1]]

local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/mnist.RData",local.file.name)
load(local.file.name)
mnist38 = mnist[[1]]
mnist38.t = mnist[[2]]
mnist49 = mnist[[3]]
mnist49.t = mnist[[4]]
mnistoe = mnist[[5]]
mnistoe.t = mnist[[6]]

######## Repeat Length
rep.len = 10

########################
######## Cluster SVM
########################

clusterSVM.cv = function(x, y, nfold = 5, ...) {
  n = nrow(x)
  ind = sample(n)
  folds = list()
  for (i in 1:(nfold-1)) {
    folds[[i]] = ind[1:(n %/% nfold)]
    ind = setdiff(ind,folds[[i]])
  }
  folds[[nfold]] = ind
  
  score = rep(0,nfold)
  for (i in 1:nfold) {
    train.ind = setdiff(1:n, folds[[i]])
    test.ind = folds[[i]]
    csvm.obj = SwarmSVM::clusterSVM(x = x[train.ind, ], y = y[train.ind], 
                                    valid.x = x[test.ind, ], valid.y = y[test.ind], 
                                    ...)
    score[i] = csvm.obj$valid.score
  }
  return(mean(score))
}

repeatClusterSVM = function(train, valid, rep.len = 10) {
  train = as.matrix(train)
  valid = as.matrix(valid)
  best.score = -Inf
  set.seed(1024)
  for (lmd in c(1,5,10,20,50,100)) {
    temp.score = clusterSVM.cv(x = train[,-1], y = train[,1], lambda = lmd,
                               centers = 8, iter.max = 1000, verbose = 0)
    if (temp.score>best.score) {
      best.score = temp.score
      best.lambda = lmd
    }
  }
  
  score = rep(0, rep.len)
  total.time = rep(0, rep.len)
  for (i in 1:rep.len) {
    csvm.obj = SwarmSVM::clusterSVM(x = train[,-1], y = train[,1], seed = i, 
                                    valid.x = valid[,-1], valid.y = valid[,1], 
                                    centers = 8, iter.max = 1000, verbose = 0,
                                    lambda = best.lambda)
    score[i] = csvm.obj$valid.score
    total.time[i] = csvm.obj$time$total.time
  }
  gc()
  result = c(mean(score), sd(score), mean(total.time), sd(total.time), best.lambda)
  names(result) = c('Average Error', 'Standard Deviation', 
                    'Average Time', 'Standard Deviation', 'Best Lambda')
  result = round(result, 7)
  return(result)
}

repeatClusterSVM(svmguide1, svmguide1.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation        Best Lambda 
# 0.8143000          0.0091225          0.1589000          0.0143562         20.0000000 

repeatClusterSVM(ijcnn1, ijcnn1.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation        Best Lambda 
# 0.9444030          0.0022965          3.6295000          0.1568306          1.0000000 

repeatClusterSVM(usps, usps.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation        Best Lambda 
# 0.9552566          0.0011697          2.2441000          0.2853292          1.0000000 

repeatClusterSVM(mnist38, mnist38.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation        Best Lambda 
# 0.9855847          0.0012162         10.4385000          1.9813294          5.0000000 

repeatClusterSVM(mnist49, mnist49.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation        Best Lambda 
# 0.980663           0.001575          12.641600           1.430985           1.000000 

repeatClusterSVM(mnistoe, mnistoe.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 9.612300e-01       1.766981e-04       6.303360e+01       9.137391e+00 


########################
######## LibLinear
########################

rep.len = 10

repeatLiblineaR = function(train, valid, rep.len = 10) {
  train = as.matrix(train)
  valid = as.matrix(valid)
  best.score = -Inf
  set.seed(1024)
  for (cst in c(0.01,0.1,1,10,100)) {
    temp.score = LiblineaR::LiblineaR(data = train[,-1], target = train[,1], 
                                      type = 1, verbose = F, cost = cst, cross = 5)
    if (temp.score>best.score) {
      best.score = temp.score
      best.cost = cst
    }
  }
  score = rep(0, rep.len)
  total.time = rep(0, rep.len)
  for (i in 1:rep.len) {
    set.seed(i)
    time.stamp = proc.time()
    liblinear.obj = LiblineaR::LiblineaR(data = train[,-1], target = train[,1], 
                                         type = 1, verbose = F, cost = best.cost)
    preds = predict(liblinear.obj, valid[,-1])$prediction
    score[i] = sum(preds==valid[,1])/length(valid[,1])
    total.time[i] = (proc.time()-time.stamp)[3]
  }
  result = c(mean(score), sd(score), mean(total.time), sd(total.time), best.cost)
  names(result) = c('Average Error', 'Standard Deviation', 
                    'Average Time', 'Standard Deviation', 'Best Cost')
  result = round(result, 7)
  return(result)
}

repeatLiblineaR(svmguide1, svmguide1.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 0.7912750          0.0000791          0.1490000          0.0313723 

repeatLiblineaR(ijcnn1, ijcnn1.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 0.9207958          0.0000293          1.8798000          0.0668079 

repeatLiblineaR(usps, usps.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 0.9367215          0.0000000          0.4671000          0.0416505 

repeatLiblineaR(mnist38, mnist38.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 0.7561492          0.0002125          1.0245000          0.0372834 

repeatLiblineaR(mnist49, mnist49.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 0.9432446          0.0000000          0.8407000          0.1075227 

repeatLiblineaR(mnistoe, mnistoe.t, rep.len)
# Average Error Standard Deviation       Average Time Standard Deviation 
# 0.9021600          0.0000699          5.8414000          0.1436548 


########################
######## Kernel SVM
########################

svm.cv = function(x, y, nfold = 5, ...) {
  n = nrow(x)
  ind = sample(n)
  folds = list()
  for (i in 1:(nfold-1)) {
    folds[[i]] = ind[1:(n %/% nfold)]
    ind = setdiff(ind,folds[[i]])
  }
  folds[[nfold]] = ind
  
  score = rep(0,nfold)
  for (i in 1:nfold) {
    train.ind = setdiff(1:n, folds[[i]])
    test.ind = folds[[i]]
    svm.obj = e1071::svm(x = x[train.ind,], y = as.factor(y[train.ind]), ...)
    preds = predict(svm.obj, x[test.ind,], probability = FALSE)
    score[i] = sum(preds==y[test.ind])/length(y[test.ind])
  }
  return(mean(score))
}

rep.len = 10

repeatSVM = function(train, valid, rep.len = 10) {
  best.score = -Inf
  set.seed(1024)
  for (gm in c(0.01,0.1,1,10,100)) {
    for (cst in c(0.01,0.1,1,10,100)) {
      temp.score = svm.cv(x = train[,-1], y = train[,1], nfold = 5, 
                          gamma = gm, cost = cst, kernel = "radial")
      if (temp.score>best.score) {
        best.score = temp.score
        best.gamma = gm
        best.cost = cst
      }
    }
  }
  score = rep(0, rep.len)
  total.time = rep(0, rep.len)
  for (i in 1:rep.len) {
    set.seed(i)
    time.stamp = proc.time()
    svm.obj = e1071::svm(x = train[,-1], y = as.factor(train[,1]), 
                         kernel = "radial", gamma = best.gamma, cost = best.cost)
    preds = predict(svm.obj, valid[,-1])
    presd = as.numeric(preds)-1
    score[i] = sum(preds==valid[,1])/length(valid[,1])
    total.time[i] = (proc.time()-time.stamp)[3]
  }
  result = c(mean(score), sd(score), mean(total.time), sd(total.time), best.gamma, best.cost)
  names(result) = c('Average Error', 'Standard Deviation', 
                    'Average Time', 'Standard Deviation', 'Best Gamma', 'Best Cost')
  result = round(result, 7)
  return(result)
}

repeatSVM(svmguide1, svmguide1.t, rep.len)

repeatLiblineaR(ijcnn1, ijcnn1.t, rep.len)

repeatLiblineaR(usps, usps.t, rep.len)

repeatLiblineaR(mnist38, mnist38.t, rep.len)

repeatLiblineaR(mnist49, mnist49.t, rep.len)

repeatLiblineaR(mnistoe, mnistoe.t, rep.len)
