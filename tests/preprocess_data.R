# source('../R/utils.R')
# require(Matrix)
# 
# rowl2norm = function(x) {
#   rs = 1/sqrt(rowSums(x^2))
#   new.x = Diagonal(x=rs) %*% x
#   return(new.x)
# }
# 
# # svmguide1
# svmguide1 = read.libsvm('../data/svmguide1')
# svmguide1.t = read.libsvm('../data/svmguide1.t')
# svmguide1[,-1] = rowl2norm(svmguide1[,-1])
# svmguide1.t[,-1] = rowl2norm(svmguide1.t[,-1])
# svmguide1 = list(svmguide1,svmguide1.t)
# save(svmguide1, file='../data/svmguide1.RData',compress = 'xz')
# 
# # ijcnn1
# ijcnn1 = read.libsvm('../data/ijcnn1')
# ijcnn1.t = read.libsvm('../data/ijcnn1.t')
# ijcnn1[,-1] = rowl2norm(ijcnn1[,-1])
# ijcnn1.t[,-1] = rowl2norm(ijcnn1.t[,-1])
# ijcnn1 = list(ijcnn1,ijcnn1.t)
# save(ijcnn1,file='../data/ijcnn1.RData',compress = 'xz')
# 
# # usps
# usps = read.libsvm('../data/usps')
# usps.t = read.libsvm('../data/usps.t')
# usps[,-1] = rowl2norm(usps[,-1])
# usps.t[,-1] = rowl2norm(usps.t[,-1])
# usps[,1] = as.numeric(usps[,1]%%2==0)
# usps.t[,1] = as.numeric(usps.t[,1]%%2==0)
# usps = list(usps,usps.t)
# save(usps,file='../data/usps.RData',compress = 'xz')
# 
# # mnist
# mnist = read.libsvm('../data/mnist', dims=c(60000,782))
# mnist.t = read.libsvm('../data/mnist.t',dim=c(10000,782))
# mnist[,-1] = rowl2norm(mnist[,-1])
# mnist.t[,-1] = rowl2norm(mnist.t[,-1])
# 
# mnist38 = mnist[which(mnist[,1]==3 | mnist[,1]==8),]
# mnist38.t = mnist.t[which(mnist.t[,1]==3 | mnist.t[,1]==8),]
# mnist38[,1] = as.numeric(mnist38[,1]==3)
# mnist38.t[,1] = as.numeric(mnist38.t[,1]==3)
# 
# mnist49 = mnist[which(mnist[,1]==4 | mnist[,1]==9),]
# mnist49.t = mnist.t[which(mnist.t[,1]==4 | mnist.t[,1]==9),]
# mnist49[,1] = as.numeric(mnist49[,1]==4)
# mnist49.t[,1] = as.numeric(mnist49.t[,1]==4)
# 
# mnistoe = mnist
# mnistoe[,1] = as.numeric(as.vector(mnist[,1])%%2==0)
# mnistoe.t = mnist.t
# mnistoe.t[,1] = as.numeric(as.vector(mnist.t[,1])%%2==0)
# 
# mnist = list(mnist38,mnist38.t,mnist49,mnist49.t,mnistoe,mnistoe.t)
# save(mnist, file='../data/mnist.RData',compress = 'xz')
