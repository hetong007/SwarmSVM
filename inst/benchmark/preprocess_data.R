source('utils.R')
require(Matrix)

########################
# For Clustered SVM
########################

# svmguide1
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/svmguide1',
              'svmguide1')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/svmguide1.t',
              'svmguide1.t')
svmguide1 = read.libsvm('svmguide1')
svmguide1.t = read.libsvm('svmguide1.t')
svmguide1[,-1] = rowl2norm(svmguide1[,-1])
svmguide1.t[,-1] = rowl2norm(svmguide1.t[,-1])
svmguide1 = list(svmguide1,svmguide1.t)
save(svmguide1, file='svmguide1.RData',compress = 'xz')

# ijcnn1
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.bz2',
              'ijcnn1.bz2')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.t.bz2',
              'ijcnn1.t.bz2')
ijcnn1 = read.libsvm(bzfile('ijcnn1.bz2'))
ijcnn1.t = read.libsvm(bzfile('ijcnn1.t.bz2'))
ijcnn1[,-1] = rowl2norm(ijcnn1[,-1])
ijcnn1.t[,-1] = rowl2norm(ijcnn1.t[,-1])
ijcnn1 = list(ijcnn1,ijcnn1.t)
save(ijcnn1,file='ijcnn1.RData',compress = 'xz')

# usps
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/multiclass/usps.bz2',
              'usps.bz2')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/multiclass/usps.t.bz2',
              'usps.t.bz2')
usps = read.libsvm(bzfile('usps.bz2'))
usps.t = read.libsvm(bzfile('usps.t.bz2'))
usps[,-1] = rowl2norm(usps[,-1])
usps.t[,-1] = rowl2norm(usps.t[,-1])
usps[,1] = as.numeric(usps[,1]%%2==0)
usps.t[,1] = as.numeric(usps.t[,1]%%2==0)
usps = list(usps,usps.t)
save(usps,file='usps.RData',compress = 'xz')

# mnist
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/multiclass/mnist.bz2',
              'mnist.bz2')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/multiclass/mnist.t.bz2',
              'mnist.t.bz2')
mnist = read.libsvm(bzfile('mnist.bz2'), dims=c(60000,782))
mnist.t = read.libsvm(bzfile('mnist.t.bz2'),dim=c(10000,782))
mnist[,-1] = rowl2norm(mnist[,-1])
mnist.t[,-1] = rowl2norm(mnist.t[,-1])

mnist38 = mnist[which(mnist[,1]==3 | mnist[,1]==8),]
mnist38.t = mnist.t[which(mnist.t[,1]==3 | mnist.t[,1]==8),]
mnist38[,1] = as.numeric(mnist38[,1]==3)
mnist38.t[,1] = as.numeric(mnist38.t[,1]==3)

mnist49 = mnist[which(mnist[,1]==4 | mnist[,1]==9),]
mnist49.t = mnist.t[which(mnist.t[,1]==4 | mnist.t[,1]==9),]
mnist49[,1] = as.numeric(mnist49[,1]==4)
mnist49.t[,1] = as.numeric(mnist49.t[,1]==4)

mnistoe = mnist
mnistoe[,1] = as.numeric(as.vector(mnist[,1])%%2==0)
mnistoe.t = mnist.t
mnistoe.t[,1] = as.numeric(as.vector(mnist.t[,1])%%2==0)

mnist = list(mnist38,mnist38.t,mnist49,mnist49.t,mnistoe,mnistoe.t)
save(mnist, file='mnist.RData',compress = 'xz')


########################
# For DC SVM
########################

# ijcnn1
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.bz2',
              'ijcnn1.bz2')
download.file('http://www.csie.ntu.edu.tw/%7Ecjlin/libsvmtools/datasets/binary/ijcnn1.t.bz2',
              'ijcnn1.t.bz2')
ijcnn1 = read.libsvm(bzfile('ijcnn1.bz2'))
ijcnn1.t = read.libsvm(bzfile('ijcnn1.t.bz2'))
ijcnn1[,-1] = rownorm(ijcnn1[,-1])
ijcnn1.t[,-1] = rownorm(ijcnn1.t[,-1])
ijcnn1 = list(ijcnn1,ijcnn1.t)
save(ijcnn1,file='ijcnn1.l1.RData',compress = 'xz')

# covtype.binary
download.file('http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/covtype.libsvm.binary.scale.bz2',
              'covtype.libsvm.binary.scale.bz2')
covtype = read.libsvm(bzfile('covtype.libsvm.binary.scale.bz2'))
set.seed(1024)
n = nrow(covtype)
ind = sample(n,n*0.2)
covtype.t = covtype[ind,]
covtype = covtype[-ind,]
covtype = list(covtype,covtype.t)
save(covtype,file = 'covtype.RData',compress = 'xz')



















