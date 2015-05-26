source('../R/utils.R')

# svmguide1
svmguide1 = read.libsvm('../data/svmguide1')
svmguide1.t = read.libsvm('../data/svmguide1.t')
svmguide1[,-1] = rowl2norm(svmguide1[,-1])
svmguide1.t[,-1] = rowl2norm(svmguide1.t[,-1])
save(svmguide1,svmguide1.t, file='../data/svmguide1.RData',compress = 'xz')

# ijcnn1
ijcnn1 = read.libsvm('../data/ijcnn1')
ijcnn1.t = read.libsvm('../data/ijcnn1.t')
ijcnn1[,-1] = rowl2norm(ijcnn1[,-1])
ijcnn1.t[,-1] = rowl2norm(ijcnn1.t[,-1])
save(ijcnn1,ijcnn1.t,file='../data/ijcnn1.RData',compress = 'xz')

# usps
usps = read.libsvm('../data/usps')
usps.t = read.libsvm('../data/usps.t')
usps[,-1] = rowl2norm(usps[,-1])
usps.t[,-1] = rowl2norm(usps.t[,-1])
usps[,1] = as.numeric(usps[,1]%%2==0)
usps.t[,1] = as.numeric(usps.t[,1]%%2==0)
save(usps,usps.t,file='../data/usps.RData',compress = 'xz')
