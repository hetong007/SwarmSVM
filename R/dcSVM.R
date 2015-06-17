dcSVM = function(x, y, k, m, C, kernel, max.levels, sample.size) {
  n = nrow(x)
  for (lvl in max.levels:1) {
    if (lvl==max.levels) 
      ind = sample(1:n,m)
    else
      ind = sample(support,m)
    
    kkmeans.res = kernlab::kkmeans(x[ind,], centers = k, kernel = kernel)
    
    # Very Slow process, looking for faster prediction version
    kern.fun = kkmeans.res@kernelf@.Data
    center.mat = kkmeans.res@centers
    cluster.label = rep(0,n)
    for (i in 1:n) {
      dist.vec= rep(0,k)
      for (j in 1:k) {
        dist.vec[j,] = kern.fun(x[i,],center.mat[j,])
      }
      cluster.label[i] = which.min(dist.vec)
    }
    
    for (clst in 1:k) {
      ind = which(cluster.label == clst)
      # train the svm with given support vectors
    }
    # get new support vectors
  }
  # fine train the model
  # output 
}
