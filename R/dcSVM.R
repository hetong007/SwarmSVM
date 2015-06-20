dcSVM = function(x, y, k, m, C, kernel, max.levels, early, ...) {
  n = nrow(x)
  support = rep(0,n)
  num.lvls = length(levels(as.factor(y)))
  alpha = matrix(0,n,num.lvls-1)
  
  checkmate::assertInt(kernel, lower = 1, upper = 3)
  kernlab.kernel = c('vanilladot','polydot','rbfdot')[kernel]
  svm.kernel = c('linear','polynomial','radial')[kernel]
  
  kpar = list(...)
  if (kernel==3) {
    if (!is.null(kpar$gamma))
      kpar$sigma = 1/kpar$gamma
  } else if (kernel==2) {
    if (!is.null(kpar$coef0))
      kpar$offset = kpar$coef0
    if (!is.null(kpar$gamma))
      kpar$scale = 1/kpar$gamma
  }
  
  for (lvl in max.levels:1) {
    kl = k^lvl
    if (lvl == max.levels)
      ind = sample(1:n,m)
    else
      ind = sample(support,m)
    
    kkmeans.res = kernlab::kkmeans(x[ind,], centers = kl, kpar = kpar,
                                   kernel = kernlab.kernel)
    
    # Very Slow process, looking for faster prediction version
    kern.fun = kkmeans.res@kernelf@.Data
    center.mat = kkmeans.res@centers
    cluster.label = rep(0,n)
    for (i in 1:n) {
      dist.vec= rep(0,kl)
      for (j in 1:kl) {
        dist.vec[j,] = kern.fun(x[i,],center.mat[j,])
      }
      cluster.label[i] = which.min(dist.vec)
    }
    
    # Train svm for each cluster
    new.alpha = matrix(0,n,num.lvls-1)
    new.support = rep(FALSE,n)
    svm.models = list()
    for (clst in 1:kl) {
      ind = which(cluster.label == clst)
      # train the svm with given support vectors
      if (lvl == max.levels)
        svm.model = svm(x = x[ind,], y = y[ind], kernel = svm.kernel, ...)
      else 
        svm.model = svm(x = x[ind,], y = y[ind], kernel = svm.kernel, 
                        alpha = alpha[ind,], ...)
      svm.models[[clst]] = svm.model
      sv.ind = ind[svm.model$index]
      new.support[sv.ind,] = TRUE
      new.alpha[sv.ind,] = svm.model$coefs
    }
    support = which(new.support)
    alpha = new.alpha
  }
  if (!early){
    # Refine
    svm.models = svm(x = x[support,], y = y[support], kernel = svm.kernel, 
                     alpha = alpha[support,], ...)
    alpha[support,] = svm.model$coefs
    
    # Final
    svm.models = svm(x = x, y = y, kernel = svm.kernel, alpha = alpha, ...)
  }
  # Result structure
  result = list(svm = svm.models,
                cluster.fun = kern.fun,
                centers = center.mat,
                early = early)
  result = structure(result, class = "dcSVM")
  return(result)
}

predict.dcSVM = function(object, newdata, ...) {
  
  assertClass(object, 'dcSVM')
  
  if (missing(newdata))
    return(fitted(object$svm))
  
  # Assign label
  if (early) {
    new.result = object$cluster.fun(newdata, object$centers)
    k = max(new.result)
    preds = rep(0, nrow(newdata))
    for (i in 1:k) {
      ind = which(new.result == i)
      preds[ind] = predict(object$svm[[i]], newdata[ind,], ...)
    }
  } else {
    preds = predict(object$svm, newdata, ...)
  }
  return(preds)
}
