csvmTransform = function(x, lambda, cluster.label, sparse = TRUE) {
  n = nrow(x)
  m = ncol(x)
  k = max(cluster.label)
  
  if (sparse){
    # x = as(x,'dgCMatrix')
    row.index = NULL
    col.index = NULL
    val = NULL
    for (i in 1:k) {
      row.index = c(row.index, rep(which(cluster.label == i),times = m))
      col.index = c(col.index, rep((1 + (i-1)*m):(i*m),each = sum(cluster.label == i)))
      val = c(val, as.vector(x[which(cluster.label == i),]))
    }
    tilde.x = spMatrix(n, k*m, i = row.index, j = col.index, x = val)
    tilde.x = cBind(x / sqrt(lambda), tilde.x)
    tilde.x = as(tilde.x,'dgCMatrix')
    tilde.x = as(tilde.x, 'matrix.csr')
  } else {
    x = as(x,'matrix')
    tilde.x = matrix(0,n,k*m)
    for (i in 1:k) {
      row.index = which(cluster.label == i)
      col.index = (1 + (i-1)*m):(i*m)
      tilde.x[row.index, col.index] = x[row.index,]
    }
    tilde.x = cbind(x / sqrt(lambda), tilde.x)
  }
  return(tilde.x)
}

clusterSVM = function(x, y, cluster.label = NULL, lambda = 1, sparse = TRUE, 
                      type = 1, cost = 1, epsilon = NULL, svr_eps = NULL, 
                      bias = TRUE, wi = NULL, verbose = TRUE, seed = 1024,
                      cluster.FUN = stats::kmeans, ...) {
  
  if (!is.null(seed))
    set.seed(seed)
  if (is.null(cluster.label)) {
    cluster.result = cluster.FUN(x, ...)
    cluster.label = cluster.result$cluster
    cluster.centers = cluster.result$centers
    k = max(cluster.label)
  } else {
    k = max(cluster.label)
    cluster.centers = rep(0,length(cluster.label))
    for (i in 1:k) {
      index = which(cluster.label == i)
      cluster.centers[i] = colMeans(x[index,])
    }
  }
  
  if (lambda <= 0)
    stop("Invalid lambda. It must be greater than 0.")
  
  tilde.x = csvmTransform(x, lambda, cluster.label, sparse = sparse)
  
  svm.result = LiblineaR(data = tilde.x, target = y, type = type, cost = cost, 
                         epsilon = epsilon, svr_eps = svr_eps, bias = bias,
                         wi = wi, cross = 0, verbose = verbose)
  
  cluster.svm.result = list(svm = svm.result, 
                            lambda = lambda,
                            sparse = sparse,
                            label = cluster.label, 
                            centers = cluster.centers)
  return(cluster.svm.result)
}

predict.clusterSVM = function(object, newdata, ...) {
  eucliDist= function(x, centers) {
    apply(centers, 1, function(C) colSums( (t(x)-C)^2 ))
  }
  if (missing(newdata))
    return(fitted(object$svm))
  
  # Assign label
  dist = eucliDist(newdata, object$centers)
  new.label = max.col(-dist)
  
  # Transformation
  tilde.newdata = csvmTransform(newdata, object$lambda, new.label, object$sparse)
  
  # Make prediction
  preds = predict(object$svm, tilde.newdata, ...)
  return(preds)
}
