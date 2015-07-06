require(SwarmSVM)

context("alphasvm")

data(svmguide1)
svmguide1.t = svmguide1[[2]]
svmguide1 = svmguide1[[1]]

data(iris)

test_that("Error Trigger",{
  expect_error({ model = alphasvm(x = iris[,-5], y = iris[,5], alpha = 0) })
  expect_error({ model = alphasvm(x = iris[,-5], y = iris[,5], alpha = rep(0,nrow(iris))) })
  expect_error({ model = alphasvm(x = svmguide1[,-1], y = svmguide1[1:100,1])})
})

test_that("Performance",{
  set.seed(1024)
  model = alphasvm(x = svmguide1[,-1], y = svmguide1[,1], cost = 32)
  preds = predict(model, svmguide1.t[,-1])
  score = sum(diag(table(preds,svmguide1.t[,1])))/nrow(svmguide1.t)
  expect_true(score>0.8)
  
  new.alpha = matrix(0, nrow(svmguide1),1)
  new.alpha[model$index,] = model$coefs
  model2 = alphasvm(x = svmguide1[,-1], y = svmguide1[,1], alpha = new.alpha, cost = 32)
  preds = predict(model2, svmguide1.t[,-1])
  score = sum(diag(table(preds,svmguide1.t[,1])))/nrow(svmguide1.t)
  expect_true(score>0.8)
  # compare the alpha changes:
  # ind = intersect(model$index,model2$index)
  # plot(model$coefs[match(ind,model$index),],model2$coefs[match(ind,model2$index),],pch=20)
})


