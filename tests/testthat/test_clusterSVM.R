require(LiblineaR)
require(SwarmSVM)

context("Error Trigger for clusterSVM")

data(svmguide1)
svmguide1.t = svmguide1[[2]]
svmguide1 = svmguide1[[1]]

test_that("Error Trigger",{
  expect_error({csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 0,
                                     centers = 8, iter.max = 1000, seed = 512, verbose = 0,
                                     valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])})
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                        centers = 8, iter.max = 1000, seed = 512, verbose = 0,
                        valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  expect_error({pred = predict(csvm.obj$sparse, svmguide1.t[,-1])})
})

