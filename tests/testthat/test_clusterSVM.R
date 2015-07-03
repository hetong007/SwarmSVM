require(LiblineaR)
require(SwarmSVM)

context("clusterSVM")

data(svmguide1)
svmguide1.t = svmguide1[[2]]
svmguide1 = svmguide1[[1]]

data(iris)

test_that("Error Trigger",{
  # Wrong parameters
  expect_error({csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 0,
                                     centers = 8, seed = 512, verbose = 0,
                                     valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
  expect_error({csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                                      centers = 8, seed = 512, verbose = 0, type = 11,
                                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
  expect_error({csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                                      centers = 8, seed = 512, verbose = 0, cost = -1,
                                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
  expect_error({csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                                      centers = 8, seed = 512, verbose = 0, epsilon = -1,
                                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
  expect_error({csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                                      centers = 8, seed = 512, verbose = 0, verbose = 10,
                                      valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
  
  # Wrong prediction object
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                        centers = 8, seed = 512, verbose = 0,
                        valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  expect_error({pred = predict(csvm.obj$sparse, svmguide1.t[,-1])})
  
  # Only one cluster
  expect_warning({
    csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                          centers = 1, seed = 512, verbose = 0,
                          valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
})

test_that("Performance",{
  liblinear.obj = LiblineaR::LiblineaR(data = svmguide1[,-1], target = svmguide1[,1], 
                                       type = 1, verbose = F)
  liblinear.pred = predict(liblinear.obj, svmguide1.t[,-1])$prediction
  liblinear.score = sum(liblinear.pred==svmguide1.t[,1])/length(liblinear.pred)
  
  # with 8 clustering
  csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                        centers = 8, seed = 512, verbose = 0,
                        valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  csvm.score = csvm.obj$valid.score
  expect_true(csvm.score>liblinear.score)
  
  # with 1 clustering
  expect_warning({
    csvm.obj = clusterSVM(x = svmguide1[,-1], y = svmguide1[,1], lambda = 1,
                          centers = 1, seed = 512, verbose = 0,
                          valid.x = svmguide1.t[,-1],valid.y = svmguide1.t[,1])
  })
  csvm.score = csvm.obj$valid.score
  expect_equal(csvm.score, liblinear.score, tolerance = 0.01)
  
  # Multiclassification
  csvm.obj = clusterSVM(x = iris[,-5], y = iris[,5], sparse = FALSE,
                        centers = 2, seed = 512, verbose = 0,
                        valid.x = iris[,-5],valid.y = iris[,5])
  expect_true(csvm.obj$valid.score>0.97)
})
