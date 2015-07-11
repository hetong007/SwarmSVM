require(SwarmSVM)

# context("gaterSVM")

data(svmguide1)
svmguide1.t = svmguide1[[2]]
svmguide1 = svmguide1[[1]]

# test_that("Performance",{
#   gatersvm.model = gaterSVM(x = svmguide1[,-1], y = svmguide1[,1], m = 10, max.iter = 5,
#                             learningrate = 0.001, threshold = 0.05, seed = 0, verbose = TRUE,
#                             valid.x = svmguide1.t[,-1], valid.y = svmguide1.t[,1])
# })
