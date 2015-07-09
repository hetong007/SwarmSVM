require(SwarmSVM)

### ijcnn1

local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/ijcnn1.dcsvm.RData",local.file.name)
load(local.file.name)
ijcnn1.t = ijcnn1[[2]]
ijcnn1 = ijcnn1[[1]]

##############
#### DC SVM
##############

dcSVMBenchmark = function(...) {
  dcsvm.model = dcSVM(..., k = 10, max.levels = 1, early = 1)
  early.score = dcsvm.model$valid.score
  early.training = dcsvm.model$time$total.time - dcsvm.model$time$validation.time
  early.time = dcsvm.model$time$total.time
  cat('Early Training Finished.\n')
  dcsvm.model = dcSVM(..., k = 4, max.levels = 4, early = 0)
  exact.score = dcsvm.model$valid.score
  exact.training = dcsvm.model$time$total.time - dcsvm.model$time$validation.time
  exact.time = dcsvm.model$time$total.time
  cat('Exact Training Finished.\n')
  res = c(early.score, early.training, early.time, 
          exact.score, exact.training, exact.time)
  names(res) = c("Early Score", "Early Training", "Early Time", 
                 "Exact Score","Exact Training", "Exact Time")
  return(res)
}

dcSVMBenchmark(x = ijcnn1[,-1], y = ijcnn1[,1], seed = 1024,
               gamma = 2^6, cost = 2^(-10), tolerance = 1e-2, m = 5000, scale = FALSE,
               valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])

dcSVMBenchmark(x = ijcnn1[,-1], y = ijcnn1[,1], seed = 1024,
               gamma = 2^10, cost = 2^(-10), tolerance = 1e-2, m = 5000, scale = FALSE,
               valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])

dcSVMBenchmark(x = ijcnn1[,-1], y = ijcnn1[,1], seed = 1024,
               gamma = 2^10, cost = 2^(-6), tolerance = 1e-2, m = 5000, scale = FALSE,
               valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])

dcSVMBenchmark(x = ijcnn1[,-1], y = ijcnn1[,1], seed = 1024,
               gamma = 2, cost = 2, tolerance = 1e-2, m = 5000, scale = FALSE,
               valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])


dcSVMBenchmark(x = ijcnn1[,-1], y = ijcnn1[,1], seed = 1024,
               gamma = 2, cost = 32, tolerance = 1e-2, m = 5000, scale = FALSE,
               valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])
# Early Score Early Training     Early Time    Exact Score Exact Training     Exact Time 
# 0.9840569      6.6290000     11.5400000      0.9841005     68.2430000     89.2960000 

dcSVMBenchmark(x = ijcnn1[,-1], y = ijcnn1[,1], seed = 1024,
               gamma = 2^6, cost = 2^10, tolerance = 1e-2, m = 5000, scale = FALSE,
               valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])
