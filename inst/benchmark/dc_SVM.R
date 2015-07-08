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

dcsvm.model = dcSVM(x = ijcnn1[,-1], y = ijcnn1[,1], k = 10, max.levels = 1, seed = 1024,
                    early = 1, gamma = 2, cost = 32, tolerance = 1e-2, m = 5000, scale = FALSE,
                    valid.x = ijcnn1.t[,-1], valid.y = ijcnn1.t[,1])
preds = dcsvm.model$valid.pred
table(preds,ijcnn1.t[,1])
dcsvm.model$valid.score
dcsvm.model$time$total.time
# 0.9840569
