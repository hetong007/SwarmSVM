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

set.seed(1024)
dcsvm.model = dcSVM(x = ijcnn1[,-1], y = ijcnn1[,1], k = 4, max.levels = 4, early = 1,
                    gamma = 2, cost = 32, tolerance = 1e-2, m = 5000)
fits = predict(dcsvm.model, ijcnn1[,-1])
table(fits,ijcnn1[,1])
preds = predict(dcsvm.model, ijcnn1.t[,-1])
table(preds,ijcnn1.t[,1])


