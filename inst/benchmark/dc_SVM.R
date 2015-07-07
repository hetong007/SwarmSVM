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
dcsvm.model = dcSVM(x = as.matrix(ijcnn1[,-1]), y = ijcnn1[,1], k = 10, max.levels = 1, 
                    early = 1, gamma = 2, cost = 32, tolerance = 1e-2, m = 5000, scale = TRUE)

fits = predict(dcsvm.model, as.matrix(ijcnn1[,-1]))
table(fits,ijcnn1[,1])
sum(diag(table(fits,ijcnn1[,1])))/nrow(ijcnn1)
# 0.9957992
preds = predict(dcsvm.model, as.matrix(ijcnn1.t[,-1]))
table(preds,ijcnn1.t[,1])
sum(diag(table(preds,ijcnn1.t[,1])))/nrow(ijcnn1.t)
# 0.9840569
