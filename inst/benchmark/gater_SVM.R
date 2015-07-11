require(SwarmSVM)

### Toy Example
local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/toydata.RData",local.file.name)
load(local.file.name)
toydata.t = toydata[[2]]
toydata = toydata[[1]]

### Covtype
local.file.name = tempfile()
download.file("http://www.sfu.ca/~hetongh/data/covtype.mult.RData",local.file.name)
load(local.file.name)
train.ind = 1:100000
valid.ind = 400001:410000
test.ind = 450001:500000
covtype.train = covtype[train.ind,]
covtype.valid = covtype[valid.ind,]
covtype.test = covtype[test.ind,]

###############
### GaterSVM
###############

gaterSVM.model = gaterSVM(x = toydata[,-3], y = toydata[,3], hidden = 10, seed = 0,
                          m = 5, max.iter = 1, learningrate = 0.001, threshold = 0.01)
preds = predict(gaterSVM.model, toydata.t[,-3])
table(preds,toydata.t[,3])


gaterSVM.model = gaterSVM(x = as.matrix(covtype.train[,-1]), y = covtype.train[,1], 
                          hidden = 100, 
                          m = 10, max.iter = 3, learningrate = 0.01, threshold = 1e-5)
preds = predict(gaterSVM.model, as.matrix(covtype.train[,-1]))
table(preds,covtype.train[,1])
preds = predict(gaterSVM.model, as.matrix(covtype.test[,-1]))
table(preds,covtype.test[,1])
