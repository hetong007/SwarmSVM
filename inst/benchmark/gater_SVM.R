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
                          m = 5, max.iter = 1, learningrate = 0.001, threshold = 0.05,
                          valid.x = toydata.t[,-3], valid.y = toydata.t[,3], verbose = TRUE)
table(gaterSVM.model$valid.pred,toydata.t[,3])


gaterSVM.model = gaterSVM(x = covtype.train[,-1], y = covtype.train[,1], 
                          hidden = 150, seed = 0, m = 50, max.iter = 3, 
                          learningrate = 0.03, threshold = 0.05, verbose = TRUE,
                          valid.x = covtype.valid[,-1], 
                          valid.y = covtype.valid[,1],stepmax = 40)
table(gaterSVM.model$valid.pred, covtype.valid[,1])
