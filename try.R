### This script is to build model for NBA

library(caret)
library(plyr)
library(xgboost)
library(randomForest)
library(gbm)

nearZeroVar(finalDB, saveMetrics = T)

nba <- finalDB[,-c(52,53,54,59,60)]
nba <- nba[,-27]
nba <- nba[,-c(6,11:19,24:26,43,44)]
nba$ranking = cut(nba$ranking, breaks = c(0,15,30,40), labels = c('first','second','third'))
nba = nba[,-3]
nba = nba[,-2]

nba[2:28,] = sapply(nba[2:28,], as.numeric)

##################################

inSample = sample(1:nrow(nba), 0.75*nrow(nba))
data.train = nba[inSample,]
data.test = nba[-inSample,]

set.seed(1)
time.now = proc.time()
model.gbm <- gbm(yvariable ~., data = data.train,
                 n.trees = 500,
                 bag.fraction = 0.95,
                 cv.folds = 10,
                 train.fraction = 0.5)
proc.time() - time.now


pred = predict(model.gbm, data.test, type = 'response')
result = as.data.frame(pred)
predList = c()
for (i in 1:nrow(data.test)) {
      current = result[i,]
      predList[i] = which.max(current)
}
predList[predList == 1] = 'first'
predList[predList == 2] = 'second'
predList[predList == 3] = 'third'

confusionMatrix(data = predList, reference = data.test$yvariable)

