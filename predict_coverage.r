A=read.csv('train.csv')
A$Cover_Type = factor(A$Cover_Type)
library('caret')
library('randomForest')
inTraining = createDataPartition(p=.75,y=A$Cover_Type,list=F)
training = A[inTraining, ]
testing = A[-inTraining, ]
pred = subset(training,select=-c(Id))
coverage.model.rf = randomForest(pred$Cover_Type ~ ., data=pred)
testing$pred.coverage.rf = predict(coverage.model.rf, subset(testing, select=-c(Cover_Type,Id)))
out = testing$Cover_Type == testing$pred.coverage.rf
cat('Random forest model:','\n',sum(out)/length(out))
#57.06 percent accuracy
testing$pred.coverage.rf = NULL
coverage.model.ctree = ctree(pred$Cover_Type ~ ., data=pred)
testing$pred.coverage.ctree = predict(coverage.model.ctree, subset(testing, select=-c(Cover_Type,Id)))
out = testing$Cover_Type == testing$pred.coverage.ctree
cat('Conditional tree model:','\n',sum(out)/length(out))
