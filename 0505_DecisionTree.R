

library(mlbench)
data("BreastCancer")
str(BreastCancer)

table(BreastCancer$Class)
mean(BreastCancer$Class=='benign')
mean(BreastCancer$Class=='malignant')
sum(!complete.cases(BreastCancer))

bc <- BreastCancer[-1]
bc <- cbind(lapply(bc[-10], function(x) as.numeric(as.character(x))), bc[10])
str(bc)
set.seed(567)
train <- sample(nrow(bc), 0.7*nrow(bc))
bc.train <- bc[train,]
bc.test <- bc[-train,]
table(bc.train$Class)
table(bc.test$Class)

install.packages('rpart')
library(rpart)
bc.dtree <- rpart(formula=Class ~., data=bc.train, method='class', parms=list(split='information'))
bc.dtree

install.packages('rpart.plot')
library(rpart.plot)

prp(bc.dtree, type=2, extra=104, fallen.leaves=T, roundint=F, main='Decision Tree from wisconsin Breast Cancer Dataset')
?prp

bc.dtree.pred <- predict(bc.dtree, newdata = bc.test, type='prob')
head(bc.dtree.pred)
bc.dtree.pred <- predict(bc.dtree, newdata = bc.test, type='class')
head(bc.dtree.pred)

table(bc.test$Class, bc.dtree.pred, dnn=c('Actual', 'Predicted'))
mean(bc.test$Class==bc.dtree.pred)

bc.dtree$cptable
printcp(bc.dtree)

table(bc.train$Class)

plotcp(bc.dtree)

bc.dtree.pruned <- rpart(Class ~ ., data=bc.train, method = 'class', cp=0.025714, parms=list(split='information'))
bc.dtree.pruned
bc.dtree.pruned <- prune(bc.dtree, cp=0.25714)
bc.dtree.pruned
bc.dtree.pruned$cptable

cols <- ifelse(bc.tree.pruned$frame$yval==1, 'green4', 'darkred')
prp(bc.dtree.pruned, type=2 , extra=104, fallen.leaves=T, roundint=F, branch.lty=3, col=cols, border.col=cols, shadow.col='gray', split.cex=1.2, split.suffix='?', split.box.col='lightgray', split.border.col='darkgray', split.round=0.5, main='Pruned Decision Tree from Wisconsin Breast Cancer Dataset')

install.packages('rattle')
library(rattle)

fancyRpartPlot(bc.dtree.pruned, sub=NULL)

bc.dtree.pred <- predict(bc.dtree.pruned, newdata=bc.test, type='class')
table(bc.test$Class, bc.dtree.pred, dnn=c('Actual', 'Predicted'))
mean(bc.test$Class==bc.dtree.pred)












