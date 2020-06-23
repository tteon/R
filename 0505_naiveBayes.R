

install.packages('mlbench')
library(mlbench)
data("HouseVotes84")
votes <- HouseVotes84
str(votes)

library(ggplot2)
vote1 <- na.omit(votes[,c(1, 2)])
vote1$v1 <- factor(vote1$V1,
                   levels=c('n', 'y'),
                   labels=c('No', 'Yes'))
ggplot(vote1, aes(x=V1, fill=Class)) +
  geom_bar(position='dodge', width=0.7) +
  labs(title='Pros and Cons for Vote 1',
       x='Vote 1', y='Number of Congressmen',
       fill='Party')

head(votes)
sum(is.na(votes))

naCount <- function(col, cls) {return(sum(is.na(votes[,col]) & votes$Class==cls))}

naCount(2, 'democrat')
naCount(2, 'republican')


yesProb <- function(col, cls) { sum.y <- sum(votes[,col]=='y' & votes$Class==cls, na.rm=T)
sum.n <- sum(votes[,col]=='n' & votes$Class==cls, na.rm=T)
return(sum.y/(sum.y+sum.n))
}

yesProb(2, 'democrat')
yesProb(2, 'republican')

set.seed(123)
for (i in 2:ncol(votes)) {
  if(sum(is.na(votes[,i])) > 0)  {
    d.na <- which(is.na(votes[,i]) & votes$Class == 'democrat')
    r.na <- which(is.na(votes[,i]) & votes$Class == 'republican')
    votes[d.na, i] <- ifelse(runif(naCount(i, 'democrat')) < yesProb(i, 'democrat'), 'y', 'n')
    votes[r.na, i] <- ifelse(runif(naCount(i, 'republican')) < yesProb(i, 'republican'), 'y', 'n')}
}

sum(is.na(votes))
head(votes)

set.seed(123)
train <- sample(nrow(votes), 0.7*nrow(votes))
votes.train <- votes[train,]
votes.test <- votes[-train,]
table(votes.train$Class)
table(votes.test$Class)


install.packages('e1071')
library(e1071)
votes.nb <- naiveBayes(Class ~ ., data=votes.train)

votes.nb

votes.nb.pred <- predict(votes.nb, newdata=votes.test[,-1])
head(votes.nb.pred)

table(votes.test$Class, votes.nb.pred, dnn=c('Actual', 'Predicted'))
mean(votes.nb.pred==votes.test$Class)

votes.nb.pred <- predict(votes.nb, newdata=votes.test[,-1], type='raw')
head(votes.nb.pred)


votes.nb.pred <- factor(votes.nb.pred[, 'republican'] > 0.5,
                        levels=c(FALSE, TRUE),
                        labels=c('democrat', 'republican'))
head(votes.nb.pred)
table(votes.test$Class, votes.nb.pred, dnn=c('Actual', 'Predicted'))
mean(votes.nb.pred==votes.test$Class)

nbRuns <- function(fraction, run) {
  results <- NULL
  for (i in 1:run) {
    train <- sample(nrow(votes), fraction*nrow(votes))
    votes.train <- votes[train,]
    votes.test <- votes[-train,]
    votes.nb <- naiveBayes(Class ~ ., data=votes.train)
    votes.nb.pred <- predict(votes.nb, newdata=votes.test[,-1])
    results[i] <- mean(votes.nb.pred==votes.test$Class)
  }
  return(results)
}

votes.nb.cv <- nbRuns(0.7 , 100)
votes.nb.cv  
summary(votes.nb.cv)

ggplot(data.frame(acc=votes.nb.cv), aes(x='', y=acc)) +
  geom_boxplot(fill='slategray', color='darkslategray', width=0.5) +
  geom_point(position='jitter', color='royalblue', alpha=0.7) +
  labs(title='Accuracy for Party Prediction with 100 Samples', y='Accuracy') +
  coord_flip() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank())


































