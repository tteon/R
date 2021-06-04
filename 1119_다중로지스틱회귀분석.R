install.packages('EffectStars')
library(EffectStars)
data(PID)
str(PID)
levels(PID$PID)

library(VGAM)
pid.mlogit <- vglm(PID ~ ., family=multinomial(), data=PID)

summary(pid.mlogit)
exp(coef(pid.mlogit))

pid.mlogit.pred <- fitted(pid.mlogit)
head(pid.mlogit.pred)


testdata <- data.frame(Education=c('low', 'high'),
                       TVnews=mean(PID$TVnews),
                       Income=mean(PID$Income),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population))
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type="response")

cbind(testdata, pid.mlogit.pred)

testdata <- data.frame(Education=rep('low', 5),
                       TVnews=mean(PID$TVnews),
                       Income=seq(20, 100, 20),
                       Age=mean(PID$Age),
                       Population=mean(PID$Population))
testdata
pid.mlogit.pred <- predict(pid.mlogit, newdata=testdata, type="response")
cbind(testdata, pid.mlogit.pred)

##

library(MASS)
str(fgl)
levels(fgl$type)
head(fgl)

fgl.scaled <- cbind(scale(fgl[,1:9]), fgl[10])
head(fgl.scaled)

set.seed(123)
train <- sample(nrow(fgl), 0.7*nrow(fgl))
fgl.train <- fgl.scaled[train,]
fgl.test <- fgl.scaled[-train,]
table(fgl.train$type)
sum(table(fgl.train$type))
table(fgl.test$type)
sum(table(fgl.test$type))

library(nnet)
fgl.mlogit <- multinom(type ~ ., data=fgl.train)
summary(fgl.mlogit)

z <- summary(fgl.mlogit)$coefficients/summary(fgl.mlogit)$standard.errors
p <- (1-pnorm(abs(z), 0, 1)) * 2
print(p, digits=3)

fgl.mlogit.pred <- predict(fgl.mlogit,
                           newdata=fgl.test,
                           type='probs')
head(fgl.mlogit.pred)
cbind(round(fgl.mlogit.pred, 3), fgl.test['type'])

max.col(fgl.mlgoit.pred)
fgl.mlgoit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
head(fgl.mlgoit.pred)

table(fgl.test$type, fgl.mlgoit.pred, dnn=c('Actual', 'Predicted'))
table(fgl.test$type, factor(fgl.mlgoit.pred,
                            levels=levels(fgl.test$type),
                            labels=levels(fgl.test$type)),
                    dnn=c('Actual', 'Predicted'))

mean(fgl.test$type==fgl.mlgoit.pred)

fgl.mlgoit.cv <- numeric()
for (i in 1:100) {
  train <- sample(nrow(fgl), 0.7*nrow(fgl))
  fgl.train <- fgl.scaled[train,]
  fgl.test <- fgl.scaled[-train,]
  fgl.mlogit.pred <- predict(fgl.mlogit,
                             newdata=fgl.test,
                             type='probs')
  fgl.mlgoit.pred <- colnames(fgl.mlogit.pred)[max.col(fgl.mlogit.pred)]
  fgl.mlgoit.cv[i] <- mean(fgl.test$type==fgl.mlgoit.pred)
}
summary(fgl.mlgoit.cv)
boxplot(fgl.mlgoit.cv, horizontal=T,
        col='tomato', xlab='Accuracy',
        main='Accuracy for Forensic Glass (100 samples)')

# possion 
install.packages('robust')
library(robust)
data('breslow.dat')
str(breslow.dat)

seizure <- breslow.dat[c('Base', 'Age', 'Trt', 'sumY')]

summary(seizure)
hist(seizure$sumY, breaks=20, col='cornflowerblue',
     xlab='Seizure count',
     main='Distribution of Seizures')

seizure.possion <- glm(sumY ~ Base + Age + Trt, 
                       data=seizure, family=poisson())
summary(seizure.possion)

pchisq(q=2122.73-559.44, df=58-55,
       lower.tail=F)

exp(coef(seizure.possion))

deviance(seizure.possion)/df.residual(seizure.possion)

install.packages('qcc')
library(qcc)
qcc.overdispersion.test(seizure$sumY, type="poisson")

seizure.qpoisson <- glm(sumY ~ Base + Age + Trt, data = seizure,family = quasipoisson())
summary(seizure.qpoisson)

library(MASS)
str(ships)

shipsinc <- subset(ships, service > 0)
shipsinc$year <- factor(shipsinc$year)
shipsinc$period <- factor(shipsinc$period)
levels(shipsinc$year)
levels(shipsinc$period)

shipsinc.poisson <- glm(incidents ~ type + year + period,
                        data=shipsinc,
                        family=poisson(),
                        offset=log(service))
summary(shipsinc.poisson)

deviance(shipsinc.poisson)/df.residual(shipsinc.poisson)
qcc.overdispersion.test(shipsinc$incidents, type="poisson")

shipsinc.qpoisson <- update(shipsinc.poisson, family=quasipoisson())
summary(shipsinc.qpoisson)
exp(coef(shipsinc.qpoisson))
