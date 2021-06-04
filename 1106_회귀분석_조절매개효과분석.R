str(mtcars)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
plot(mtcars.lm)

library(car)
vif(mtcars.lm)

mean(mtcars$mpg)
powerTransform(mtcars$mpg)
summary(powerTransform(mtcars$mpg))

# linerity
boxTidwell(mpg ~ hp + wt, data=mtcars)

# 
spreadLevelPlot(lm(mpg ~ hp + wt, data=mtcars))

# 
str(mtcars)

# nested model
mtcars.lm1 <- lm(mpg ~ hp + wt, data=mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
#
anova(mtcars.lm1, mtcars.lm2)
# more flexible than above method(anova)
AIC(mtcars.lm1, mtcars.lm2)

?step
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
step(mtcars.lm, direction='backward')

install.packages('leaps')
library(leaps)
mtcars.regsubsets <- regsubsets(x=mpg ~ hp + wt + disp + drat, data=mtcars,
                                nbest=4)
?plot.regsubsets
library(RColorBrewer)
?RColorBrewer

plot(mtcars.regsubsets)
plot(mtcars.regsubsets, scale='adjr2', col=brewer.pal(9, 'Pastel1'), main='All Subsets Regression')

# substracting process
str(summary(mtcars.regsubsets))
summary(mtcars.regsubsets)$adjr2
which.max(summary(mtcars.regsubsets)$adjr2)
coef(mtcars.regsubsets, 9)

## dummy part

str(InsectSprays)
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean)

sprays.lm <- lm(count ~ spray, data=InsectSprays)
summary(sprays.lm)

contrasts(InsectSprays$spray)

sprays.aov <- aov(count ~ spray, data=InsectSprays)
summary(sprays.aov)
TukeyHSD(sprays.aov)
# 기준범주 변경 argument -> ref
respray <- relevel(InsectSprays$spray, ref=6)
contrasts(respray)

sprays.lm <- lm(count ~ respray, data=InsectSprays)
summary(sprays.lm)

## 매개효과분석

str(mtcars)

model.total <- lm(mpg ~ disp, data=mtcars)
summary(model.total)

model.M <- lm(wt ~ disp, data=mtcars)
summary(model.M)

model.Y <- lm(mpg ~ disp + wt, data=mtcars)
summary(model.Y)

0.007*(-3.351)

install.packages('multilevel')
library(multilevel)
# pred , med , out 
model.sob <- sobel(pred=mtcars$disp, med=mtcars$wt, out=mtcars$mpg )
model.sob

pnorm(abs(model.sob$z.value), lower.tail=FALSE)
pnorm(abs(model.sob$z.value), lower.tail=FALSE) * 2

install.packages('bda')
library(bda)
# mv -> 매개,  iv -> 독립 , dv -> 종속
mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

install.packages('mediation')
library(mediation)

model.M <- lm(wt ~ disp, data=mtcars)
model.Y <- lm(mpg ~ disp + wt, data=mtcars)
set.seed(123)
model.mediation <- mediate(model.m=model.M,
                           model.y=model.Y,
                           treat='disp',
                           mediator='wt',
                           boot=T,
                           sims=500)
summary(model.mediation)

plot(model.mediation, cex=1.2, col='royalblue', lwd=2,
     main='Mediation Effect Analysis')

## 조절효과분석

str(mtcars)
# hp:wt -> interaction between hp and wt
mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars.lm)

install.packages('effects')
library(effects)
m <- round(mean(mtcars$wt), 1)
s <- round(sd(mtcars$wt), 1)
m;s

plot(effect(term='hp:wt', 
            mod=mtcars.lm, 
            xlevels=list(wt=c(m-s, m, m+s))),
            lines=list(multiline=T, 
            lwd=2, lty=c(3, 2, 1), col=c('royalblue', 'violet', 'maroon')),
            main='Interaction Plot for Horsepower and Weight')

install.packages('rockchalk')
library(rockchalk)
plotSlopes(model=mtcars.lm, plotx='hp', modx='wt',
           modxVals='std.dev', col=rainbow(3),
           main='Interaction Plot for Horsepower and Weight')

## 조절매개효과분석

model.M <- lm(wt ~ disp * am, data=mtcars)
model.M <- lm(wt ~ disp + am + disp:am, data=mtcars)#
model.Y <- lm(mpg ~ disp * am + wt * am, data=mtcars)
model.Y <- lm(mpg ~ disp + am + wt + disp:am + wt:am , data=mtcars)#

library(mediation)
set.seed(12)
model.med1 <- mediate(model.m=model.M,
                      model.y=model.Y,
                      covariates=list(am=0),
                      treat='disp',
                      mediator='wt',
                      boot=T, sims=500)
summary(model.med1)

set.seed(12)
model.med2 <- mediate(model.m=model.M,
                      model.y=model.Y,
                      covariates=list(am=1),
                      treat='disp',
                      mediator='wt',
                      boot=T, sims=500)

summary(model.med2)

set.seed(12)
model.med <- mediate(model.m=model.M,
                     model.y=model.Y,
                     treat='disp',
                     mediator='wt',
                     boot=T,
                     sims=500)
set.seed(12)
test.modmed(object=model.med,
            covariates.1 = list(am=0) , covariates.2 = list(am=1),
            sims=500)
