library(MASS)
str(cats)


plot(cats$Hwt ~ cats$Bwt,
     col='forestgreen', pch=19,
     xlab = 'Body Weight', ylab='Hear wegiht',
     main='body weight and heart weight of cats')

cor(cats$Bwt, cats$Hwt)
with(cats, cor(Bwt, Hwt))

cor.test(cats$Bwt, cats$Hwt)

cor.test(cats$Bwt, cats$Hwt, alternative='greater', conf.level = 0.99)

cor.test( ~ Bwt + Hwt, data=cats)

cor.test( ~ Bwt + Hwt, data=cats, subset=(Sex=="F"))

str(iris)
cor(iris[-5])


iris.cor <- cor(iris[-5])
class(iris.cor)
iris.cor['Petal.Width', 'Petal.Length']

library(psych)
corr.test(iris[-5])
print(corr.test(iris[-5]), short=FALSE) # including conf.level

str(state.x77)

cor(state.x77)

pairs.panels(state.x77) # plot
pairs.panels(state.x77, pch=21, bg='red', hist.col='gold',
             main='Correlation Plot of US States Data',)

install.packages('corrgram')
library(corrgram)
corrgram(state.x77)
corrgram(state.x77, lower.panel=panel.shade, 
         upper.panel=panel.pie, text.panel=panel.txt,
         order=TRUE, main='Corrgram of US States Data')

cols <- colorRampPalette(c('red', 'pink',
                           'green', 'blue'))

corrgram(state.x77, col.regions=cols,
         lower.panel=panel.pie, 
         upper.panel=panel.conf, text.panel=panel.txt,
         order=FALSE, main='Corrgram of US States Data')

# ======================================================
  
str(mtcars)
mtcars2 <- mtcars[, c('mpg', 'cyl', 'hp', 'wt')]
cor(mtcars2)  

install.packages("ggm")
library(ggm)  
pcor(c(1, 3, 2, 4), cov(mtcars2))  
pcor(c("mpg", "hp", "cyl", "wt"), cov(mtcars2))  

pcor.test(pcor(c(1, 3, 2, 4), cov(mtcars2)),
          q=2, n=nrow(mtcars2))  

install.packages("ppcor")
library(ppcor)  
pcor(mtcars2)  
  
# ======================================================

install.packages("car")
library(car)
str(Prestige)
head(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm)
Prestige.lm

plot(Prestige$income ~ Prestige$education,
     col='cornflowerblue', pch=19,
     xlab='Education', ylab='Income',
     main='Education and Income')
abline(PRestige.lm, col='salmon', lwd=2)

summary(Prestige.lm)

coef(summary(Prestige.lm))

anova(Prestige.lm)

coef(Prestige.lm)

confint(Prestige.lm)
confint(Prestige.lm, level=0.99)

fitted(Prestige.lm)[1:3]
resid(PRestige.lm)[1:3]
Prestige$income[1:3]

Prestige.new <- data.frame(education=c(5, 10, 15))
predict(Prestige.lm, newdata=Prestige.new)
predict(Prestige.lm, newdata=Prestige.new, interval="confidence")

mean(Prestige$education)
max(Prestige$education)
min(Prestige$education)

lm(income ~ education, data=Prestige,
   subset=(education > mean(Prestige$education)))

lm(income ~ education, data=Prestige,
   subset=(education <= mean(Prestige$education)))


# ======================================================

scatterplot(income ~ education, data=Prestige,
            pch=19, col='orangered', cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col='royalblue'),
            smooth=list(smoother=loessLine, spread=F, lty.smooth=1, lwd.smooth=3, col.smooth="green3"),
            xlab='Education', ylab='Income', main='Education and Income')

Prestige.ploy <- lm(income ~ education + I(education^2), data=Prestige) # I argument 

## formular symbol

summary(Prestige.ploy)
summary(Prestige.lm)


plot(Prestige$income ~ Prestige$education,
     col='darkoranger', pch=19,
     xlab='Education', ylab='Income',
     main='Education and Income')
install.packages('dplyr')
library(dplyr)
lines(data.frame(Prestige$education, fitted(Prestige.ploy), Prestige$education), col='cornflowerblue',lwd=2)


str(faithful)
scatterplot(eruptions ~ waiting, data=faithful,
            pch=19, col='deepskyblue', cex=1.2,
            regLine=list(metohd=lm, lty=2, lwd=3, col='blueviolet'),
            smooth=list(smoother=loessLine, spread=F, lty.smooth=1, lwd.smooth=3, col.smooth='coral'),
            xlab='Waiting', ylab='Eruptions', main='Wating and Eruptions')

faithful.poly <- lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), data=faithful)
summary(faithful.poly)
faithful.lm <- lm(eruptions ~ waiting, data=faithful)
summary(faithful.lm)

# ======================================================
str(mtcars)
mtcars <- mtcars[c('mpg', 'hp', 'wt', 'disp', 'drat')]
head(mtcars)

summary(mtcars)
cor(mtcars)

library(car)
windows(width=15, height=8)
scatterplotMatrix(mtcars, pch=19, col='royalblue', cex=1.2,
                  regLine=list(method=lm, lty=1, lwd=3, col='salmon'),
                  smooth=list(smoother=loessLine, spread=FALSE, 
                              lty.smooth=1, lwd.smooth=3, col.smooth='forestgreen'),
                  main='Car Performance')

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)

mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data=mtcars)
summary(mtcars.lm)

install.packages("QuantPsyc")
library(QuantPsyc)
mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data=mtcars)
lm.beta(mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data=mtcars))


















