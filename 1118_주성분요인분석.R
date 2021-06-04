# pca

str(state.x77)

head(state.x77)

pca <- prcomp(state.x77, scale=T)
summary(pca)

plot(pca, type='l', pch=19, lwd=2, col='red', main='scree plot')

round(pca$rotation, 3)

round(scale(state.x77) %*% pca$rotation, 3)

round(pca$x, 3)

round(pca$x[, c(1, 2)], 3)

round(cor(pca$x), 3)

biplot(pca, cex=c(0.5, 0.75), main='Biplot')

# factor analysis

install.packages('ade4')
library(ade4)
data(olympic)
str(olympic)

library(psych)
?fa.parallel
fa.parallel(olympic$tab, fm='ml', fa='fa', n.iter=100)

install.packages('nFactors')
library(nFactors)
nScree(olympic$tab)

eigen(cor(olympic$tab))

?factanal
fa <- factanal(olympic$tab, factors=2, scores='regression')
fa

fa$loadings

print(fa$loadings, cutoff=0.001)

round(fa$uniquenesses,3)
1 - round(fa$uniquenesses,3)

factor.plot(fa, labels=colnames(olympic$tab),
            pos=4, title='Factor Plot')

library(gplots)
library(RColorBrewer)
heatmap.2(abs(fa$loadings), col=brewer.pal(9, 'Blues'),
          trace='none', key=F, dendrogram='none', 
          cesCol=1.2, main='Factor Loadings')


install.packages('semPlot')
library(semPlot)
semPaths(fa, what='est', residuals=F,
         cut=0.3, posCol=c('white', 'darkgreen'),
         negCol=c('white', 'red'),
         edge.label.cex=0.75)

fa.scores <- fa$scores
fa.scores
colnames(fa.scores) <- c('Run', 'Throw')
library(RColorBrewer)
heatmap.2(abs(fa$scores), col=brewer.pal(9, 'GnBu'),
          trace='none', key=F, dendrogram='none', 
          cesCol=1.2, main='Factor Scores by Athletes')


## MDS

install.packages('eurodist')
str(eurodist)
labels(eurodist)
as.matrix(eurodist)[1:5, 1:5]

eurocity.mds <- cmdscale(d=eurodist, k=2)
eurocity.mds

plot(eurocity.mds, type='n', main='MDS plot')
text(eurocity.mds, rownames(eurocity.mds),
     col='maroon', cex=0.7)

str(USJudgeRatings)

USJudgeRatings.dis <- dist(USJudgeRatings)
USJudgeRatings.mds <- cmdscale(USJudgeRatings.dis)

plot(USJudgeRatings.mds, type='n', main='MDS plot')
text(USJudgeRatings.mds, rownames(eurocity.mds),col='blue', cex=0.5)


str(mtcars)

library(cluster)
mtcars.dis <- daisy(mtcars, metric='gower')

library(MASS)
mtcars.mds <- isoMDS(mtcars.dis)
str(mtcars.mds)

plot(mtcars.mds$points, type='n', main='MDS plot')
text(mtcars.mds$points, rownames(eurocity.mds),col='blue', cex=0.9)
