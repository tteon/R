install.packages('flexclust')
library(flexclust)
data(nutrient)
str(nutrient)

d <- dist(nutrient, method='euclidean')
class(d)
as.matrix(d)[1:5, 1:5]
#as.matrix(d)[1:7, 1:7]

library(MASS)
str(survey)
levels(survey$Sex)
levels(survey$Smoke)
survey.dummy <- survey[c('Sex', 'Smoke')]
head(survey.dummy, 5)

install.packages('devtools')
devtools::install_github("jacobkap/fastDummies")
library(fastDummies)
survey.dummy <- fastDummies::dummy_cols(survey.dummy, remove_selected_columns=T, remove_first_dummy=T,ignore_na=T)

head(survey.dummy)

d <- dist(survey.dummy, method='binary')
as.matrix(d)[1:5,1:5]

library(cluster)
d <- daisy(survey, metric='gower') #metric = 'gower'
as.matrix(d)[1:5,1:5]

#
library(flexclust)
data(nutrient)
str(nutrient)
head(nutrient)

nutrition <- nutrient
row.names(nutrient) <- tolower(row.names(nutrient))
head(nutrition)
nutrition.scaled <- scale(nutrition)

d <- dist(nutrition.scaled)
?hclust
clustering.average <- hclust(d, method='average')

plot(clustering.average, hang=-1, col='darkgreen', xlab='Food', main='Hierarchical Clustering with Average Linkage')

install.packages('NbClust')
library(NbClust)
nc <- NbClust(nutrition.scaled, distance='euclidean', method='average', min.nc=3, max.nc=15)

nc$Best.nc
table(nc$Best.nc[1,])

clusters <- cutree(clustering.average, k=5)
clusters
table(clusters)
plot(clustering.average, hang=-1, col='darkgreen', xlab='Food', main='Hierarchical Clustering with Average Linkage')
rect.hclust(clustering.average, k=5)

aggregate(nutrition, by=list(cluster=clusters), mean)

a <- aggregate(nutrition.scaled, by=list(cluster=clusters), mean)
n <- as.vector(table(clusters))
cbind(a, n)

# 

str(state.x77)
head(state.x77)
state.scaled <- scale(state.x77)
head(state.scaled)

library(NbClust)
set.seed(123)
nc <- NbClust(state.scaled, distance='euclidean', method='kmeans', min.nc=2, max.nc=15)

nc$Best.nc
table(nc$Best.nc[1,])

set.seed(123)
clustering.km <- kmeans(state.scaled, centers=3, nstart=25)

clustering.km$cluster
clustering.km$centers
clustering.km$size

aggregate(state.x77,
          by=list(cluster=clustering.km$cluster),
          mean)

library(cluster)
clusplot(x=state.x77, clus=clustering.km$cluster, color=T, shade=T, labels=2, lines=0, main='Cluster Plot')

#

install.packages('rattle')
library(rattle)
str(wine)

library(cluster)
set.seed(123)
clustering.pam <- pam(wine[-1], k=3, stand=T)

clustering.pam$clusinfo
clustering.pam$medoids
clustering.pam$id.med
clustering.pam$clustering

aggregate(wine[-1], by=list(cluster=clustering.pam$clustering), mean)

clusplot(clustering.pam, color=T, shade=T, labels=4, lines=0, main='Cluster Plot')


result.pam <- table(wine$Type, clustering.pam$clustering, dnn=c('Acutal', 'Clustered'))
result.pam

mean(wine$Type==clustering.pam$clustering)



