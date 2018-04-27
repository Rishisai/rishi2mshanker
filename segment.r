#1a a. The variables that describe purchase behavior (including brand loyalty)
library(NbClust)
library(caret)
set.seed(1234)
X=read.csv('BathSoap.csv')
X=lapply(X,as.numeric)
X=as.data.frame(X)
library(caret)
Preprocessed<-preProcess(X,method = "medianImpute")
X<-predict(Preprocessed,X)
C=X[,c(2:21)]
nc <- NbClust(data =C ,distance = "euclidean", min.nc=2,max.nc=5,method="kmeans",index = "all", alphaBeale = 0.1) # Best number of clusters  is 2

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of criteria",
        main = "Number of clusters chosen by 8 Criteria")


set.seed(1234)
fit.km1 <- kmeans(C,3,nstart=25)
fit.km1
barplot(table(fit.km1$cluster),
        xlab="Number of Clusters", ylab="Cluster Size",
        main = "Number of clusters chosen by 8 Criteria")

fit.km1$cluster #Shows which person is in which cluster


#1.b : The variables that describe the basis for purchase
C=X[,c(10:30)]
nc <- NbClust(data =C ,distance = "euclidean", min.nc=2,max.nc=5,method="kmeans",index = "all", alphaBeale = 0.1) # Best number of clusters  is 2

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of criteria",
        main = "Number of clusters chosen by 8 Criteria")


set.seed(1234)
fit.km1 <- kmeans(C,3,nstart=25)
fit.km1
barplot(table(fit.km1$cluster),
        xlab="Number of Clusters", ylab="Cluster Size",
        main = "Number of clusters chosen by 8 Criteria")

fit.km1$cluster #Shows which person is in which cluster

#1.c  : The variables that describe both purchase behavior and basis of purchase
C=X[,c(2:46)]
nc <- NbClust(data =C ,distance = "euclidean", min.nc=2,max.nc=5,method="kmeans",index = "all", alphaBeale = 0.1) # Best number of clusters  is 2

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of criteria",
        main = "Number of clusters chosen by 8 Criteria")


set.seed(1234)
fit.km1 <- kmeans(C,3,nstart=25)
fit.km1
barplot(table(fit.km1$cluster),
        xlab="Number of Clusters", ylab="Cluster Size",
        main = "Number of clusters chosen by 8 Criteria")

fit.km1$cluster #Shows which person is in which cluster

#2
# Age and affluence index are positively correlated which states higher the age , higher the affluence index
# Age and total volume are also positively correlated
# Affluence index also positively correlates to the value that they bring in to the company

mean(dist((fit.km1$centers)))
#Q3:
library(FNN)
X2=C[1:35,1:45]
(pred.knn <- get.knnx(fit.km1$center, X2, 1)$nn.index[,1])
