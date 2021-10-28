setwd("D:/et4_e")

ewcs=read.table("ewcs2016.csv",sep=",",header=TRUE)
ewcs[,][ewcs[, ,] == -999] <- NA
kk=complete.cases(ewcs)
ewcs=ewcs[kk,]

sum(is.na(ewcs))

# Basic Details of data---------------------------------------------------------
names(ewcs)
apply(ewcs,2,mean)
str(ewcs)
table(ewcs$Q87a)
table(ewcs$Q87b)
table(ewcs$Q87c)
table(ewcs$Q87d)
table(ewcs$Q87e)

table(ewcs$Q90a)
table(ewcs$Q90b)
table(ewcs$Q90c)
table(ewcs$Q90f)

summary(ewcs)

 # PCA ---------------------------------------------------------------
pr.ewcs <- prcomp(ewcs,center= TRUE, scale. = TRUE)
pr.ewcs$rotation
dim(pr.ewcs$x)

summary(pr.ewcs)
## First two principal components captures 52% of variance.


biplot(pr.ewcs, main = "Biplot",scale=0)
pr.var=pr.ewcs$sdev^2
pve.ewcs=pr.var/sum(pr.var)

plot(pve.ewcs , xlab="Principal Component", ylab="Proportion of
Variance Explained", ylim=c(0,1),type='b',
     main = "Scree Plot")
plot(cumsum(pve.ewcs), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1),
     type='b',main = "Cumulative Scree Plot")


library(ggfortify)
autoplot(pr.ewcs)
autoplot(pr.ewcs, loadings = TRUE, loadings.label = TRUE,
         data = ewcs, colour = 'Q87a')
autoplot(pr.ewcs, loadings = TRUE, loadings.label = TRUE,
         data = ewcs, colour = 'Q90a')

# Finding the top n principal component covering >80 % total variance
which(cumsum(pve.ewcs) >= 0.8)[1]



# K-means Clustering-----------------------------------------------------------------
ewcs.scaled <- scale(ewcs)

library(factoextra)
library(NbClust)

fviz_nbclust(ewcs.scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) + 
  labs(subtitle = "Elbow method") #Elbow
fviz_nbclust(ewcs.scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method") #Silhouette 
fviz_cluster(kmeans(ewcs.scaled, centers = 2), geom = "point", data = ewcs.scaled)
## The optimal number of clusters would be 2.


# Further info on cluster using PCA --------------------------------------------
pca1 <- prcomp(ewcs.scaled)

ewcs$cluster<-as.factor(kmeans(ewcs.scaled, centers = 2)$cluster)
## added cluster to initial ewcs data
ewcs$PC1<-pca1$x[,1]
ewcs$PC2<-pca1$x[,2]
## PC1 and PC2 added into initial ewcs data

ggplot(aes(x=PC1, y=PC2, col=cluster), data=ewcs)+geom_point()+facet_grid(.~Q2a)+ggtitle("Q2a")
ggplot(aes(x=PC1, y=PC2, col=cluster), data=ewcs)+geom_point()+facet_grid(.~Q87a)+ggtitle("Q87a")
ggplot(aes(x=PC1, y=PC2, col=cluster), data=ewcs)+geom_point()+facet_grid(.~Q90a)+ggtitle("Q90a")
ggplot(aes(x=PC1, y=PC2, col=cluster), data=ewcs)+geom_point()+facet_grid(.~Q90f)+ggtitle("Q90f")
##Similar cluster pattern for Q2a (gender)
##For Q87, majority of people who voted '6' were in Cluster 1, majority who voted '1' were in cluster 2.
##For Q90, majority of people who voted '5' were in cluster 1, majority who voted '1' were in cluster2.


set.seed(123)
k2 <- kmeans(ewcs.scaled, centers=2)  # set k = 2 to see natural clusters of 2.

#Checking Q2b
k1results <- data.frame(ewcs$Q2a, ewcs$Q2b, k2$cluster)
cluster1_2b <- subset(k1results, k2$cluster==1)
cluster2_2b <- subset(k1results, k2$cluster==2)
summary(cluster1_2b$ewcs.Q2b)
summary(cluster2_2b$ewcs.Q2b)

# Checking Q87a
k2results <- data.frame(ewcs$Q2a, ewcs$Q87a, k2$cluster)
cluster1_Q87a <- subset(k2results, k2$cluster==1)
cluster2_Q87a <- subset(k2results, k2$cluster==2)

summary(cluster1_Q87a$ewcs.Q87a)
summary(cluster2_Q87a$ewcs.Q87a)
## Cluster 2 has higher mean value than cluster 1 for Q87a.

# Checking Q90a
k3results <- data.frame(ewcs$Q2a, ewcs$Q90a, k2$cluster)
cluster1_Q90a <- subset(k3results, k2$cluster==1)
cluster2_Q90a <- subset(k3results, k2$cluster==2)
summary(cluster1_Q90a$ewcs.Q90a)
summary(cluster2_Q90a$ewcs.Q90a)

cluster1_2b$ewcs.Q2a <- factor(cluster1_2b$ewcs.Q2a)
cluster2_2b$ewcs.Q2a <- factor(cluster2_2b$ewcs.Q2a)
round(prop.table(table(cluster1_2b$ewcs.Q2a)),2)
round(prop.table(table(cluster2_2b$ewcs.Q2a)),2)
## 48% in Cluster 1 are Males, 53% in Cluster 2 are Males.

# Goodness of Fit Test----------------------------------------------------------
# Is Cluster 1 statistically same as Cluster 2 in terms of Q87?
M <- as.matrix(table(cluster1_Q87a$ewcs.Q87a))
p.null <- as.vector(prop.table(table(cluster2_Q87a$ewcs.Q87a)))
chisq.test(M, p=p.null)
## Cluster 1 Q87a Proportions are different from Cluster 2 Q87a Proportions
## K-means clustering concludes Q87a is a significant differentiator.

# Is Cluster 1 statistically same as Cluster 2 in terms of Q90?
Z <- as.matrix(table(cluster1_Q90a$ewcs.Q90a))
p.null1 <- as.vector(prop.table(table(cluster2_Q90a$ewcs.Q90a)))
chisq.test(Z, p=p.null1)
## Cluster 1 Q90a Proportions are different from Cluster 2 Q90a Proportions
## K-means clustering concludes Q90a is a significant differentiator.

