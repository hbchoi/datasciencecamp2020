### Kmeans
# user_item (kmeans)
user_item_t <- t(user_item)
View(user_item_t)
user_item_t[is.na(user_item_t)] <- 0
clusterKmeans <- kmeans(user_item_t, centers = 5, iter.max = 100)
clusterKmeans$cluster
table(clusterKmeans$cluster)
clusterKmeans$center

# hungry_eun_tae (kmeans)  
# df <- read.csv("hungry_eun_tae.csv")
df <- fread("hungry_eun_tae.csv", sep=',', encoding='UTF-8')


### Hierarchical clustering
d <- dist(df_t, method="euclidean") 
d
pfit <- hclust(d, method="ward.D") 
View(df_t)
df_t$user <- c(rownames(df_t))
plot(pfit, labels=df_t$user)


### DBSCAN
# install.packages("fpc")
# install.packages("dbscan")
library(fpc) 
library(dbscan)
library(ggplot2)

n <- 100 
x <- cbind(
  x = runif(5, 0, 5) + rnorm(n, sd = 0.2),
  y = runif(5, 0, 5) + rnorm(n, sd = 0.2)
)
x

res <- dbscan(x, eps = .3, minPts = 3)
res

plot(x, col=res$cluster)
hullplot(x, res) # data, clustering


