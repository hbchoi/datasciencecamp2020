pearsonCor <- function(x, y){ 
  x_mean <- mean(x, na.rm = T) 
  y_mean <- mean(y, na.rm = T) 
  idx <- !is.na(x) & !is.na(y) 
  if(sum(idx) == 0) return(NA) 
  x_new <- x[idx]
  y_new <- y[idx]
  sum((x_new- x_mean) * (y_new-y_mean)) /
    sqrt( sum( (x_new - x_mean)**2) * sum( (y_new-y_mean) **2) )
}

u_dist<-function(u, v){
  sqrt(sum((u -v) **2))
}

m <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))
View(m)
u <-m['u5',]
u

sim <- apply(m, 1, function(x) {
  pearsonCor(u, x) })
sim

library(doBy)
k=2
k_neighbors <- setdiff(which.maxn(sim, k+1), 5)
k_neighbors

k_recommend <- apply(m[k_neighbors,], 2, function(x) { mean(x, na.rm = T)})
k_recommend

k_recommend_final <- k_recommend[is.na(u)]
sort(k_recommend_final, decreasing = T)


# install.packages("proxy")  
library(proxy)
View(user_item)
user_item_t <- t(user_item)
cor(user_item_t, y = NULL, use ="everything", method = c("pearson"))   
# x, y 두column 사이만이 아니라, x에 다 넣어서 할 수도 있다. use는 columnuse = 결측치 처리 (NA포함이면 NA처리), 지금은 NA가 너무 많아서 잘 안됨.
user_similarity <- as.matrix(dist(user_item_t, method = "euclidean")) # cosine, euclidean
item_similarity <- as.matrix(dist(user_item, method = "cosine"))
head(user_similarity)
head(item_similarity)



