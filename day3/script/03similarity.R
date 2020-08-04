# 피어슨
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

# 유클리디안
u_dist<-function(u, v){
  sqrt(sum((u -v) **2))
}

# 50 -> 100 / 1:5 -> 1:10
m <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))
View(m)
u <-m['u5',]
u

# 유사도
sim <- apply(m, 1, function(x) {
  pearsonCor(u, x) })
sim


# install.packages("doBy")
library(doBy)
k=2         # 나와 유사한 몇명 뽑을지
k_neighbors <- setdiff(which.maxn(sim, k+1), 5)
k_neighbors

k_recommend <- apply(m[k_neighbors,], 2, function(x) { mean(x, na.rm = T)})
k_recommend

k_recommend_final <- k_recommend[is.na(u)]
sort(k_recommend_final, decreasing = T)


# 다른 라이브러리
# install.packages("proxy")  
library(proxy)
View(user_item)
user_item_t <- t(user_item)

# 피어슨
cor(user_item_t, y = NULL, use ="everything", method = c("pearson"))   # 원래 두 변수간 상관계수(everything->NA, obs->에러)

# 코사인, 유클리디안
user_similarity <- as.matrix(dist(user_item_t, method = "euclidean"))  # cosine, euclidean
item_similarity <- as.matrix(dist(user_item, method = "cosine"))


