
# install.packages(c("recommenderlab", "tibble"))
library(recommenderlab)
library(dplyr) 
library(tidyr)
library(tibble)

View(user_item)  # row:items, col:users
class(user_item) # matrix

rating <- as(user_item, "realRatingMatrix")
identical(as(rating, "matrix"), user_item)
rating_lst <- as(rating, "list")
rating_lst

r_m <- normalize(rating)
r_d <- denormalize(r_m)
image(rating, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")

r_b <- binarize(rating, minRating=4)  # binary case
binary <- as(r_b, "matrix")
View(binary)


# recommender class start
rating <- as(user_item, "realRatingMatrix")
class(rating)

nrow(rating)
r <- Recommender(rating[1:45], method = "UBCF") # POPULAR, UBCF
r
getModel(r)$description

recom <- predict(r, rating[46], n=15, type="topNList") 
recom
as(recom, "list")
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")

# user_item_t
user_item_t <- t(user_item)

rating <- as(user_item_t, "realRatingMatrix")
class(rating)

nrow(rating)
r <- Recommender(rating[1:117], method = "UBCF") # POPULAR, UBCF
r
getModel(r)$description

recom <- predict(r, rating[118], n=15, type="topNList") 

origin <- user_item_t
origin <- origin[which(rowSums(!is.na(origin)) > 5),]
rating <- as(origin, "realRatingMatrix")
nrow(rating)
r <- Recommender(rating[1:13], method = "UBCF") # POPULAR, UBCF
r
recom <- predict(r, rating[14], n=15, type="topNList") 
as(recom, "list")
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")

### 직접!
df <- read.csv("hungry_eun_tae.csv")
View(df)

df_t <- t(df)
View(df_t)

df_t <- df_t[-1,]
colnames(df_t) <- paste("i", 1:ncol(df_t), sep='')

df_t <- as.data.frame(df_t)
str(df_t)

df_t[,c(1:ncol(df_t))] <- as.double(unlist(df_t[,c(1:ncol(df_t))]))
df_t[which(df_t == 0, arr.ind = TRUE)] <- NA
str(df_t)


user_item_ratings <- as.matrix(df_t)
rating <- as(user_item_ratings, "realRatingMatrix")

# recommenderRegistry$get_entries(dataType = "realRatingMatrix")
nrow(rating)
class(rating)

# Recommender(data, method)
r <- Recommender(rating[1:9], method = "UBCF")
r
names(getModel(r))
getModel(r)$description
recom <- predict(r, rating[10], n=5, type="topNList")
recom
t <- as(recom, "list")
recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")
