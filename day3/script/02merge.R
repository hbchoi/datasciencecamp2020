A <- list_df_data[[1]]
colnames(A) <- c("title", 1)
merge_df <- A
View(merge_df)

i = 2
repeat{
  if(i>src_file_cnt-1) break
  B <- list_df_data[[i]]
  colnames(B) <- c("title", i)
  merge_df <- merge(x=merge_df, y=B, by='title', all=TRUE)
  i <- i+1
}

restaurant_lst <- as.vector(merge_df[,1])
restaurant_lst

user_item <- merge_df[,-1]
colnames(user_item) <- paste("u", 1:ncol(user_item), sep='')
View(user_item)
user_item <- t(user_item)
colnames(user_item) <- paste("i", 1:ncol(user_item), sep='')
user_item <- t(user_item)
user_item <- user_item[which(rowSums(!is.na(user_item)) > 3),]
View(user_item)