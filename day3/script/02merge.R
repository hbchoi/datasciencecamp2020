
A <- list_df_data[[1]]
View(A)
colnames(A) <- c("title", 1)
merge_df <- A
View(merge_df)

i = 2
repeat{
  if(i>src_file_cnt) break
  B <- list_df_data[[i]]
  colnames(B) <- c("title", i)
  merge_df <- merge(x=merge_df, y=B, by='title', all=TRUE)
  i <- i+1
}

restaurant_lst <- as.vector(merge_df[,1])
restaurant_lst

View(merge_df)
user_item <- merge_df[,-1]
colnames(user_item) <- paste("u", 1:ncol(user_item), sep='')

# rownames(user_item) <- paste("i", 1:nrow(user_item), sep='')
user_item <- t(user_item)
colnames(user_item) <- paste("i", 1:ncol(user_item), sep='')
user_item <- t(user_item)
user_item <- user_item[which(rowSums(!is.na(user_item)) > 3),]
View(user_item)