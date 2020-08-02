library(stringr)
library(dplyr)
library(data.table)

src_dir <- c('/Users/osangjin/Desktop/past2/past/Recommendation-system/data') 
src_file <- list.files(src_dir)

file_name <- paste0(src_dir, '/', src_file[1])
user_id <- as.numeric(str_extract(src_file[1], '\\d+'))
df <- fread(file_name, sep=',', encoding="UTF-8")
df_name <- paste('df',user_id, sep='')
id_lst <- c()
id_lst[1] <- user_id
View(df)
df <- df[, -1]
assign(df_name, df)


src_file_cnt <- length(src_file)
src_file_cnt

id_lst <- c()
for(i in 1:src_file_cnt) {
  file_name <- paste0(src_dir, '/', src_file[i])
  user_id <- as.numeric(str_extract(src_file[i], '\\d+'))
  df <- fread(file_name, sep=',', encoding="UTF-8")
  df_name <- paste('df',user_id, sep='')
  id_lst[i] <- user_id
  df <- df[, -1]
  assign(df_name, df)
}

list_df_data <- mget(ls(pattern = "^df\\d+"))


