#movie crawling 
library(rvest)
library(R6)

all.reviews <- c() 
url_base <- 'https://movie.daum.net/moviedb/grade?movieId=111292&type=netizen&page='
for(page in 1:5){
  url <- paste(url_base,page, sep = '')
  htxt <- read_html(url)
  comments <- html_nodes(htxt,'div') %>% html_nodes('p')
  reviews <- html_text(comments)
  reviews <- repair_encoding(reviews, from = 'UTF-8')
  if(length(reviews) ==0){break}
  all.reviews <- c(all.reviews, reviews)
  print(page)
  
}
library(stringr)
all.reviews <- all.reviews[!str_detect(all.reviews, "평점")]
head(all.reviews)
write.table(all.reviews, 'parasite.txt')

#crawling over 
---------------------------------------------------

#wordcloud method2
noun <-sapply(all.reviews, extractNoun, USE.NAMES = F) %>% unlist() 
noun2 <- Filter(function(x){nchar(x) >=2}, noun)
head(noun2)

#추가 전처리
data_unlist<- gsub('[~!@#$%&*()_+=?<>]','',data_unlist)
data_unlist <- gsub("[A-Za-z]", "", data_unlist)
data_unlist <- gsub("\\[","",data_unlist)
data_unlist <- gsub('[ㄱ-ㅎ]','',data_unlist)
data_unlist<- gsub('(ㅜ|ㅠ)','',data_unlist)
data_unlist <- gsub("\\d+","",data_unlist)
data_unlist <- gsub("\\▲","",data_unlist)
data_unlist <- gsub("\\.","",data_unlist)
data_unlist <- gsub("\\/","",data_unlist)
data_unlist <- gsub("\\^","",data_unlist)
data_unlist <- gsub("\\:","",data_unlist)
data_unlist <- gsub("\\-","",data_unlist)
data_unlist <- gsub("\\.","",data_unlist)

wordFreq <- table(noun2)
noundata <- sort(wordFreq, decreasing = TRUE, 300)
noundata_df <- data.frame(noundata)
print(noundata)

write.csv(noundata_df, file = 'word_number.csv', 
          row.names = FALSE, quote = FALSE)

pal2 <- brewer.pal(8, "Dark2")
pal <- brewer.pal(12, 'Set3')
pal <- pal[-c(1:2)]

wordcloud(words = noundata_df$noun2,
          freq = noundata_df$Freq, 
          random.order = F,
          min.freq = 40,
          colors = brewer.pal(8, 'Dark2'))

png("NewsWordCloud.png", width = 800, height = 800 )
wordcloud(names(noundata), freq = noundata, min.freq = 10, random.order = T, rot.per = 0, col = pal)
dev.off() 

#----------------------------------------------------------------------

#data remove & noun and adjective 
library(KoNLP)
useNIADic()
ko.words <- function(doc){
  d <-as.character(doc)
  pos <- paste(SimplePos09(d))
  extracted <- str_match(pos, '([가-힣]+)/[NP]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
  
}
rev_all <- ko.words(all.reviews)
head(rev_all)

rev <- Corpus
rev
tdm <- TermDocumentMatrix(rev, control = list(tokenize = ko.words, 
                                              removePunctuation = T, 
                                              removeNumbers = T, 
                                              wordLengths = c(2, 8), 
                                              weighting = weightBin
))
tdm.matrix <- as.matrix(tdm)
dim(tdm)
tdm.matrix

word.count <- rowSums(tdm.matrix)
word.order <- order(word.count, decreasing = T)
freq.words <- tdm.matrix[word.order,]
co.matrix <- freq.words%*% t(freq.words)
co.matrix

wordcloud(tdm)
#wordcloud method1
library(wordcloud)
library(RColorBrewer)
library()

head(rev)
wordcloud(rev, min.freq = 5)



#associate analysis 
ko_words <- function(doc) {
  d <- as.character(doc)
  pos <- unlist(SimplePos22(d))
  
  extracted <- str_match(pos, '([媛-?옡]+)/[NP][A-Z]')
  
  keyword <- extracted[, 2]
  keyword[!is.na(keyword)]
}
texts <- news_crolling %>%
  str_replace_all(pattern="\r", replacement="") %>%
  str_replace_all(pattern="\n", replacement=" ") %>%
  str_replace_all(pattern="[[:punct:]]", replacement=" ") %>%
  str_replace_all(pattern="[?꽦-?뀕?뀖-?뀭]+", replacement="") %>%
  str_replace_all(pattern="/", replacement=" ") %>%
  str_trim(side="both")

texts <- texts[texts != ""]
texts
pos <- Map(extractNoun, texts)
pos <- str_remove_all( texts,pattern = '?솗?궛')
corpus <- Corpus(VectorSource(pos))
corpus
stopWord <- c("援??궡", "?씠?뒋")
tdm <- TermDocumentMatrix(corpus, control=list(
  removePunctuation=TRUE, stopwords=stopWord,
  removeNumbers=TRUE, wordLengths=c(4, 10), weighting=weightBin))
tdm
tdm.matrix <- as.matrix(tdm)
tdm.matrix
word.count <- rowSums(tdm.matrix)
word.count
word.order <- order(word.count, decreasing=TRUE)

freq.words <- tdm.matrix[word.order[c(1, 3, 6, 9, 14, 23, 24, 33, 38, 44, 45, 50, 62, 60, 63, 64, 68, 70, 71, 80,86,87,96,97,105,106,109,110,113,114,121,122,124,136,148 )], ]
freq.words<- tdm.matrix[word.order[1:150], ]

co.matrix <- freq.words %*% t(freq.words)
head(co.matrix)
df <- as.data.frame(co.matrix)
library(rvest)

library(networkD3)

qgraph(co.matrix, labels=rownames(co.matrix),
       diag=FALSE, layout='spring', threshold=1,
       vsize=log(diag(co.matrix)) *1.3)
