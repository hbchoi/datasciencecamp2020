setwd('C:/Users/PRIME#93/Google 드라이브/한동대 문서_/4학년/데이터 외전 캠프')
positive <- readLines('positive.txt', encoding = 'UTF-8')
negative <- readLines('negative.txt', encoding = 'UTF-8')
negative %>% head
emotion <- read.csv('emotion.csv')
emotion %>% head
save(positive, negative, emotion, parasite, hideandseek, aladin, file = 'day2.RData')
load("C:/Users/PRIME#93/Google 드라이브/한동대 문서_/4학년/데이터 외전 캠프/day2.RData")

#Naver movie crawling 
#1번페이지만 로딩 -> read.html encoding설정하기!! 
#1 Parasite

library(rvest)
parasite_url <- 'https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=161967&target=&page='
parasite.review  <- c()
for(page in 1: 5){
  url <- paste0(parasite_url,page)
  htxt <- read_html(url, encoding="CP949")
  table <- html_nodes(htxt, '.list_netizen') %>% html_nodes('.title') %>%
    html_text()

  if(length(table)==0){break}
  parasite.review  <- c(parasite.review , table)
  print(page)
}


head(parasite.review )
tail(parasite.review )

#2 hide and seek
hideandseek_url <- "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=102824&target=&page="
hideandseek.review <- c()
for(page in 1:5){
  url <- paste0(hideandseek_url,page)
  htxt <- read_html(url, encoding="CP949")
  table <- html_nodes(htxt, '.list_netizen') %>% html_nodes('.title') %>% html_text()
  
  if(length(table)==0){break}
  hideandseek.review <- c(hideandseek.review, table)
  print(page)
}

head(hideandseek.review)
tail(hideandseek.review)

#3 Aladin 
aladin_url  <- 'https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=163788&target=&page='
aladin.review <- c()
for(page in 1:5){
  url <- paste0(aladin_url,page)
  print(url)
  htxt <- read_html(url, encoding="CP949")
  table <- html_nodes(htxt, '.list_netizen')
  content <- html_nodes(table, '.title')
  reviews <- html_text(content)
  
  if(length(table)==0){break}
  aladin.review <- c(aladin.review, reviews)
  print(page)
}

head(aladin.review)
tail(aladin.review)

aladin.review

#Text data (only me)  ------------------------------
#https://kutar37.tistory.com/entry/R%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-%ED%85%8D%EC%8A%A4%ED%8A%B8%EB%A7%88%EC%9D%B4%EB%8B%9D-%EC%9B%8C%EB%93%9C%ED%81%B4%EB%9D%BC%EC%9A%B0%EB%93%9C

library(KoNLP)
useSejongDic()
#기생충 
parasite.review_noun <- sapply(parasite.review, extractNoun, USE.NAMES = F)
parasite.review_unlist <- unlist(parasite.review_noun) #list -> vector
head(parasite.review_unlist, 20)
parasite.review_unlist <- gsub("영화", "", parasite.review_unlist)
parasite.review_unlist <- gsub("기생충", "", parasite.review_unlist)
parasite.review_unlist <- gsub("신고", "", parasite.review_unlist)
parasite.review_unlist <- gsub("평점", "", parasite.review_unlist)
parasite.review_unlist <- gsub("몰할것", "", parasite.review_unlist)
parasite.review_unlist <- gsub("별점", "", parasite.review_unlist)
parasite.review_unlist <- gsub("개인적으로", "", parasite.review_unlist)
parasite.review_unlist <- gsub("만큼", "", parasite.review_unlist)
parasite.review_unlist <- gsub("할거", "", parasite.review_unlist)
parasite.review_unlist<- gsub('[~!@#$%&*()_+=?<>,"]','',parasite.review_unlist)
parasite.review_unlist <- gsub("[A-Za-z]", "", parasite.review_unlist)
parasite.review_unlist <- gsub("\\[","",parasite.review_unlist)
parasite.review_unlist <- gsub('[ㄱ-ㅎ]','',parasite.review_unlist)
parasite.review_unlist<- gsub('(ㅜ|ㅠ)','',parasite.review_unlist)
parasite.review_unlist <- gsub("\\d+","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\▲","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\.","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\/","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\^","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\:","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\-","",parasite.review_unlist)
parasite.review_unlist <- gsub("\\.","",parasite.review_unlist)
parasite.review_unlist <- gsub("또한같은","",parasite.review_unlist)
parasite.review_unlist <- gsub("크으","",parasite.review_unlist)
parasite.review_unlist <- gsub("하나","",parasite.review_unlist)
parasite.review_unlist <- gsub("대한","",parasite.review_unlist)
parasite.review_unlist<-  gsub("감독상\\S*", "감독상", parasite.review_unlist)
parasite.review_unlist <- gsub("말한마디에넘어가는","",parasite.review_unlist)
parasite.review_unlist <- gsub("들이","",parasite.review_unlist)
parasite.review_unlist <- gsub("보나마나","",parasite.review_unlist)
parasite.review_unlist <- gsub("였다는","",parasite.review_unlist)
parasite.review_unlist <- gsub("삼는건","",parasite.review_unlist)
parasite.review_unlist <- gsub("척만할뿐","",parasite.review_unlist)
parasite.review_unlist <- gsub("하게","",parasite.review_unlist)
parasite.review_unlist <- gsub("들마","",parasite.review_unlist)
parasite.review_unlist <- gsub("보셈","",parasite.review_unlist)
parasite.review_unlist <- gsub("로운","",parasite.review_unlist)
parasite.review_unlist <- gsub("보게되요","",parasite.review_unlist)
parasite.review_unlist <- gsub("재인이와","",parasite.review_unlist)
parasite.review_unlist <- gsub("여러번","",parasite.review_unlist)
parasite.review_unlist <- gsub("오른걸","",parasite.review_unlist)
parasite.review_unlist <- gsub("받앗는지는","",parasite.review_unlist)
parasite.review_unlist <- gsub("그러하구요","",parasite.review_unlist)
parasite.review_unlist <- gsub("모르겟지만","",parasite.review_unlist)
parasite.review_unlist <- gsub("있는사람이","",all.reviews1_unlist)
parasite.review_unlist <- gsub("잘봤습니다","",parasite.review_unlist)
parasite.review_unlist <- gsub("생각밖에안들었음","",parasite.review_unlist)
parasite.review_unlist <- gsub("보나봐뭐","",parasite.review_unlist)
parasite.review_unlist <- gsub("던졌단건","",parasite.review_unlist)

parasite.review_unlist<-  gsub("찝찝\\S*", "찝찝", parasite.review_unlist)
parasite.review_unlist<-  gsub("부족\\S*", "부족", parasite.review_unlist)
parasite.review_unlist<-  gsub("반지하\\S*", "반지하", parasite.review_unlist)
parasite.review_unlist<-  gsub("극찬\\S*", "극찬", parasite.review_unlist)
parasite.review_unlist <- Filter(function(x){nchar(x)>=2}, parasite.review_unlist)
parasite.review_unlist<-  gsub("금수저\\S*", "금수저", parasite.review_unlist)
parasite.review_unlist<-  gsub("봉준호\\S*", "봉준호", parasite.review_unlist)
parasite.review_unlist<-  gsub("계급\\S*", "계급", parasite.review_unlist)
parasite.review_unlist<-  gsub("웃김\\S*", "욱심", parasite.review_unlist)
parasite.review_unlist<-  gsub("잘\\S*", "잘", parasite.review_unlist)
parasite.review_unlist<-  gsub("소름\\S*", "소름", parasite.review_unlist)
parasite.review_unlist<-  gsub("생각\\S*", "생각", parasite.review_unlist)
parasite.review_unlist<-  gsub("상\\S*", "상", parasite.review_unlist)
parasite.review_unlist<-  gsub("재밋어요\\S*", "재밌는", parasite.review_unlist)

parasite.review_unlist
head(parasite.review_unlist)
str(parasite.review_unlist)
tail(parasite.review_unlist)
parasite.review_unlist
#if needed, adding word
mergeUserDic(data.frame(c("노잼"), "ncn"))


#숨바꼭질 
hideandseek.review_noun <- sapply(hideandseek.review, extractNoun, USE.NAMES = F)
hideandseek.review_unlist <- unlist(hideandseek.review_noun) #list -> vector
head(hideandseek.review_unlist, 40)
hideandseek.review_unlist <- gsub("영화", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("숨바꼭질", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("신고", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("평점", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("되버린", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("별점", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("개인적으로", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("만큼", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("할거", "", hideandseek.review_unlist)
hideandseek.review_unlist<- gsub('[~!@#$%&*()_+=?<>,"]','',hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("[A-Za-z]", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\[","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub('[ㄱ-ㅎ]','',hideandseek.review_unlist)
hideandseek.review_unlist<- gsub('(ㅜ|ㅠ)','',hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\d+","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\▲","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\.","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\/","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\^","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\:","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\-","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("\\.","",hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("애새끼", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("존나", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("징징대고", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("보다끔", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("스마트폰시대에살고", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("고급아파트를살면서", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("스연레기", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("이리없는사람이", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("저리살수있다는게", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("벌써다", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("쓰러지네개", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("볼껄", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("빠따", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("보는내내", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("할줄모르는", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("쓰러지네개", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("냅둔거는", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("집착주희가", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("집문은", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("했음", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("케릭터여야", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("아무리전개를", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("할줄모르는", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("캐리할만하면", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("이런가", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("끝냈으면중간이라도", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("내시간시간분돌리도", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("넣질않나", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("다른걸", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("라는단어를", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("왜이렇게", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("들이", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("폼으로있냐", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("나온이상", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("생긴다는걸", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("쳐맞고", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("케릭터에", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("전무상황에", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("안만들었으면함", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("제발이런류으이", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("생긴다는걸", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("왜이렇게", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("정도껏", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("달려있을꺼고", "", hideandseek.review_unlist)
hideandseek.review_unlist <- gsub("듯이", "", hideandseek.review_unlist)


hideandseek.review_unlist<-  gsub("암\\S*", "암", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("찝찝\\S*", "찝찝", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("부족\\S*", "부족", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("생각\\S*", "생각", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("극찬\\S*", "극찬", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("남자\\S*", "남자", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("재밌\\S*", "재미", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("마이너스으로\\S*", "마이너스", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("답답\\S*", "답답", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("일관성\\S*", "일관성", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("긴장감\\S*", "긴장감", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("손현주\\S*", "손현주", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("킬링타임\\S*", "킬링타임", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("원펀치\\S*", "원펀치", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("시간\\S*", "시간", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("손현주\\S*", "손현주", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("킬링타임\\S*", "킬링타임", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("원펀치\\S*", "원펀치", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("다른집\\S*", "다른집", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("저럼고급아파트\\S*", "아파트", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("있을거\\S*", "", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("무섭\\S*", "무섭", hideandseek.review_unlist)
hideandseek.review_unlist<-  gsub("화딱지\\S*", "화딱지", hideandseek.review_unlist)
hideandseek.review_unlist
hideandseek.review_unlist <- Filter(function(x){nchar(x)>=2}, hideandseek.review_unlist)

hideandseek.review_unlist



#알라딘
aladin.review_noun <- sapply(aladin.review, extractNoun, USE.NAMES = F)
aladin.review_unlist <- unlist(aladin.review_noun) #list -> vector
head(aladin.review_unlist, 40)
aladin.review_unlist <- gsub("영화", "", aladin.review_unlist)
aladin.review_unlist <- gsub("알라딘", "", aladin.review_unlist)
aladin.review_unlist <- gsub("신고", "", aladin.review_unlist)
aladin.review_unlist <- gsub("평점", "", aladin.review_unlist)
aladin.review_unlist <- gsub("몰할것", "", aladin.review_unlist)
aladin.review_unlist <- gsub("별점", "", aladin.review_unlist)
aladin.review_unlist <- gsub("개인적으로", "", aladin.review_unlist)
aladin.review_unlist <- gsub("만큼", "", aladin.review_unlist)
aladin.review_unlist <- gsub("할거", "", aladin.review_unlist)
aladin.review_unlist<- gsub('[~!@#$%&*()_+=?<>,"]','',aladin.review_unlist)
aladin.review_unlist <- gsub("[A-Za-z]", "", aladin.review_unlist)
aladin.review_unlist <- gsub("\\[","",aladin.review_unlist)
aladin.review_unlist <- gsub('[ㄱ-ㅎ]','',aladin.review_unlist)
aladin.review_unlist<- gsub('(ㅜ|ㅠ)','',aladin.review_unlist)
aladin.review_unlist <- gsub("\\d+","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\▲","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\.","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\/","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\^","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\:","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\-","",aladin.review_unlist)
aladin.review_unlist <- gsub("\\.","",aladin.review_unlist)
aladin.review_unlist<-  gsub("재밌\\S*", "재밌 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("심규혁은\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("못라고걱정들을하셧대\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("안쓰는\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("영환\\S*", "영화 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("너무지루\\S*", "지루 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("못생견어\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("찰떡\\S*", "찰떡 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("봐도봐도재밌다\\S*", "재미 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("빨리나와라\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("라귀여우넘앙증뿌띠세상에\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("슬퍼서우는게아니라\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("어떤것처럼\\S*", "", aladin.review_unlist)
aladin.review_unlist<-  gsub("걍말이필요없을정도로\\S*", "말이필요없음", aladin.review_unlist)
aladin.review_unlist<-  gsub("귀가\\S*", "귀", aladin.review_unlist)
aladin.review_unlist<-  gsub("팬됬어요\\S*", "팬 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("네러티브지만\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("페스를\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("이스탄불과\\S*", "이스탄불", aladin.review_unlist)
aladin.review_unlist<-  gsub("코르도바를\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("뭔기준인지\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("봤네요집에서\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("눈물이나는건\\S*", "눈물 ", aladin.review_unlist)
aladin.review_unlist<-  gsub("섭외하더라도\\S*", "섭외", aladin.review_unlist)

aladin.review_unlist<-  gsub("보는재미가\\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("짱재미\\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("개재미\\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("재밌\\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("재미\\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("잔뜩나와서\\S*", "잔뜩", aladin.review_unlist)
aladin.review_unlist<-  gsub("울컥합니다최고\\S*", "울컥", aladin.review_unlist)
aladin.review_unlist<-  gsub("디게웃기넹자기들이뭔데\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("봐도봐도재밌 \\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("잘봤습니당\\S*", "잘봄", aladin.review_unlist)
aladin.review_unlist<-  gsub("영리하달까\\S*", "영리", aladin.review_unlist)
aladin.review_unlist<-  gsub("영악하달까 \\S*", "영악", aladin.review_unlist)
aladin.review_unlist<-  gsub("잘봤습니당\\S*", "잘봄", aladin.review_unlist)
aladin.review_unlist<-  gsub("못본거\\S*", " ", aladin.review_unlist)
aladin.review_unlist<-  gsub("잘봤습니당\\S*", "잘봄", aladin.review_unlist)
aladin.review_unlist<-  gsub("컸음주연들도\\S*", "주연들", aladin.review_unlist)
aladin.review_unlist<-  gsub("재개봉\\S*", "재개봉", aladin.review_unlist)
aladin.review_unlist<-  gsub("보는내내\\S*", "보는", aladin.review_unlist)
aladin.review_unlist<-  gsub("들었어요우울할때\\S*", "우울할", aladin.review_unlist)
aladin.review_unlist<-  gsub("암튼\\S*", "암튼", aladin.review_unlist)
aladin.review_unlist<-  gsub("양탄자타고날아갈때\\S*", "양탄자", aladin.review_unlist)
aladin.review_unlist<-  gsub("낑겨날아갈수있다\\S*", "날아갈", aladin.review_unlist)
aladin.review_unlist<-  gsub("잘봤습니당\\S*", "잘봄", aladin.review_unlist)
aladin.review_unlist<-  gsub("후회함\\S*", "후회", aladin.review_unlist)
aladin.review_unlist<-  gsub("행복햇어요\\S*", "행복", aladin.review_unlist)
aladin.review_unlist<-  gsub("소름돋\\S*", "소름돋다", aladin.review_unlist)
aladin.review_unlist<-  gsub("내용도재미\\S*", "재미", aladin.review_unlist)
aladin.review_unlist<-  gsub("감동감동\\S*", "감동", aladin.review_unlist)
aladin.review_unlist<-  gsub("너무쟈밌는것같아요\\S*", "재밌는", aladin.review_unlist)
aladin.review_unlist<-  gsub("너무좋았고\\S*", "좋다", aladin.review_unlist)


aladin.review_unlist <- Filter(function(x){nchar(x)>=2}, aladin.review_unlist)
aladin.review_unlist

parasite.review_unlist
hideandseek.review_unlist
aladin.review_unlist



#only me done ------------------------------------------------

#명사 형용사 파일 불러와서 전처리 

library(ggplot2)

#-----------빈도분석-------------------------------------------------
#frequency analysis (빈도분석)
parasite_wordcount  <- table(parasite) %>% sort(decreasing = T) %>% 
  as.data.frame()
parasite_wordcount %>% head

ggplot(parasite_wordcount[1:10,], aes(x = parasite , y = Freq))+
  geom_bar(stat = 'identity')


hideandseek_wordcount  <- table(hideandseek) %>% sort(decreasing = T) %>% 
  as.data.frame()
hideandseek_wordcount %>% head
ggplot(hideandseek_wordcount[1:10,], aes(x = hideandseek , y = Freq))+
  geom_bar(stat = 'identity')



aladin_wordcount  <- table(aladin) %>% sort(decreasing = T) %>% 
  as.data.frame()
aladin_wordcount %>% head
ggplot(aladin_wordcount[1:10,], aes(x = aladin , y = Freq))+
  geom_bar(stat = 'identity')

#-----------워드크라우드-------------------------------------------------
# wordcloud analysis 
library(wordcloud)
library(RColorBrewer)
display.brewer.all()
color <- brewer.pal(12, "Set3")
wordcloud(parasite_wordfreq$parasite, parasite_wordfreq$Freq, min.freq = 2)

wordcloud(parasite_wordfreq$parasite, parasite_wordfreq$Freq,
          random.order = FALSE, random.color = TRUE, colors = color,  min.freq = 2)
wordcloud(hideandseek_wordfreq$hideandseek, parasite_wordfreq$Freq,
          random.order = FALSE, random.color = TRUE, colors = color, min.freq = 2)
wordcloud(aladin_wordfreq$aladin, parasite_wordfreq$Freq,
          random.order = FALSE, random.color = TRUE, colors = color, min.freq = 2)
#options 
wordcloud(parasite_wordfreq$parasite, parasite_wordfreq$Freq,
          min.freq = 3,colors = color)
wordcloud(aladin_wordfreq$aladin, parasite_wordfreq$Freq, 
          max.words = 30, colors = color)

#-----------감성분석-------------------------------------------------
#sentiment analysis (크롤링 txt 그대로 사용!! )
library(tidyr)
library(dplyr)

#left_join 함수 사용해 감성점수 매기기 (전체 합계)
#1 
parasite_wordcount %>% head
colnames(parasite_wordcount) <- c('word', 'Freq')
parasite_senti <- left_join(parasite_wordcount, emotion, by = 'word') %>% 
  filter(type == 'positive'|type == 'negative')
parasite_senti

# 긍정 단어 / 부정 단어 
parasite_pos <- parasite_senti %>% filter(type == 'positive')
parasite_neg <- parasite_senti%>% filter(type == 'negative')
wordcloud(parasite_pos$word, parasite_pos$Freq, min.freq = 1, color = 'green')
wordcloud(parasite_neg$word, parasite_neg$Freq, min.freq = 1,color = 'red')

# 긍정 / 부정 전체 지수 
# 전체 지수 비교 
parasite_senti %>% group_by(type) %>% summarise(sum(Freq))

type <- c('negative','positive', 'total')
score <- c(-27,22, -5)
parasite_sentimental <- data.frame(type, score)
parasite_sentimental %>% ggplot(aes(x = type, y = score)) + geom_bar(stat = 'identity', aes(fill = type))

parasite_sentimental[-3,] %>% ggplot(aes(x = "", y = score)) +
  geom_bar(stat = 'identity', aes(fill = type)) + coord_polar('y')


#left_join 함수 사용해 감성점수 매기기 (전체 합계)
#1 
hideandseek_wordcount %>% head
colnames(hideandseek_wordcount) <- c('word', 'Freq')
hideandseek_senti <- left_join(hideandseek_wordcount, emotion, by = 'word') %>% 
  filter(type == 'positive'|type == 'negative')
hideandseek_senti

# 긍정 단어 / 부정 단어 
hideandseek_pos <- hideandseek_senti %>% filter(type == 'positive')
hideandseek_neg <- hideandseek_senti%>% filter(type == 'negative')
wordcloud(hideandseek_pos$word, hideandseek_pos$Freq, min.freq = 1, color = 'green')
wordcloud(hideandseek_neg$word, hideandseek_neg$Freq, min.freq = 1,color = 'red')

# 긍정 / 부정 전체 지수 
# 전체 지수 비교 
hideandseek_senti %>% group_by(type) %>% summarise(sum(Freq))

type <- c('negative','positive', 'total')
score <- c(-33,25, -8)
hideandseek_sentimental <- data.frame(type, score)
hideandseek_sentimental %>% ggplot(aes(x = type, y = score)) + geom_bar(stat = 'identity', aes(fill = type))

hideandseek_sentimental[-3,] %>% ggplot(aes(x = "", y = score)) + geom_bar(stat = 'identity', aes(fill = type)) + coord_polar('y')


#left_join 함수 사용해 감성점수 매기기 (전체 합계)
#1 
aladin_wordcount %>% head
colnames(aladin_wordcount) <- c('word', 'Freq')
aladin_senti <- left_join(aladin_wordcount, emotion, by = 'word') %>% 
  filter(type == 'positive'|type == 'negative')
aladin_senti

# 긍정 단어 / 부정 단어 
aladin_pos <- aladin_senti %>% filter(type == 'positive')
aladin_neg <- aladin_senti%>% filter(type == 'negative')
wordcloud(aladin_pos$word, aladin_pos$Freq, min.freq = 1, color = 'green')
wordcloud(aladin_neg$word, aladin_neg$Freq, min.freq = 1,color = 'red')

# 긍정 / 부정 전체 지수 
# 전체 지수 비교 
aladin_senti %>% group_by(type) %>% summarise(sum(Freq))

type <- c('negative','positive', 'total')
score <- c(-7,27, 20)
hideandseek_sentimental <- data.frame(type, score)
hideandseek_sentimental %>% ggplot(aes(x = type, y = score)) + geom_bar(stat = 'identity', aes(fill = type))

hideandseek_sentimental[-3,] %>% ggplot(aes(x = "", y = score)) + geom_bar(stat = 'identity', aes(fill = type)) + coord_polar('y')




#감성분석 함수  (리뷰별)
library(plyr)     #laply() 함수 제공
library(stringr)  #str_split() 함수 제공 
#각각 리뷰별 감성점수를 매겨 시각화 
#score을 계산해서 array로 반환 
#function 안에 필요한 사전 불러오기  
#dataframe 만들기 (score & text 로 )

sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores)
  return(scores.df)
}

#parasite
parasite_senti<- sentimental(parasite.review, positive, negative)
parasite_senti

parasite_senti$remark[parasite_senti$score >=1] = "긍정"
parasite_senti$remark[parasite_senti$score ==0] = "중립"
parasite_senti$remark[parasite_senti$score < 0] = "부정"

parasite_senti

parasite_sentiment_result<-  table(parasite_senti$remark)
parasite_sentiment_result <- as.data.frame(parasite_sentiment_result)
parasite_sentiment_result

ggplot(parasite_sentiment_result, aes(x = "",y = Freq, fill = Var1)) + 
  geom_bar(stat = 'identity') + coord_polar("y")


#hide and seek 
hideandseek_senti<- sentimental(hideandseek.review, positive, negative)
hideandseek_senti

hideandseek_senti$remark[hideandseek_senti$score >=1] = "긍정"
hideandseek_senti$remark[hideandseek_senti$score ==0] = "중립"
hideandseek_senti$remark[hideandseek_senti$score < 0] = "부정"


hideandseek_sentiment_result <- table(hideandseek_senti$remark)
hideandseek_sentiment_result <- as.data.frame(hideandseek_sentiment_result)
hideandseek_sentiment_result

ggplot(hideandseek_sentiment_result, aes(x = "",y = Freq, fill = Var1)) + 
  geom_bar(stat = 'identity') + coord_polar("y")



#aladin
aladin_senti<- sentimental(aladin.review, positive, negative)
aladin_senti
aladin_senti$remark[aladin_senti$score >=1] = "긍정"
aladin_senti$remark[aladin_senti$score ==0] = "중립"
aladin_senti$remark[aladin_senti$score < 0] = "부정"

aladin_sentiment_result = table(aladin_senti$remark)
aladin_sentiment_result <- as.data.frame(aladin_sentiment_result)
aladin_sentiment_result

ggplot(aladin_sentiment_result, aes(x = "",y = Freq, fill = Var1)) + 
  geom_bar(stat = 'identity') + coord_polar("y")







#---------------시간상 어려울듯...------------------------------------
#상관성 분석    
#명사형용사 추출 이후
library(tm)
library(qgraph)

# VectorSource함수는 Source 객체로 변형시킴
# 그래야 Corpus라는 말뭉치 형태로 단어를 저장할 수 있음  
DtaCorpusNC <- Corpus(VectorSource(parasite)) 
#DtaCorpusNC <- Corpus(VectorSource(hideandseek)) 
#DtaCorpusNC <- Corpus(VectorSource(aladin)) 
DtaCorpusNC
#말뭉치를 TermDocumentMatrix에 넣어 단어문서행렬을 생성한다. 
#그러면 단어의 빈도수를 행렬로 저장한다.
#matrix에 종류가 몇가지 있음 
#TermDocumentMatrix: 단어가 document별로 얼마나 나왔는지를 행렬 형태로 보여줌 
myTdmNC <- TermDocumentMatrix(DtaCorpusNC, 
                              control = list(wordLenghts = c(2, 6),
                                             weighting = weightBin))
myTdmNC
str(myTdmNC)
#들어간 문서 인코딩 
Encoding(myTdmNC$dimnames$Terms) = 'UTF-8'
findFreqTerms(myTdmNC, lowfreq = 1) # 빈번히 출현 단어 찾아주는 함수 
mtNC <- as.matrix(myTdmNC) # str(myTdmNC) -> list를 matrix로 ***
Encoding(rownames(mtNC)) <- 'UTF-8'
mtNC
mtrowNC <- rowSums(mtNC) # 단어별 합계 
mtNC.order <- mtrowNC[order(mtrowNC, decreasing = T)]
freq.wordsNC <- mtNC.order[mtNC.order>=2]
freq.wordsNC
#co-occurence Matrix로 변경 
co.matrix <- freq.wordsNC %*% t(freq.wordsNC) #행렬의 곱셈 -> 

#t() transpose: 행과 열을 바꿔줌 
qp <- qgraph(co.matrix, 
             labels = rownames(co.matrix), 
             diag = F, # 자기 자신과의 관계 제거 
             layout = 'spring', #스프링처럼 연결성이 강할수록 가까이 
             edge.color = 'blue')


#한글 인코딩 
Sys.setlocale("LC_ALL", "korean")
Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8");
install.packages("extrafont")
library(extrafont) ## 잊지마세요 package 불러오기!
font_import() #설치된 모든 폰트 가져오기
cairo_pdf("Routput.pdf",family="Nanumgothic")
#나눔고딕 폰트 지정 
