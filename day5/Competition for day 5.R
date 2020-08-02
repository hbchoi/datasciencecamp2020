load("C:/Users/sec/Desktop/개인 업무 자료/DRA/R 캠프 앙상블/day5Data.RData")

#tr <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/train.csv")

#te <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/test.csv")
# 구조 확인  
str(tr)
str(te)
  
### EDA & Feature Engineering
  
# factor 데이터 type 확인
# 자 이렇게 factor 타입이 다른 데이터들이 있을 수 있다. - ganwon, gwangju는 학습에, jeju는 테스트 데이터에 존재... 이런 경우 어떻게 해야할까?
table(tr$sido)
table(te$sido)
  
# dental_clinic은 train에만 있고 test에는 없다.
# "" 공란의 level도 하나 존재한다.
table(tr$instkind)
table(te$instkind)
  
table(tr$ownerChange)
table(te$ownerChange)
  
  # 이 경우에는 level들을 다 통일해줘야 한다. 모든 경우를 다 포함하는 합집합의 level를 공통 레벨로 삼게 한다.
Sido <- c("busan", "choongbuk", "choongnam","daegu","daejeon","gangwon","gwangju","gyeongbuk","gyeonggi","gyeongnam","incheon",
          "jeonbuk","jeonnam","sejong","seoul","ulsan","jeju")
  
Instkind <- c("", "clinic", "dental_clinic", "general_hospital","hospital","nursing_hospital","traditional_clinic","traditional_hospital")
  
tr$sido <- factor(tr$sido, levels = Sido)
te$sido <- factor(te$sido, levels= Sido)
  
tr$instkind <- factor(tr$instkind, levels= Instkind)
te$instkind <- factor(te$instkind, levels=Instkind)
  
  
# train과 test간의 type이 다른 employee1, employee2
# train은 int형으로 되어 있지만, test는 factor로 되어 있음
# factor형을 int형으로 바꾸어 주기로 하자.
# 다만, ','가 들어가 있는 것을 빼주는 전처리를 먼저 한다.
te$employee1
te$employee2
# ','를 빼는 이유!
a <- as.factor("1,234")
a
a <- as.numeric(a)
a
te$employee1 <- gsub(",", "", te$employee1)
te$employee2 <- gsub(",", "", te$employee2)
te$employee1 <- as.integer(te$employee1)
te$employee2 <- as.integer(te$employee2)

# NA 갯수 확인
colSums(is.na(tr))
colSums(is.na(te))

# tr의 경우 NA로 가득한 관측치가 있다. 
rowSums(is.na(tr))
rowSums(is.na(te))
# 그러한 관측치는 제거
tr <- tr[rowSums(is.na(tr))<48,]

# 수치형태로 전환
tr$OC <- ifelse(tr$OC=='open',1,0)
tr$OC <- as.factor(tr$OC)
## 단순하게 fill로 채우는 과정을 거쳐봄
library(tidyr)
fill_na <- function(df, col){
  fill(df, col, .direction='updown') -> df
  return(df)
}
colSums(is.na(tr))  
# 나머지 결측치 제거
tr <- fill_na(tr, 'bedCount')
tr <- fill_na(tr, 'employee1')
tr <- fill_na(tr, 'employee2')
tr <- fill_na(tr, 'ownerChange')
colSums(is.na(tr))

colnames(te)

for(i in 3:58){
  te <- fill_na(te, colnames(te)[i])
}
  
colSums(is.na(te))

tr$openDate
## 개업 날짜 데이터 전처리
tr$openDate <- sapply(tr$openDate, FUN= function(x){as.factor(x%/%10000)})
te$openDate <- sapply(te$openDate, FUN= function(x){as.factor(x%/%10000)})

levels(tr$openDate)
levels(te$openDate)

setdiff(levels(tr$openDate), levels(te$openDate))
setdiff(levels(te$openDate), levels(tr$openDate))

years <- c( "2007","2016","2000" ,"2005", "2002", "1982", "1987", "2006", "2008", "2013", "1981", "2012", "1994", "1996", "2004", "1997", "2003", "2001", "2014", "2011", "1988",
            "1998", "2009" ,"2017", "1993", "1983", "2010", "1999", "1985", "2015", "1984", "1978", "1989", "1980", "1986", "1992", "1995", "1976")

tr$openDate <- factor(tr$openDate, levels=years)
te$openDate <- factor(te$openDate, levels=years)

# factor 변수인 -- sido, openDate, instkind, ownerChange 제외하고 전부 gather로 묶어서 정리
colnames(tr)
trim_tr <- tr[,c(-1,-3,-5,-7,-58)]
library(dplyr)
trim_tr %>% gather(key=variable, value, -OC) -> gather_tr
head(gather_tr,10)
library(ggplot2)
## 피처의 전반적인 특징을 확인할 수 있다..!
gather_tr %>% ggplot(aes(x=OC,y=value))+facet_wrap(~variable, scales='free') + geom_boxplot()

# 변수 간의 관계도 확인해볼 수 있다...!
library(corrplot)
trim_tr <- trim_tr[,-1]
cors <- cor(trim_tr)
corrplot(cors)

# factor 형 변수의 영향력도 확인할 수 있다.
table(tr$sido, tr$OC)
table(tr$openDate, tr$OC)
table(tr$instkind, tr$OC)
table(tr$ownerChange, tr$OC)

## 새로운 파생 변수도 추가할 수 있다...!

# 수익 차 변수 추가
tr$revenue0 <- tr$revenue1 - tr$revenue2
te$revenue0 <- te$revenue1 - te$revenue2

# 이윤 차 변수 추가
tr$profit0 <- tr$profit1 - tr$profit2
te$profit0 <- te$profit1 - te$profit2

# 설명 변수 column들
# 일단 모든 피처를 다 집어 넣었을 때
name <- names(tr)[3:ncol(tr)]
name
  
fomula2 <- as.formula(paste('OC',paste(name,collapse='+'),sep='~'))
fomula2
  
set.seed(10)
library(randomForest)
first_rf <- randomForest(fomula2, tr)
varImpPlot(first_rf)

head(sub, 10)
