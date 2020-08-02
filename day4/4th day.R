# credit_train, temperature_train 데이터 활용
load("C:/Users/sec/Desktop/개인 업무 자료/DRA/R 캠프 앙상블/eduforday4.RData")

## Bagging and Boosting 개념 요약
### 1. Bagging 직접 구현 방법
str(spamD)
# 이제 본격적으로 앙상블 구현을 해보자
dim(spamD)

ntrain <- dim(spamD)[1]
n <- ntrain
ntree <- 100

## 분할
library(caret)
spam_idx <- createDataPartition(spamD$spam, p=.8, list=F)
spam_tr <- spamD[spam_idx, ]
spam_te <- spamD[-spam_idx, ]

## 4601개의 랜덤한 샘플들을 5개 만들어보자 그리고 그 샘플들에 모델링을 적용해보자..!
library(rpart)
sample1 <- sample(1:ntrain, size=n, replace=T)
bag_model1 <- rpart(spam~., data=spam_tr[sample1,])

sample2 <- sample(1:ntrain, size=n, replace=T)
bag_model2 <- rpart(spam~., data=spam_tr[sample2,])

sample3 <- sample(1:ntrain, size=n, replace=T)
bag_model3 <- rpart(spam~., data=spam_tr[sample3,])

sample4 <- sample(1:ntrain, size=n, replace=T)

bag_model4 <- rpart(spam~., data=spam_tr[sample4,])

sample5 <- sample(1:ntrain, size=n, replace=T)

bag_model5 <- rpart(spam~., data=spam_tr[sample5, ])

pre1 <- predict(bag_model1, newdata=spam_te)[,2]

pre2 <- predict(bag_model2, newdata=spam_te)[,2]

pre3 <- predict(bag_model3, newdata=spam_te)[,2]

pre4 <- predict(bag_model4, newdata=spam_te)[,2]

pre5 <- predict(bag_model5, newdata=spam_te)[,2]

pred_sums <- (pre1+pre2+pre3+pre4+pre5)
pred_sums <- pred_sums/5

# predict.bag에서 산출된 확률을 threshold로 사용하는 것!
# 정확도 테스트 하는 부분
accuracyMeasures <- function(pred, truth, name="model"){
  ctable <- table(actual = truth, pred =ifelse(pred>mean(pred),1,0))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- 2*precision*recall/(precision+recall)
  data.frame(model=name, accuracy=accuracy,precision=precision, recall=recall, f1=f1)
}

accuracyMeasures(pre1, spam_te$spam)
accuracyMeasures(pred_sums, spam_te$spam)

# 그렇다면 이제 그 sample들 5개가 아닌 100개를 만들어 수행해보자
samples <- sapply(1:ntree, FUN = function(iter){sample(1:ntrain,size=n,replace=T)})
head(samples,3)

set.seed(123)
# 각 데이터에 맞게 100개의 decision tree를 구축한다, 이 부분 코드는 돌리지 않습니다..!(시간 과소비)
treelists <- lapply(1:ntree, FUN = function(iter){samp <- samples[,iter]; rpart(spam~.,data=spam_tr[samp,])})
head(treelists,2)

# 만든 여러개의 모델의 예측치의 평균 수치 -- bagging의 과정

predict.bag <- function(treelist, newdata){
  preds <- sapply(1:length(treelist),
                  FUN=function(iter){
                    predict(treelist[[iter]],newdata=newdata)[,2]
                  })
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

p <- sapply(1:length(treelists),FUN=function(iter){predict(treelists[[iter]],newdata=spam_te)[,2]})
psum <- rowSums(p)
psum
psum/length(treelists)

accuracyMeasures(predict.bag(treelists,newdata=spam_te),spam_te$spam)

######## Classification

credit_bg <- credit_train
str(credit_bg)

# 보험사나 은행 같은 금융기관에서 해당 고객이 금액을 대출 받고 상환한 금액과 사용 금액들에 대한 정보가 나와있고

#채무 불이행의 여부를 예측하는 모델이다.

# caret 패키지의 createDataPartition 함수를 통해 train, test데이터를 나눈다.
library(caret)
bg_indx <- createDataPartition(credit_train_bg$default.payment.next.month, p=.8, list=F)
credit_bg1 <- credit_bg[bg_indx, ]
credit_bg2 <- credit_bg[-bg_indx, ]

#### <randomForest>
#parameter에 대한 설명

#mtry는 각각의 tree마다 몇 개의 feature를 사용할 것인지를 정하는 것.
#이는 각 트리의 랜덤성을 결정, 클수록 과적합을 줄인다.

#mtry == max_features

#mtry값이 큰 경우 : 모든 특성을 고려하므로 피처 선택에 무작위성이 들어가지 않는다. 
#하지만 부트스트랩 샘플링으로 인한 무작위성은 그대로 유지된다. 
#따라서 각 트리들이 비슷해지고 가장 두드러진 특성을 이용해 데이터를 맞출 것이다.

#mtry값이 작은 경우 : 트리의 분기에서 선택되는 피처가 적어진다. 1로 설정할 경우 테스트할 특성을 
#고를 필요가 없게 되며 그냥 무작위로 선택된 특성의 임계값만을 찾는다
#따라서 각 트리들이 많이 달라지고 각 트리를 데이터에 맞추기 위해서 깊이가 깊어지게 된다.

#regression의 경우 (변수갯수)/3 or (변수갯수)자체??, classification의 경우 sqrt(변수 갯수)

#ntree는 tree의 총 갯수를 의미 – 너무 많이하면 overfit, 너무 적게 하면 underfit

#importance는 변수의 중요도를 측정할 것인지를 의미
### bagging === 과적합
#replace는 복원 추출을 할 것인지를 의미
library(randomForest)
example_rf <- randomForest(default.payment.next.month~., credit_bg1)
example_rf
example_rf$ntree
example_rf$mtry

## set.seed예제
sample(10)
set.seed(2020)
sample(10)

set.seed(2020)
rf_credit <- randomForest(default.payment.next.month~., credit_bg1, ntree=500, mtry=5, importance=T)
rf_credit

importance(rf_credit)
varImpPlot(rf_credit)

pred_credit <- predict(rf_credit, credit_bg1)
pred_credit2 <- predict(rf_credit, credit_bg2)

table(pred = pred_credit, actual = credit_bg1$default.payment.next.month)
table(pred= pred_credit2, actual = credit_bg2$default.payment.next.month)

library(ROCR)
calc_AUC <- function(model, new_data, new_label){
  pred <- predict(model, newdata=new_data, type='prob')[,2]
  p <- prediction(pred, new_label)
  auc <- performance(p, 'auc')
  return(auc@y.values[[1]])
}
set.seed(2020)
calc_AUC(rf_credit, credit_bg1, credit_bg1$default.payment.next.month)
calc_AUC(rf_credit, credit_bg2, credit_bg2$default.payment.next.month)

# 일반 logistic과의 성능 비교
credit_glm <- glm(default.payment.next.month~., credit_bg1, family='binomial')
pred <- predict(credit_glm, credit_bg2)
p_g <- prediction(pred, credit_bg2$default.payment.next.month)
aucss <- performance(p_g, 'auc')
aucss@y.values[[1]]


########### 실습 time!!!!
## ntree의 수를 바꾸어가며 성능을 테스트해보자!!
set.seed(2020)
another_rf1 <- randomForest(default.payment.next.month~., ntree=[  ], mtry=5, credit_bg1, importance=T)
calc_AUC(another_rf1, credit_bg2, credit_bg2$default.payment.next.month)

## mtry의 수를 바꾸어가며 성능을 테스트해보자!!
set.seed(2020)
another_rf2 <- randomForest(default.payment.next.month~., ntree=500, mtry=[  ], credit_bg1, importance=T)
calc_AUC(another_rf2, credit_bg2, credit_bg2$default.payment.next.month)
#############

# hyper parameter tuning, 실행을 하지는 않는다..
# 반복 교차 검증의 방법으로 2번 반복 시행
fitContrl <- trainControl(method='repeatedcv', number=2, repeats=2, search='grid')
grids <- expand.grid(.mtry=c(2:6))
modellists1 <- list()
for (ntrees in c(100,300,500,700)){
  set.seed(2020)
  rf_fit <- train(default.payment.next.month~., data=credit_bg1, method='rf', 
                  metric = 'Accuracy', tuneGrid = grids, trControl=fitContrl,
                  ntree=ntrees,verbose=T)
  key <- toString(ntrees)
  modellists1[[key]] <- rf_fit
}
modellists1

set.seed(2020)
## 보다 나은 모델을 만들 수 있다...!
rf_credit_better <- randomForest(default.payment.next.month~., credit_bg1, ntree=300, mtry=3, importance=T)
pred_credit_better <- predict(rf_credit_better, credit_bg2)
table(pred= pred_credit_better, actual = credit_bg2$default.payment.next.month)
set.seed(2020)
calc_AUC(rf_credit_better, credit_bg1, credit_bg1$default.payment.next.month)
calc_AUC(rf_credit_better, credit_bg2, credit_bg2$default.payment.next.month)

#### <C5.0>
library(C50)
c50_idx <- createDataPartition(credit_c50$default.payment.next.month, p=.8, list=F)

credit_c50_train <- credit_c50[c50_idx, ]
credit_c50_test <- credit_c50[-c50_idx,]


# 여기는 설명 변수에 factor형 변수가 없지만, 실제로 factor형 변수의 경우 levels 통일 해주어야하고 

# "" 이러한 levels 명이 되어 있는 거는 "missing"으로 치환해서 해주어야 하는 부분도 있음

#장점

#모든 문제에 적합한 분류기

#결측치, 명목속성, 수치를 처리할수 있는 자동성이 높은 학습

#가장 중요한 속성만사용

#매우 많은 수 또는 상대적으로 적은 훈련 예제와 데이터 사용

#수학적 배경 없이도 해석할 수 있는 모델 도출

#다른 복잡한 모델보다 높은 효율

#가지치기의 최적화…! – 자동으로 합리적인 기본 값을 사용해 많은 결정을 다룬다..

#단점

#결정 트리는 다수의 레벨을 가진 속성 쪽으로 구분하는 경향이 있음

#모델이 쉽게 과적합화나 과소적합화됨

#훈련 데이터에 대한 약간의 변경이 결정 논리에 큰 변화를 줌

#큰 트리는 이해하기가 어렵고 직관적이지 않음

#C5.0의 경우

#먼저 훈련 데이터에 과적합해 트리는 크게 성장한다.

#이후에 분류 오차를 가진 노드와 가지를 제거한다. 일부 경우에 전체 가지는 좀 더 단순한 결정으로 교체되거나 옮겨진다.

#이러한 변경되는 과정을 부분 트리 생성, 부분 트리 대체라고 한다.
set.seed(2020)
basic_model <- C5.0(credit_c50_train[,-24], credit_c50_train$default.payment.next.month, trials = 1)
# C5.0(train, class, trials=1, costs=NULL)
# train : 훈련 데이터 프레임
# class : 훈련 데이터의 각 행에 대해 범주를 가진 팩터 벡터
# trial : 부스팅 반복의 수를 조절하기 위한 선택적 숫자
# costs : 오차의 타입에 관련된 cost를 명시한 선택적 매트릭스

basic_model

summary(basic_model)

plot(basic_model)

## 가지치기의 경우는 자동으로 수행하는지?
c50_pred1 <- predict(basic_model, newdata=credit_c50_train)
c50_pred2 <- predict(basic_model, newdata = credit_c50_test)
table(pred=c50_pred1, actual = credit_c50_train$default.payment.next.month)
table(pred=c50_pred2, actual = credit_c50_test$default.payment.next.month)

calc_AUC(basic_model, credit_c50_train, credit_c50_train$default.payment.next.month)

calc_AUC(basic_model, credit_c50_test, credit_c50_test$default.payment.next.month)

#C5.0의 성능을 향상시킬수는 없을까..?

#C5.0에 부스팅을 추가하는 방법은 trials 매개변수를 활용

#알고리즘은 트리가 추가적인 trial이 정확도를 향상시키지 못할 것으로 보이면 트리 추가를 멈춘다.
set.seed(123)
better_model <- C5.0(credit_c50_train[,-24], credit_c50_train$default.payment.next.month, trials = 10)

summary(better_model)

c50_better1 <- predict(better_model, newdata=credit_c50_train)
c50_better2 <- predict(better_model, newdata=credit_c50_test)
table(pred=c50_better1, actual = credit_c50_train$default.payment.next.month)
table(pred=c50_better2, actual = credit_c50_test$default.payment.next.month)

calc_AUC(better_model, credit_c50_train, credit_c50_train$default.payment.next.month)

calc_AUC(better_model, credit_c50_test, credit_c50_test$default.payment.next.month)


#성능이 조금 더 개선됨을 확인할 수 있다.

############# 실습 time!!!

# trial을 다양하게 바꾸어가며 성능을 체크해보자!!
set.seed(123)
another_model <- C5.0(credit_c50_train[,-24], credit_c50_train$default.payment.next.month, trials=[ ])
calc_AUC(another_model, credit_c50_test, credit_c50_test$default.payment.next.month)

#그렇다면 trials를 일정량 늘려 AUC가 trials의 변화에 따라 어떻게 달라지는지 확인하기 위한 그래프를 그리는 함수를 정의해보자

# 시각화를 통한 최적의 trials 갯수 확인해보기
get_c50_auc <- function(train,target_tr,test,target_te){
  auc_train <- c()
  auc_test <- c()
  for(i in 1:30){
    model <- C5.0(train,target_tr,trials=i)
    prob1 <- predict(model, newdata=train, type='prob')[,2]
    prob2 <- predict(model, newdata=test, type='prob')[,2]
    auc_tr <- performance(prediction(prob1,target_tr),'auc')
    auc_te <- performance(prediction(prob2,target_te),'auc')
    auc_train <- c(auc_train, auc_tr@y.values[[1]])
    auc_test <- c(auc_test, auc_te@y.values[[1]])
  }
  df = data.frame(trials=1:30, auc_train = auc_train, auc_test = auc_test)
  library(tidyr)
  library(dplyr)
  df %>% gather(key=type,value=score,-trials) %>% ggplot(aes(x=trials,y=score,color=type)) + geom_line()
}

# 시각화를 해보자, 여기는 결과값만 그냥 보여드리는 걸로
get_c50_auc(credit_c50_train[,-24],
            credit_bg1$default.payment.next.month, 
            credit_c50_test[,-24], 
            credit_bg2$default.payment.next.month)

# 가장 성능이 좋았던 경우를 모델에 적용해보자
set.seed(123)
best_model <-  C5.0(credit_c50_train[,-24], credit_c50_train$default.payment.next.month, trials = 28)

calc_AUC(best_model, credit_c50_train, credit_c50_train$default.payment.next.month)
calc_AUC(best_model, credit_c50_test, credit_c50_test$default.payment.next.month)
##### <xgboost>
#nrounds = 최대 반복수

#eta : learning rate default=0.3 [0,1]

#Step size shrinkage로 학습 단계별로 가중치를 얼마나 적용할 지 결정하는 숫자, 가중치이므로 0~1 사이의 값을 지니며, 낮을 수록 more conservative

#gamma : min split loss, default=0, [0,] gamma가 커질수록 보수적인 모델
#information gain은 가지치기를 할 때 타겟변수에 대해 얼마나 설명할 수 있는 지표인데 이에 패널티를 부여하는 숫자가 바로 감마

#gamma가 크다는 것은 가지치기를 잘 만들려 하지 않음을 의미하게 됨.

#max_depth, default=6, [0,] 말 그대로 의사결정나무의 깊이의 한도
#깊이가 깊을수록 모델은 더 복잡해지고 overfitting의 문제를 야기할 수 있음

#subsample : default=1, (0,1] training 데이터셋에서 subset를 만들지 전부를 사용할 지를 정하는 파라미터, 매번 나무를 만들때 적용하며 overfitting문제를 방지하려고 사용
#각각의 트리를 만들때 데이터에서 사용할 행(row)의 비율(0~1)로도 볼 수 있다.

#colsample_bytree : default=1, (0,1]
#나무를 만들때 칼럼, 즉 변수를 샘플링해서 쓸지에 대한 파라미터

#나무를 만들기 전에 한 번 샘플링을 하게 됨

#colsample_bytree : 각각의 트리를 만들때 데이터에서 사용할 열의 비울(0~1)로도 볼 수 있다.
library(xgboost)
str(credit_bg1)
# 현재 데이터에는 특별히 factor형 변수가 없지만 있는 경우는
# 반드시 one - hot - encoding 과정을 거쳐야 함을 명심!!! ---- 이를 꼭 언급하면 좋을듯
# data.matrix() 함수를 사용할 경우 이를 자동으로 처리해줌!

library(dplyr)
# 데이터를 매트릭스 형태로 바꾸어줌
x_credit <- credit_bg1 %>% select(-default.payment.next.month) %>% data.matrix
y_credit <- credit_bg1$default.payment.next.month

x_credit2 <- credit_bg2 %>% select(-default.payment.next.month) %>% data.matrix
y_credit2 <- credit_bg2$default.payment.next.month

## y데이터는 xgboost는 0부터 시작하는 숫자 형태로 받아들이기 때문에 다음과 같은 전처리를 요한다.
head(as.numeric(y_credit),10)
set.seed(2020)
xg_credit <- xgboost(data=x_credit, label= as.numeric(y_credit)-1, num_class=2, nfold=5, nrounds=200,objective = "multi:softprob",
                     eval_metric = 'mlogloss', eta=0.001, max_depth=5, gamma=3, prediction=T, verbose=F)

?xgboost # 파라미터에 대한 보충설명이 필요할 경우
xg_credit

# 예측한 결과 값을 한번 확인해보면 다음과 같은 특징을 갖는다.
pred_credit3 <- predict(xg_credit, newdata=x_credit2)
# 본디 테스트 데이터의 수인 5000개가 나와야하는데
nrow(credit_bg2)
# 예측 결과는 2배인 10000이 나온다
length(pred_credit3)
# num_class가 2이므로

head(pred_credit3, 10)
sum(pred_credit3[1], pred_credit3[2])
sum(pred_credit3[3], pred_credit3[4])

# 1로 예측한 경우의 확률만 가져오는 함수
get_pred <- function(vec){
  pred <- c()
  for(i in 1:length(vec)){
    if((i%%2)==0){
      pred <- c(pred, vec[i])
    }
  }
  return(pred)
}

pred_credit3 <- get_pred(pred_credit3)
pred_credit3
# 0 과 1인 경우의 확률값의 평균은 다음과 같다.
tapply(pred_credit3, y_credit2, mean)

# 0.46을 대강의 threshold로 잡고 AUC를 구해본다.
pred_logic <- ifelse(pred_credit3>0.46,1,0)
table(pred=pred_logic, actual=y_credit2)

pred_xg <- prediction(pred_credit3, y_credit2)
auc_xg <- performance(pred_xg, 'auc')
auc_xg@y.values[[1]]

########### 실습 time!! ########

## nround, eta, max_depth, gamma값을 바꾸어가며 모델을 돌려보자!!
set.seed(2020)
another_xg <- xgboost(data=x_credit, label= as.numeric(y_credit)-1, num_class=2, nrounds=[ ],
                      objective = "multi:softprob",eval_metric = 'mlogloss', eta=[ ], max_depth=[ ], 
                      gamma=[ ], prediction=T, verbose=F)
pred_credit5 <- predict(another_xg, newdata=x_credit2)
pred_credit5 <- get_pred(pred_credit5)
pred_xg2 <- prediction(pred_credit5, y_credit2)
auc_xg2 <- performance(pred_xg2, 'auc')
auc_xg2@y.values[[1]]

##################################

# nrounds가 변할 때마다 에러값이 어떻게 변하는 지를 체크하는 함수
cvplot = function(model){
  eval.log = model$evaluation_log
  data.frame(error=c(unlist(eval.log[,2])),
             class = c(rep('train',nrow(eval.log))),
             nround=c(rep(1:nrow(eval.log),2))
  ) %>% ggplot(aes(nround,error,col=class))+geom_point(alpha=0.2)+geom_smooth(alpha=0.4, se=F)+theme_bw()
}

## 그리드 탐색의 인자를 다음과 같이 설정하고 탐색을 수행해본다. 시간이 다소 오래걸리니 이는 직접 시행을 하지는 않도록 한다.
library(caret)
grid <- expand.grid(nrounds=400,eta=c(0.05,0.07,0.1), max_depth=c(3,5,7,10), gamma=c(0,1,3), colsample_bytree=1, subsample=1, min_child_weight=1)

trcontrol <- trainControl(method='cv', number=5)

xgb_hypter_tuning <- train(x=x_credit, y=y_credit, trControl=trcontrol, tuneGrid=grid, method='xgbTree')

set.seed(2020)
xg_credit_better <- xgboost(data=x_credit, label= as.numeric(y_credit)-1, num_class = levels(y_credit)%>%length, nfold=5, nrounds=400,objective = "multi:softprob",
                     eval_metric = 'mlogloss', eta=0.07, max_depth=10, gamma=3, prediction=T, verbose=F)

pred_credit4 <- predict(xg_credit_better, newdata=x_credit2)

pred_credit4 <- get_pred(pred_credit4)

pred_xg_better <- prediction(pred_credit4, y_credit2)
auc_xg_better <- performance(pred_xg_better, 'auc')
auc_xg_better@y.values[[1]]

## xgb.DMatrix 자료구조를 이용해서도 모델링을 만들 수 있음
## 보통 이것을 더 애용한다...!

xgb_train <- xgb.DMatrix(data=x_credit, label=y_credit)
xgb_test <- xgb.DMatrix(data=x_credit2, label=y_credit2)

set.seed(2020)
xgbc <- xgboost(data=xgb_train, nrounds=400,max_depth=10,eta=0.07,gamma=3, prediction=T)

pred_m <- predict(xgbc, newdata=xgb_test)

p_m <- prediction(pred_m, y_credit2)
auc_m <- performance(p_m, 'auc')
auc_m@y.values[[1]]



#### < Regression >
temperature_train
temperature_test

str(temperature_train)
str(temperature_test)

temp_idx <- createDataPartition(temperature_train$Y18, p=.8, list=F)
temp_tr <- temperature_train[temp_idx, ]
temp_te <- temperature_train[-temp_idx, ]

str(temp_tr)
str(temp_te)

# id항목 아예 제거
temp_tr <- temp_tr[-1]
temp_te <- temp_te[-1]

# RandomForest

set.seed(2020)
rf_temp <- randomForest(Y18~., data=temp_tr)

pred_temp2 <- predict(rf_temp, temp_te)

calcRMSE <- function(label, estimation){
  return(sqrt(mean((label-estimation)**2)))
}


calcRMSE(temp_te$Y18, pred_temp2)

# ntree의 갯수를 조금 줄여서 실행해보자
set.seed(2020)
rf_temp2 <- randomForest(Y18~., data=temp_tr, ntree=300)

pred_temp4 <- predict(rf_temp2, temp_te)

calcRMSE(temp_te$Y18, pred_temp4)

# 마찬가지로 hyper parameter tuning을 해보면 어떻게 될까...?
fitContrl2 <- trainControl(method='cv', number=3, search='grid')
grids2 <- expand.grid(.mtry=c(10:15))
modellists2 <- list()
for (ntrees in c(100,300,500,700)){
  set.seed(2020)
  rf_fit2 <- train(Y18~., data=temp_tr, method='rf', metric = 'RMSE', tuneGrid = grids2, trControl=fitContrl2,ntree=ntrees,verbose=T)
  key2 <- toString(ntrees)
  modellists2[[key2]] <- rf_fit2
}

modellists2

set.seed(2020)
rf_temp3 <- randomForest(Y18~., data=temp_tr, mtry=13, ntree=700)

pred_temp6 <- predict(rf_temp3, temp_te)

calcRMSE(temp_te$Y18, pred_temp6)

# RandomForest 서버 제출용 하나 만들어보자
head(sub_temp,10)
head(temperature_test[c("id")],10)
rf_temp_sub <- randomForest(Y18~., data=temperature_train, mtry=13, ntree=700)

pred_temp_sub_rf <- predict(rf_temp_sub, temperature_test)

sub_temp$Y18 <- pred_temp_sub_rf

write.csv(sub_temp, file="submission_temp_rf.csv", row.names = F)


# Xgboost
colnames(temp_tr)
temp_tr_mtx <- data.matrix(temp_tr[-41])
temp_tr_mty <- data.matrix(temp_tr$Y18)
temp_te_mtx <- data.matrix(temp_te[-41])
temp_te_mty <- data.matrix(temp_te$Y18)

train_data <- xgb.DMatrix(data=temp_tr_mtx, label=temp_tr_mty)
set.seed(2020)
xg_temp <- xgboost(data=train_data, nrounds=500, eta=0.05, max_depth=20, gamma=3, prediction=T, verbose=F)

pred_temps <- predict(xg_temp, newdata=temp_te_mtx)


calcRMSE(temp_te_mty, pred_temps)

# nrounds에 따른..
cvplot(xg_temp)

best_param = list()
best_rmse = Inf
best_rmse_index = 0
for (iter in 1:10){
  params <- list(objective = "reg:linear",
                 eval_metric = "rmse",
                 max_depth=20,
                 eta=runif(1,0.01,0.3),
                 gamma=sample(1:4,1))
  set.seed(2020)
  xgbcv <- xgb.cv(data=train_data,params=params,nfold=5,nrounds=500,early_stopping_rounds = 500,verbose=F)
  min_rmse <- min(xgbcv[[4]]$test_rmse_mean)
  min_rmse_index <- which.min(xgbcv[[4]]$test_rmse_mean)
  
  if(min_rmse < best_rmse){
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_param = params
  }
  
}

best_param
set.seed(2020)
xg_temp2 <- xgboost(data=train_data, params=best_param, nrounds=500, verbose=F)

pred_temps2 <- predict(xg_temp2, temp_te_mtx)

calcRMSE(temp_te_mty, pred_temps2) 


## xgboost로 temperature_train 훈련 데이터 모델링을 돌린 뒤 temperature_test에 맞게 제출을 해보자
## 실습!!

