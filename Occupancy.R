library(dplyr)
library(ggplot2)
library(gridExtra)
library(ROCR)
library(rpart)
library(randomForest)
library(caret)

dat=read.csv("C:/Users/LG/Downloads/datatest.txt",strip.white = TRUE)
data2=read.csv("C:/Users/LG/Downloads/datatest2.txt",strip.white = TRUE)
train=read.csv("C:/Users/LG/Downloads/datatraining.txt",strip.white = TRUE)

dat=dat[,-1]
data2=data2[,-1]
train=train[,-1]

# 시각화
tmp=as.data.frame(cor(train[,-6], as.numeric(train$Occupancy)))
tmp=tmp%>%rename(cor=V1)
tmp$var=rownames(tmp)
tmp%>%ggplot(aes(reorder(var,cor), cor))+geom_point()+coord_flip()

#훈련, 검증, 테스트 데이터의 구분
set.seed(1606)
n=nrow(train)
index=1:n
training_index=sample(index,n*0.75)
validation_index=setdiff(index,training_index)
training=train[training_index,]
validation=train[validation_index,]

####### 로지스틱 회귀 분석 모형
# 모형 적합 단계, 즉 학습 단계
logistic=glm(Occupancy~.,data=training,family=binomial)
# 학습 결과 확인하기 
summary(logistic)

# 모형평가를 위한 예측 단계 (검증 집합, 즉 validation set 사용)
y_obs=as.numeric(as.character(validation$Occupancy))
y_hat=predict(logistic,newdata=validation,type='response')
pred_glm=prediction(y_hat,y_obs)
performance(pred_glm,'auc')@y.values[[1]]

####### 나무 모형 
# 모형 적합 단계, 즉 학습 단계
tree=rpart(Occupancy~.,data=training)

# 학습 결과 확인하기 
printcp(tree)
summary(tree)
opar=par(mfrow=c(1,1),xpd=NA)
plot(tree)
text(tree,use.n=TRUE)
par(opar)

# 모형평가를 위한 예측 단계 (검증 집합, 즉 validation set 사용)
y_hat=predict(tree,newdata=validation)
y_hat=y_hat[,'1']
pred_tree=prediction(y_hat,y_obs)
performance(pred_tree,'auc')@y.values[[1]]

# ROC 곡선 그리기
p_glm=performance(pred_glm,'tpr','fpr')
p_tree=performance(pred_tree,'tpr','fpr')
plot(p_glm,col='green', lty='solid', main='ROC 곡선')
plot(p_tree,add=TRUE, col='red',lty='dotted')
abline(0,1)
legend('bottomright',inset=0.1,legend=c('glm','tree'),col=c('green','red'),lty=c('solid','dotted'),lwd=2)

###### 검증 집합으로 모형 형가를 실시한 결과, 모형이 가장 좋은 성능을 보임
###### 따라서 를 최종 모형으로 선정함

###### 테스트집합을 가지 모형을 평가함
# dat
y_obs_test=as.numeric(as.character(dat$Occupancy))
y_hat=predict(logistic,newdata=dat,type='response')
pred=prediction(y_hat,y_obs_test)
performance(pred,'auc')@y.values[[1]]
acc=performance(pred,'acc')
plot(acc,main="분계점(Cutoff)에 따른 정확률 곡선")
roc=performance(pred,'tpr','fpr')
plot(roc,main="ROC 곡선")

# data2
y_obs_test2=as.numeric(as.character(data2$Occupancy))
y_hat2=predict(logistic,newdata=data2,type='response')
pred2=prediction(y_hat2,y_obs_test2)
performance(pred2,'auc')@y.values[[1]]
acc2=performance(pred2,'acc')
plot(acc2,main="분계점(Cutoff)에 따른 정확률 곡선")
roc2=performance(pred2,'tpr','fpr')
plot(roc2,main="ROC 곡선")




set.seed(1607)
forest=randomForest(Occupancy~.,training)

# 학습 결과 확인하기 
forest
summary(forest)
opar=par(mfrow=c(1,2))
plot(forest)
varImpPlot(forest)
par(opar)

# 모형평가를 위한 예측 단계 (검증 집합, 즉 validation set 사용)
y_hat=predict(forest,newdata=validation,type='prob')[,'1']
pred_rf=prediction(y_hat,y_obs)
performance(pred_rf,'auc')@y.values[[1]]
