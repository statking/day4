getwd()

#타이타닉 데이터 불러오기
titanic<-read.xlsx("titanic.xlsx", sheetIndex=1, encoding='UTF-8')

install.packages("nnet")
install.packages("caret")
install.packages("e1071")
install.packages("devtools")

install.packages("NeuralNetTools")
library(NeuralNetTools)


library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')


library(nnet)
library(caret)
library(e1071)


#타이타닉 데이터 확인하기
head(titanic)
tail(titanic)
str(titanic)
summary(titanic)


titanic$생존여부 <- as.factor(titanic$생존여부)
str(titanic)


#훈련용 데이터와 검증용 데이터로 나누기
set.seed(1)
samp <- c(sample(1:891, 624))
train <- titanic[samp,]
test <- titanic[-samp,]


#신경망 모형 구축하기
set.seed(2)
model_1 <-nnet(생존여부 ~ ., data=train, size=6, maxit=10000, decay =0.0005, rang=0.1)




plot.nnet(model_1)

garson(model_1)


#검증용 데이터로 테스트하기
pred <- as.factor(predict(model_1, newdata = test, type="class"))

#결과 확인하기
confusionMatrix(data=pred, reference=test$생존여부)


