getwd()

#패키지 설치하기
install.packages("xlsx")
install.packages("ggplot2")

library(xlsx)
library(ggplot2)

#타이타닉 데이터 불러오기
titanic<-read.xlsx("titanic.xlsx", sheetIndex=1, encoding='UTF-8')

#타이타닉 데이터 확인하기
head(titanic)
tail(titanic)
str(titanic)
summary(titanic)

#기본 그래프 그리기
plot(titanic)



#ggplot2 그래프 그리기
#산점도 그리기
ggplot(titanic, aes(x=나이, y=요금, color=성별))+
  geom_point()


#막대 그래프 그리기
ggplot(titanic, aes(factor(생존여부)))+
         geom_bar()
  
ggplot(titanic, aes(factor(생존여부), fill=factor(성별)))+
  geom_bar()

ggplot(titanic, aes(factor(생존여부), fill=factor(성별)))+
  geom_bar() +
  facet_grid(.~등급)

ggplot(titanic, aes(factor(생존여부), fill=factor(성별)))+
  geom_bar(position='fill') +
  facet_grid(.~등급)


#박스플롯 그리기
ggplot(titanic, aes(factor(생존여부), 나이))+
  geom_boxplot() +
  facet_grid(.~등급)
