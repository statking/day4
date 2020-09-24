getwd()
par(family="Malgun Gothic")

install.packages("xlsx")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("qgraph")

library(xlsx)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(qgraph)

#파일 불러오기(엑셀 데이터)
news<-read.csv("news.csv")
news

#변수명'키워드' 추출하기
keyword <-news$키워드

#corpus 데이터 만들기
corpus <- VCorpus(VectorSource(keyword))

#corpus 내용 확인
corpus[[1]]$content

##### 전처리 #####
#숫자 모두 삭제
corpus.pre <- tm_map(corpus, removeNumbers)


#특수기호 처리를 위한 함수
function.substitute <- function(obj, old, new){
  sub.obj<-tm_map(obj, content_transformer(function(x, pattern) gsub(pattern, new, x)), old)
  sub.obj
}
#특수기호 처리
corpus.pre<-function.substitute(corpus.pre, "[[:lower:]]","")
corpus.pre<-function.substitute(corpus.pre, "[[:upper:]]","")
corpus.pre<-function.substitute(corpus.pre, ","," ")
corpus.pre<-function.substitute(corpus.pre, "\\(","")
corpus.pre<-function.substitute(corpus.pre, "\\)","")
corpus.pre<-function.substitute(corpus.pre, "'","")
corpus.pre<-function.substitute(corpus.pre, "&","")
corpus.pre<-function.substitute(corpus.pre, "・","")
corpus.pre<-function.substitute(corpus.pre, "㈜","")
corpus.pre<-function.substitute(corpus.pre, "들","")

#특수문자 제거
corpus.pre<-tm_map(corpus.pre, removePunctuation)

#공백 처리
corpus.pre<-tm_map(corpus.pre, stripWhitespace)

corpus.pre[[1]]$content



############################# 
# 단어 빈도분석과 Wordcloud #
#############################

dtm <- DocumentTermMatrix(corpus.pre, control = list(wordLengths =c(2,10)))

tdm <- t(dtm)
  

#단어 빈도 확인하기
word.freq <- apply(dtm[,], 2, sum) #apply(x, 행/열, 함수), 행:1, 열:2

#단어 높은 빈도순으로 정렬
word.freq.sort <- sort(word.freq, decreasing =TRUE)
word.freq.sort[1:40]

#단어 누적 빈도 구하기
word.freq.sum <-cumsum(word.freq.sort)
word.freq.sum[1:40]

#단어 비율 구하기
word.freq.ratio <- word.freq.sum/word.freq.sum[length(word.freq.sum)]
word.freq.ratio[1:40]

#빈도가 높은 단어 저장하기
write.xlsx(word.freq.sort, file="word_freq_news.xlsx")


##### wordcloud 분석 #####
wordcloud(names(word.freq), freq=word.freq, scale=c(7,0.3), rot.per = 0.1, min.freq = 10, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


v <- head(sort(slam::row_sums(tdm), decreasing = T),100) 
wordcount <- data.frame(X=names(v),freq=v)


wordcloud2(data=wordcount, fontFamily='Malgun Gothic')


#네트워크 분석
tdm_network<-as.matrix(tdm)

word.count_1 <- rowSums(tdm_network)
word.order_1 <- order(word.count_1, decreasing=TRUE)
freq.words_1 <- tdm_network[word.order_1[1:20], ]

co.matrix_1 <- freq.words_1 %*% t(freq.words_1)



#par(family="Apple SD Gothic Neo")
qgraph(co.matrix_1, labels=rownames(co.matrix_1),
       diag=FALSE, layout='spring', threshold=1000,
       edge.color='skyblue', esize=20,       vsize=sqrt(diag(co.matrix_1))*0.1)
