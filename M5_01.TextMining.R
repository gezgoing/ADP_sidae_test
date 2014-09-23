#=================#
# 01. Text Mining #
#=================#

###################################
# 3. tm 패키지를 이용한 분석 사례 #
###################################

## (2) VCorpus로 문서 읽기

# (a) 문서 위치 지정
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
reut21578 <- system.file("texts", "crude", package = "tm")
reut21578

# (b) 문서 읽기
crudeCorp<-VCorpus(DirSource(reut21578), list(reader = readReut21578XMLasPlain))
crudeCorp
crudeCorp[[1]]
inspect(crudeCorp[2])

# (c) 문서 변형

# Convert to Lower Case
crudeCorp <- tm_map(crudeCorp, content_transformer(tolower))
inspect(crudeCorp[1])

# Remove Stopwords
crudeCorp <- tm_map(crudeCorp, removeWords, stopwords("english"))
inspect(crudeCorp[1])

# Stemming
crudeCorp<-tm_map(crudeCorp, stemDocument, language = "english")
inspect(crudeCorp[1])

# Eliminating Extra Whitespace
crudeCorp <- tm_map(crudeCorp, stripWhitespace)
inspect(crudeCorp[1])

# Make sure all of your data is in PlainTextDocument 
crudeCorp <- tm_map(crudeCorp, PlainTextDocument)
# the functions tolower and trim won't necessarily return TextDocuments. Instead they return characters and the DocumentTermMatrix isn't sure how to handle a corpus of characters.
inspect(crudeCorp[c(1,2)])

# (d) Creating Term-Document Matrices
crudeDtm <- DocumentTermMatrix(crudeCorp) # Error Occured
inspect(crudeDtm[5:10, 650:653])

# (e) Operations on Term-Document Matrices
findFreqTerms(crudeDtm, 10)
findAssocs(crudeDtm, "opec", 0.8)

inspect(removeSparseTerms(crudeDtm, 0.5))

# (f) Dictionary
crudeDctnry = c("prices", "crude", "oil")
inspect(DocumentTermMatrix(crudeCorp,list(dictionary = crudeDctnry)))


#######################
# 4. wordcloud 패키지 #
#######################

## (1) gdata 패키지 설치
install.packages("gdata")
library(gdata)

## (2) Remove Punctuation
crudeCorp <- tm_map(crudeCorp, removePunctuation)
inspect(crudeCorp[1])

## (3) Eliminating Numbers
crudeCorp <- tm_map(crudeCorp, removeNumbers)
inspect(crudeCorp[1])

## (4) Eliminating Extra Whitespace
crudeCorp <- tm_map(crudeCorp, stripWhitespace)
inspect(crudeCorp[1])

## (5) wordcloud 생성

# (a) 데이터 변환
crudeDf<-as.data.frame(trim(unlist(strsplit(as.character(crudeCorp),' '))))
crudeWord<-table(crudeDf)
head(crudeWord,10)

# (b) wordcloud 생성
library(wordcloud)
wordcloud(names(crudeWord), min.freq=3, as.numeric(crudeWord), colors= c('gray','blue'))

ind<-grep("=",names(crudeWord));crudeWord[ind]
crudeWord<-crudeWord[-ind]
ind<-grep("character",names(crudeWord));crudeWord[ind]
crudeWord<-crudeWord[-ind]
ind<-grep("said",names(crudeWord));crudeWord[ind]
crudeWord<-crudeWord[-ind]
ind<-grep("\\d+",names(crudeWord));crudeWord[ind]
crudeWord<-crudeWord[-ind]
ind<-grep("list",names(crudeWord));crudeWord[ind]
crudeWord<-crudeWord[-ind]

wordcloud(names(crudeWord), min.freq=3, as.numeric(crudeWord), colors= c('gray','blue'))

##################################################
# 5. twitteR 패키지와 Twitter를 이용한 감성 분석 #
##################################################

## (1) Twitter자료 가져오기
# Twitter app을 먼저 생성하고 아래 실행 

# (e) R script 실행하여 인증서 다운로드
library(twitteR)
library(RColorBrewer)

url_rqst<-"https://api.twitter.com/oauth/request_token"
url_acc<-"https://api.twitter.com/oauth/access_token"
url_auth<-"https://api.twitter.com/oauth/authorize"

API_key<-"----------------" # input your API key
API_secret<-"-----------------------" # input your API secret

twitCred<-OAuthFactory$new(consumerKey=API_key,consumerSecret=API_secret,requestURL=url_rqst,accessURL=url_acc,authURL=url_auth)
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

# (f) 생성된 인증서로 Twitter 자료를 다운받아 저장
twitCred$handshake(cainfo="cacert.pem")

save(list="twitCred",file="twitter_credentials")

load("twitter_credentials")
registerTwitterOAuth(twitCred)

apple_tweets<-searchTwitter("@applenws",n=500,cainfo="cacert.pem")
head(apple_tweets)
length(apple_tweets)

ibm_tweets<-searchTwitter("@ibm",n=500,cainfo="cacert.pem")
head(ibm_tweets)
length(ibm_tweets)

ms_tweets<-searchTwitter("@microsoft",n=500,cainfo="cacert.pem")
head(ms_tweets)
length(ms_tweets)

# (g) list를 character로 변환
library(plyr)
apple_text<-laply(apple_tweets,function(t)t$getText())
str(apple_text)
head(apple_text,3)

# (h) 긍부정 단어 사전 불러오기(단어 추가 가능)
pos.word=scan("positive-words.txt",what="character",comment.char=";")
neg.word=scan("negative-words.txt",what="character",comment.char=";")

pos.words<-c(pos.word,"ripper","speed")
neg.words<-c(neg.word,"small","narrow")

# (i) 긍부정 점수화 함수 정의
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence) # for english
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# (j) UTF-8로 저장된 문장 제외(영어가 아닌 문장)
head(apple_text,5)
Encoding(apple_text)[1:10]

apple_text<-apple_text[!Encoding(apple_text)=="UTF-8"]
head(apple_text,4)
apple_text[[10]]

# (k) 기 정의된 함수를 이용한 점수 계산 및 히스토그램 출력
apple_scores=score.sentiment(apple_text,pos.words,neg.words,.progress='text')
hist(apple_scores$score)

# (l) 주의사항
sample=c("You're awesome and Iloveyou","I hate and hate and hate. So angry. Die!","Impressedandamazed:youarepeerlessinyourachievementofunparalleledmediocrity.","I love you")
result<-score.sentiment(sample,pos.words,neg.words)
result$score
