
filteronlythat <- function(mystring,segfilter)
{
  mystringseg = segmentCN(mystring, nature = TRUE)
  myn = names(mystringseg)
  result = mystringseg[myn==segfilter]
  result
}


getmywordcloud <- function(descriptions,segfilter)
{
  vs =VectorSource(descriptions)
  z1 = Corpus(vs,language = "zh")
  
  
  # chinese handle
  library(Rwordseg)
  d.corpus <- tm_map(z1[1:length(z1)], filteronlythat,segfilter)
  #d.corpus <- tm_map(d.corpus,filteronlythat)
  
  d.corpus <- Corpus(VectorSource(d.corpus))
  
  # 取字至少1個字
  z2 = TermDocumentMatrix(d.corpus, control = list(wordLengths = c(1, Inf)))
  z3 = findFreqTerms(z2,lowfreq= 3)
  
  # Term document matrix 
  z5 = as.matrix(z2)
  
  wordFreq = sort(rowSums(z5),decreasing=TRUE)
  set.seed(375)
  
  library(wordcloud)
  Sys.setlocale("LC_CTYPE","zh_CN.utf-8")
  w1 = wordcloud(words=names(wordFreq),freq=wordFreq, min.freq=1,max.words=1000,random.order=F,family="STSong")
  
  w1
}