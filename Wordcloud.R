rm(list=ls())

library(tm)

library(stringi)

library(proxy)

library(wordcloud)

wiki = "http://en.wikipedia.org/wiki/"

titles = c("United_Kingdom_European_Union_membership_referendum,_2016")

articles = character(length(titles))

for (i in 1:length(titles)) {
        articles[i] = stri_flatten(readLines(stri_paste(wiki, titles[i])), col = " ")
}

docs = Corpus(VectorSource(articles))

substr(head(docs[[1]],1),1,100000)

docs = tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "))

substr(head(docs[[1]],1),1,1000)

docs = tm_map(docs, function(x) stri_replace_all_fixed(x, "\t", " "))

docs = tm_map(docs, stripWhitespace)

docs = tm_map(docs, removeWords, stopwords("english"))

docs = tm_map(docs, removePunctuation)

docs = tm_map(docs, removeNumbers)

docs = tm_map(docs, tolower)

docs = tm_map(docs, PlainTextDocument)

substr(head(docs[[1]],1),1,1000)

docsTDM = TermDocumentMatrix(docs,control = list(minWordLength = 1))

inspect(docsTDM[100:110,1])

# Frequent Terms and Associations

findFreqTerms(docsTDM, lowfreq=100)

# Wordcloud

m = as.matrix(docsTDM)

# calculate the frequency of words
v = sort(rowSums(m), decreasing=TRUE)

myNames = names(v)

d = data.frame(word=myNames, freq=v)

wordcloud(d$word, d$freq, min.freq=3)
