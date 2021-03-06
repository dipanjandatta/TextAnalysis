
rm(list=ls())

library(tm)

library(stringi)

library(proxy)

wiki = "http://en.wikipedia.org/wiki/"

titles = c("Integral", "Riemann_integral", "Riemann-Stieltjes_integral", "Derivative",
           "Limit_of_a_sequence", "Edvard_Munch", "Vincent_van_Gogh", "Jan_Matejko",
           "Lev_Tolstoj", "Franz_Kafka", "J._R._R._Tolkien")

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

docsTDM = DocumentTermMatrix(docs)

inspect(docsTDM[100:105,6:11])

docsdissim = dist(as.matrix(docsTDM), method = "cosine")

docsdissim2 = as.matrix(docsdissim)

rownames(docsdissim2) = titles

colnames(docsdissim2) = titles

docsdissim2

h = hclust(docsdissim, method = "ward.D")

plot(h, labels = titles, sub = "")
