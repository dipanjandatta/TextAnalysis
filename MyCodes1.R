rm(list=ls())

library(pdftools)
library(tm)
library(ggplot2)
library(wordcloud)   
library(cluster)   
library(fpc)   

setwd("C:\\Users\\dipanjand\\Desktop\\ICRA\\ICRA Text Analytics")

pdfInput = "Power\\CR-2015-Q4-4-ICRA-Power.pdf"

txt = pdf_text(pdfInput)

# first page text
cat(txt[1])

# second page text
cat(txt[2])

# Author, version, etc
info = pdf_info(pdfInput)

# Table with fonts
fonts = pdf_fonts(pdfInput)

# Loading Texts

myCorpus = Corpus(VectorSource(txt))

summary(myCorpus)

inspect(myCorpus[2])

## Preprocessing

# Removing punctuation:
myCorpus1 = tm_map(myCorpus, removePunctuation)   
# inspect(docs[3]) # Check to see if it worked.  

# Removing numbers:
myCorpus1 = tm_map(myCorpus1, removeNumbers)

# Remove special characters.
for(j in seq(myCorpus1))   
{   
        myCorpus1[[j]] = gsub("/", " ", myCorpus1[[j]])   
        myCorpus1[[j]] = gsub("'", " ", myCorpus1[[j]])   
        myCorpus1[[j]] = gsub("\\|", " ", myCorpus1[[j]])   
}  

# Converting to lowercase:
myCorpus1 = tm_map(myCorpus1, tolower)   

# Removing "stopwords" (common words) that usually have no analytic value.
myCorpus1 = tm_map(myCorpus1, removeWords, stopwords("english"))

# Removing particular words:
myCorpus1 = tm_map(myCorpus1, removeWords, c("abracadabra", "giligiligili"))

# Combining words that should stay together
for (j in seq(myCorpus1))
{
        myCorpus1[[j]] = gsub("hello", "hi", myCorpus1[[j]])
        myCorpus1[[j]] = gsub("ello", "hi", myCorpus1[[j]])
        myCorpus1[[j]] = gsub("lo", "hi", myCorpus1[[j]])
        myCorpus1[[j]] = gsub("holla", "hi", myCorpus1[[j]])
}

# Stripping unnecesary whitespace from your documents:
myCorpus1 = tm_map(myCorpus1, stripWhitespace)

# This is the end of the preprocessing stage.
myCorpus1 = tm_map(myCorpus1, PlainTextDocument)

## Stage the Data

# To proceed, create a document term matrix.
dtm = DocumentTermMatrix(myCorpus1)   

dtm   

inspect(dtm[1:5, 1:20])

# Create a transpose of this matrix.
tdm = TermDocumentMatrix(myCorpus1)   

tdm 

inspect(tdm[1:5, 1:20])

# Explore your data
# Check out the frequency of frequencies.

freq = sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14)   

wf = data.frame(word=names(freq), freq=freq)  
head(wf)

# Plot Word Frequencies
# Plot words that appear at least 150 times.

p = ggplot(subset(wf, freq > 150), aes(word, freq))    

p = p + geom_bar(stat="identity")  

p = p + theme(axis.text.x=element_text(angle=45, hjust=1))   

p   

# Relationships Between Terms
# Term Correlations

# specifying a correlation limit of 0.98   

data.frame(findAssocs(dtm,"cost", corlimit=0.6))

data.frame(findAssocs(dtm,"coal", corlimit=0.6))

# wordcloud
set.seed(123)   
dark2 = brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)   

dtms = removeSparseTerms(dtm, 0.5)
dtms

d = dist(t(dtms), method="euclidian")   

fit = hclust(d=d, method="ward")   

fit   

plot.new()

plot(fit, hang=-1)

numclust = 6

groups = cutree(fit, k=numclust)   
# "k=" defines the number of clusters you are using   

rect.hclust(fit, k=numclust, border="red") 
# draw dendogram with red borders around the 6 clusters   

# K-means clustering

d = dist(t(dtms), method="euclidian")   

kfit = kmeans(d, 2)

clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
