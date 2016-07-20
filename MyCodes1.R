rm(list=ls())

library(pdftools)
library(tm)

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
# Organize terms by their frequency:

