rm(list=ls())

setwd("C:\\Users\\dipanjand\\Desktop\\Imacs")

library(tm)

# Collect data
tweets.mandrill = read.csv('Mandrill.csv',header=T)
tweets.other = read.csv('Other.csv',header=T)
tweets.test = read.csv('Test.csv',header=T)

tweets.mandrill["class"] = rep("App",nrow(tweets.mandrill))
tweets.other["class"] = rep("Other",nrow(tweets.mandrill))

# Helper Function
replacePunctuation = function(x)
{
        x = tolower(x)
        x = gsub("[.]+[ ]"," ",x)
        x = gsub("[:]+[ ]"," ",x)
        x = gsub("[?]"," ",x)
        x = gsub("[!]"," ",x)
        x = gsub("[;]"," ",x)
        x = gsub("[,]"," ",x)
        x
}

# Do our punctuation stuff
tweets.mandrill$Tweet <- replacePunctuation(tweets.mandrill$Tweet)
tweets.other$Tweet <- replacePunctuation(tweets.other$Tweet)
tweets.test$Tweet <- replacePunctuation(tweets.test$Tweet)

# Create corpus
tweets.mandrill.corpus <- Corpus(VectorSource(as.vector(tweets.mandrill$Tweet)))
tweets.other.corpus <- Corpus(VectorSource(as.vector(tweets.other$Tweet)))
tweets.test.corpus <- Corpus(VectorSource(as.vector(tweets.test$Tweet)))

# Create term document matrix
tweets.mandrill.matrix <- t(TermDocumentMatrix(tweets.mandrill.corpus,control = list(wordLengths=c(4,Inf))));
tweets.other.matrix <- t(TermDocumentMatrix(tweets.other.corpus,control = list(wordLengths=c(4,Inf))));
tweets.test.matrix <- t(TermDocumentMatrix(tweets.test.corpus,control = list(wordLengths=c(4,Inf))));

# Probability Matrix
probabilityMatrix <-function(docMatrix)
{
        # Sum up the term frequencies
        termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
        # Add one
        termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
        # Calculate the probabilties
        termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
        # Calculate the natural log of the probabilities
        termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
        # Add pretty names to the columns
        colnames(termSums)<-c("term","count","additive","probability","lnProbability")
        termSums
}

tweets.mandrill.pMatrix<-probabilityMatrix(tweets.mandrill.matrix)
tweets.other.pMatrix<-probabilityMatrix(tweets.other.matrix)


#Predict

# Get the test matrix
# Get words in the first document

getProbability <- function(testChars,probabilityMatrix)
{
        charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
        # Count how many words were not found in the mandrill matrix
        charactersNotFound<-length(testChars)-length(charactersFound)
        # Add the normalized probabilities for the words founds together
        charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
        # We use ln(1/total smoothed words) for words not found
        charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
        #This is our probability
        prob<-charactersFoundSum+charactersNotFoundSum 
        prob
}

# Get the matrix
tweets.test.matrix<-as.matrix(tweets.test.matrix)

# A holder for classification 
classified<-NULL

for(documentNumber in 1:nrow(tweets.test.matrix))
{
        # Extract the test words
        tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
        # Get the probabilities
        mandrillProbability <- getProbability(tweets.test.chars,tweets.mandrill.pMatrix)
        otherProbability <- getProbability(tweets.test.chars,tweets.other.pMatrix)
        # Add it to the classification list
        classified<-c(classified,ifelse(mandrillProbability>otherProbability,"App","Other"))
}

View(cbind(classified,tweets.test$Tweet))
