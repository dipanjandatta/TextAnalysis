#Clear the memory file
rm(list=ls())

Riskyname = "Amtek"
NonRiskyName = "Bosch"
Industry = "AutoParts"

#Set working directory
setwd(paste("C:\\Users\\dipanjand\\Desktop\\Imacs\\",Industry,sep = ""))


# Get a list of files 
Riskyfilelist = list.files(pattern = paste(Riskyname,".*.txt",sep = ""))
NonRiskyfilelist = list.files(pattern = paste(NonRiskyName,".*.txt",sep = ""))
Testfilelist = list.files(pattern = ".*.txt")

## Read data
# For Risky Auto

Risky = character(length(Riskyfilelist))

for (i in 1:length(Riskyfilelist)) {
        Risky[i] = paste(readLines(Riskyfilelist[i]), collapse=" ")
}

# For NonRisky
NonRisky = character(length(NonRiskyfilelist))

for (i in 1:length(NonRiskyfilelist)) {
        NonRisky[i] = paste(readLines(NonRiskyfilelist[i]), collapse=" ")
}

# For testing, both Risky and NonRisky
Test = character(length(Testfilelist))

for (i in 1:length(Testfilelist)) {
        Test[i] = paste(readLines(Testfilelist[i]), collapse=" ")
}

# Load required packages

library(tm)

DocumentCleaning = function(docVector){
        
        # Inputs doc vector, output cleaned corpus
        docs = Corpus(VectorSource(docVector))
        
        docs = tm_map(docs, stripWhitespace)
        
        docs = tm_map(docs, removeWords, stopwords("english"))
        
        docs = tm_map(docs, removePunctuation)
        
        docs = tm_map(docs, removeNumbers)
        
        docs = tm_map(docs, tolower)
        
        docs = tm_map(docs, PlainTextDocument)
        
        docs
                
}

Risky.corpus = DocumentCleaning(Risky)
NonRisky.corpus = DocumentCleaning(NonRisky)
Test.corpus = DocumentCleaning(Test)

Risky.matrix = t(TermDocumentMatrix(Risky.corpus,
                                    control = list(wordLengths=c(4,Inf))))
NonRisky.matrix = t(TermDocumentMatrix(NonRisky.corpus,
                                    control = list(wordLengths=c(4,Inf))))

Test.matrix = t(TermDocumentMatrix(Test.corpus,
                                   control = list(wordLengths=c(4,Inf))))

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

Risky.pMatrix = probabilityMatrix(Risky.matrix)
NonRisky.pMatrix = probabilityMatrix(NonRisky.matrix)

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
Test.matrix = as.matrix(Test.matrix)
# A holder for classification 
classified = NULL

RiskyScore = NULL

NonRiskyScore = NULL

for(j in 1:nrow(Test.matrix))
{
        # Extract the test words
        Test.chars = names(Test.matrix[j,Test.matrix[j,] %in% 1])
        # Get the probabilities
        RiskyScore[j] = getProbability(Test.chars,Risky.pMatrix)
        NonRiskyScore[j] = getProbability(Test.chars,NonRisky.pMatrix)
        # Add it to the classification list
        classified = c(classified,
                       ifelse(RiskyScore[j]>NonRiskyScore[j],"Risky","Non-Risky"))
}

Final_Out = data.frame(Test,classified,
                       "Risk_Score"= round(10-log(-RiskyScore),2),
                       "Safe_Score"= round(10-log(-NonRiskyScore),2)
)

View(Final_Out)


