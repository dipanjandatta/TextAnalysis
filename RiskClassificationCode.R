rm(list=ls())

setwd("C:\\Users\\dipanjand\\Desktop\\Imacs")

Amtekfilelist = list.files(pattern = "Amtek.*.txt")
Boschfilelist = list.files(pattern = "Bosch.*.txt")
Testfilelist = list.files(pattern = ".*.txt")

articles = character(10)

Amtek = character(10)

for (i in 1:10) {
        Amtek[i] = paste(readLines(Amtekfilelist[i]), collapse=" ")
}

Bosch = character(10)

for (i in 1:10) {

        Bosch[i] = paste(readLines(Boschfilelist[i]), collapse=" ")
}

Test = character(20)

for (i in 1:20) {
        
        Test[i] = paste(readLines(Testfilelist[i]), collapse=" ")
}

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
                x = gsub("[)]"," ",x)
                x = gsub("[(]"," ",x)
                x
}

Amtek = replacePunctuation(Amtek)
Bosch = replacePunctuation(Bosch)
Test = replacePunctuation(Test)

library(tm)

Amtek.corpus = Corpus(VectorSource(as.vector(Amtek)))
Bosch.corpus = Corpus(VectorSource(as.vector(Bosch)))
Test.corpus = Corpus(VectorSource(as.vector(Test)))

Amtek.matrix = t(TermDocumentMatrix(Amtek.corpus,
                                        control = list(wordLengths=c(4,Inf))))
Bosch.matrix = t(TermDocumentMatrix(Bosch.corpus,
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

Amtek.pMatrix = probabilityMatrix(Amtek.matrix)
Bosch.pMatrix = probabilityMatrix(Bosch.matrix)

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

AmtekScore = NULL

BoschScore = NULL

for(j in 1:nrow(Test.matrix))
{
        # Extract the test words
        Test.chars = names(Test.matrix[j,Test.matrix[j,] %in% 1])
        # Get the probabilities
        AmtekScore[j] = getProbability(Test.chars,Amtek.pMatrix)
        BoschScore[j] = getProbability(Test.chars,Bosch.pMatrix)
        # Add it to the classification list
        classified = c(classified,
                       ifelse(AmtekScore[j]>BoschScore[j],"Risky","Non-Risky"))
}

Final_Out = data.frame(Test,classified,"Risk_Score"= AmtekScore,"Safe_Score"= BoschScore)


Wordcloud.Fun = function(docsTDM){
        library(wordcloud)
        m = as.matrix(docsTDM)
        
        # calculate the frequency of words
        v = sort(rowSums(m), decreasing=TRUE)
        
        myNames = names(v)
        
        d = data.frame(word=myNames, freq=v)
        
        wordcloud(d$word, d$freq, min.freq=9)        
}

Wordcloud.Fun(Amtek.matrix)

docsTDM = t(Amtek.matrix)
