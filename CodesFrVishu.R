rm(list=ls())

setwd("C:/Users/dipanjand/Desktop/Projects/11.08.2016/")

# Load the dividend map data
Div.Map = read.csv("Dividend Map.csv")

# 75176 obs of 3 variables

trim = function (x) gsub("^\\s+|\\s+$", "", x)

dataClean.fun = function(doc){
        doc = as.character(doc)
        doc = tolower(doc)
        doc = gsub("[^[:alnum:]]"," ",doc)
        doc = gsub(" ","",doc)
        doc = trim(doc)
        doc
}

Div.Map$Myscheme_name = dataClean.fun(Div.Map$scheme_name)

Div.Map$Myshort_name = dataClean.fun(Div.Map$short_name)

library(RecordLinkage)

Div.Map$Linkage = levenshteinSim(
                                Div.Map$Myscheme_name,
                                Div.Map$Myshort_name
                                )
                                      

100*(sum(Div.Map$Linkage==1,na.rm=T)/nrow(Div.Map))



# write.csv(Div.Map,"DividendwithLinkage.csv")
