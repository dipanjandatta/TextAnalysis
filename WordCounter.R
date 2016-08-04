rm(list=ls())

library(pdftools)

setwd("C:\\Users\\dipanjand\\Desktop\\ICRA\\ICRA Text Analytics\\Power")

pdfInput = list.files(pattern = "pdf$")

FileName = NULL
No.of.pages = NULL
Author = NULL
Creator = NULL
Producer = NULL
created = NULL
modified = NULL

for(i in 1:length(pdfInput))
{
        FileName[i]    = substr(pdfInput[i],1,nchar(pdfInput[i])-4)
        
        No.of.pages[i] = pdf_info(pdfInput[i])$pages
        
        Author[i]      = pdf_info(pdfInput[i])$keys$Author
        
        Creator[i]     = pdf_info(pdfInput[i])$keys$Creator
        
        Producer[i]    = pdf_info(pdfInput[i])$keys$Producer
        
        created[i]     = as.character(pdf_info(pdfInput[i])$created)
        
        modified[i]    = as.character(pdf_info(pdfInput[i])$modified)
}

MetaData = data.frame(FileName,No.of.pages,Author,created,modified)

txt = NULL

for(i in 1:length(pdfInput)){
       txt[i] = paste(pdf_text(pdfInput[i]), collapse = " ")        
}

library(stringi)

WrdCntr = function(phrase){
        WordCount = NULL
        
        for(i in 1:length(pdfInput)){
                
                WordCount[i] = sum(str_count(tolower(txt[i]), phrase))
                
        }
        data.frame(phrase,WordCount)
}

DF = as.data.frame(t(data.frame(FileName,No.of.pages,
                                WrdCntr("capacity addition"),
                                WrdCntr("plant capacity"),
                                WrdCntr("generation capacity"),
                                WrdCntr("gas based")
                                )
                        )
                )

for(j in 3:nrow(DF)){
        row.names(DF)[j] = as.character(DF[j-1,1])        
}

DF = DF[-seq(3,nrow(DF),by = 2),]


