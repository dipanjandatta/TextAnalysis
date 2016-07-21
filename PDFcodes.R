rm(list=ls())

library(pdftools)

setwd("C:\\Users\\dipanjand\\Desktop\\ICRA\\ICRA Text Analytics\\Power")

list.files(pattern = "pdf$")

pdfInput = list.files(pattern = "pdf$")

FileName = NULL
No_of_pages = NULL
Author = NULL
Creator = NULL
Producer = NULL
created = NULL
modified = NULL

for(i in 1:length(pdfInput))
{

        
        FileName[i]    = substr(pdfInput[i],1,nchar(pdfInput[i])-4)
        
        No_of_pages[i] = pdf_info(pdfInput[i])$pages
        
        Author[i]      = pdf_info(pdfInput[i])$keys$Author
        
        Creator[i]     = pdf_info(pdfInput[i])$keys$Creator
        
        Producer[i]    = pdf_info(pdfInput[i])$keys$Producer
        
        created[i]     = as.character(pdf_info(pdfInput[i])$created)
        
        modified[i]    = as.character(pdf_info(pdfInput[i])$modified)
}

test = data.frame(FileName,No_of_pages,Author,created,modified)

