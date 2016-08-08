rm(list=ls())

setwd("C:\\Users\\dipanjand\\Desktop\\Projects\\08.08.2016")

library(NLP)
library(tm)
library(openNLP)

# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
library(graph)
library(Rgraphviz)

source("WordGraphFunction.R")

# --- MAIN CODE
doc = c("Compatibility of systems of linear constraints over the set of natural numbers. 
         Criteria of compatibility of a system of linear Diophantine equations, strict inequations, 
         and nonstrict inequations are considered. 
         Upper bounds for components of a minimal set of solutions and algorithms of construction of 
         minimal generating sets of solutions for all types of systems are given. 
         These criteria and the corresponding algorithms for constructing a minimal supporting set of solutions 
         can be used in solving all the considered  types systems and systems of mixed types.")

corp = Corpus(VectorSource(doc))

corp = tm_map(corp, stripWhitespace)

corp = tm_map(corp, tolower)

words_with_punctuation = SplitText(as.character(corp[[1]]))

corp = tm_map(corp, removePunctuation)

#--- GRAPH CONSTRUCTION
words = SplitText(as.character(corp[[1]]))

tagged_text = tagPOS(corp[[1]])

tagged_words = SplitText(as.character(tagged_text))

tagged_words = c(SelectTaggedWords(tagged_words,"/NN"),
                 SelectTaggedWords(tagged_words,"/JJ"))  
# keep only NN & JJ tagged words 

tagged_words = RemoveTags(tagged_words)                                                       
# remove un-used tag POS

selected_words = unique(tagged_words)                                                          

text_graph = ConstructTextGraph(2)  
# co-occurrence of window size 2

## Visualize obtained text graph
plot(text_graph, attrs = list(node = 
                                list(fillcolor = "lightblue",fontsize = 20),
                              edge = list(arrowsize=0.5)))


# ---  PAGE RANK
d = 0.85                               # damping factor
threshold = 1e-4               # convergence threshold 
text_nodes = nodes(text_graph)
nodes_num = length(text_nodes)
nodes_rank = matrix(1,nodes_num,2)

k = 0                                  # iterations
convergence_reached = FALSE
repeat {
        for (i in 1:nodes_num) {
                incoming_link = adj(text_graph,text_nodes[i])[[1]]
                incoming_num = length(incoming_link)
                
                tmp = 0
                for (j in 1:incoming_num) {
                        link_num = which(text_nodes==incoming_link[j])
                        outgoing_num = length(adj(text_graph,text_nodes[link_num])[[1]])
                        tmp = tmp + nodes_rank[link_num,1] / outgoing_num
                }
                nodes_rank[i,1] = (1-d)+d*tmp
        }
        k = k+1
        for (i in 1:nodes_num) {
                if (abs(nodes_rank[i,1]-nodes_rank[i,2])<threshold) convergence_reached = TRUE
        }
        if (convergence_reached) break
        nodes_rank[,2] = nodes_rank[,1]
}

# --- POST-PROCESSING
keywords_num = round(nodes_num/3) # a third of the number of vertices in the graph.

ranked_words = data.frame(text_nodes,nodes_rank[,1])

names(ranked_words) = c("word","rank")

strong_words = ranked_words[order(ranked_words$rank,decreasing=TRUE),]
strong_words = as.character(strong_words$word[1:keywords_num])

strong_words

keywords = ""
keywords_scores = 0

for (i in 1:keywords_num) {
        keyword_positions = which(words==strong_words[i])
        for (j in 1:length(keyword_positions)) {
                keyword = ""
                keyword_score = 0
                k = keyword_positions[j]                                       
                repeat {
                        if (IsSelectedWord(words[k])) { 
                                keyword = trim(paste(c(keyword,words[k]),collapse=" "))
                                keyword_score = keyword_score 
                                                + ranked_words[which(ranked_words$word==words[k]),2]
                        }
                        else break                                                    
                        
                        if (IsPunctuated(words_with_punctuation[k])) break
                        if (k==length(words)) break                               
                        k = k+1
                }
                k = keyword_positions[j]-1                                 
                repeat {
                        if (k<1) break
                        
                        if (IsSelectedWord(words[k])) { 
                                keyword = paste(c(words[k],trim(keyword)),collapse=" ")
                                keyword_score = keyword_score 
                                                + ranked_words[which(ranked_words$word==words[k]),2]
                        }
                        else break
                        
                        if (k>1) {            
                                if (IsPunctuated(words_with_punctuation[k-1])) break
                        } 
                        k = k-1
                }
                if (keyword!=strong_words[i]) { 
                        keywords = c(keywords,keyword)
                        keywords_scores = c(keywords_scores,keyword_score)
                }   
        }
}

keywords_df = data.frame(keywords,keywords_scores)
keywords_list = keywords_df[order(keywords_df$keywords_scores,decreasing=TRUE),] 
keywords_list = unique(as.character(keywords_list$keywords[1:nrow(keywords_list)]))  
sort(keywords_list)

keywords_list

