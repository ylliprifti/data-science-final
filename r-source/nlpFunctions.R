##https://www.rdocumentation.org/packages/tm/versions/0.7-1/topics/tm_map

writeObjectToDisk <- function(obj, env){
  writeLines(obj, paste(env$sample_dir_name, "/", env$sample_fname, sep = ""))
}

cleanData <- function(dataAll, env){

#writeObjectToDisk(dataAll, env)
cl_data <- Corpus(VectorSource(dataAll))
#cl_data <- VCorpus(DirSource(env$sample_dir_name, encoding = "UTF-8"))  
cl_data <- tm_map(cl_data, tolower)
textcp <- tm_map(cl_data, stripWhitespace)
#clean_data <- tm_map(clean_data, PlainTextDocument)
cl_data <- tm_map(cl_data, removePunctuation)
#cl_data <- tm_map(cl_data, removeNumbers)
#cl_data <- tm_map(cl_data, removeWords, stopwords('english'))
#Corpus(VectorSource(cl_data))

cl_data

}


createTdmNgram <- function (textcp, n_min, n_max) {
  currentTokenizer <- function(x) {  RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n_min, max = n_max))}
  tdm_ngram <- TermDocumentMatrix(textcp, control = list(tokenizer = currentTokenizer))
  rownames(tdm_ngram) <- tdm_ngram$term
}

createCorpusNGram <- function(textcp, n){
  temp <- term_stats(textcp, ngrams = n)
  rownames(temp) <- temp$term
  temp
}

makeNGrams <- function(res){
clean <- unlist(res$lines$clean)

res$lines$adm_1 <- createCorpusNGram(clean, 1) #monogram
res$lines$adm_2 <- createCorpusNGram(clean, 2) #bigram
res$lines$adm_3 <- createCorpusNGram(clean, 3) #trigram
res$lines$adm_4 <- createCorpusNGram(clean, 4) #fourgram

res

}
