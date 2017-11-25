

###############################################################
#' Build and load C-Source for low level file read
#' 
###############################################################

#'Used to Read total number of lines for each file
# system("R CMD SHLIB c-source/readLinesC.c")
# dyn.load("c-source/readLinesC.so")
# readLinesRC <- function(fileName) {
#   res <- .C("getlines",paste(getwd(), "/",fileName, sep=""), result = as.integer(0))  
#   return(res$result)
# }

#'Used to Read random rows from each file
loadCpp <- function(){
  Rcpp::sourceCpp("c-source/readLinesCpp.cpp")
}

readRandomRC <- function(fileName, randomLines) {
  res <- readRandom(fileName, randomLines)
  res
}

readLinesRC <- function(fileName) {
  res <- readTotaLines(paste(getwd(), "/",fileName, sep=""))
  res
}
#dyn.unload("c-source/readLinesCpp.so")

###############################################################
#' Load libraries
#' 
###############################################################


###############################################################
#' Create an @env variable to store all execution parameters
#' 
###############################################################
setEnv <- function(wd, f_name, f_from, sample_file_name, sample_dir_name, blog_fn, news_fn, twitter_fn, read_fraction, seed){

  env <- list()
  env$wd <- wd
  setwd(env$wd)

  env$sample_fname <- sample_file_name
  env$sample_dir_name <- sample_dir_name
  
  env$f_name <- f_name
  env$f_from <- f_from
  
  env$blog_fn <- blog_fn
  env$news_fn <- news_fn
  env$twitter_fn <- twitter_fn
  
  env$READ_FRACTION <- read_fraction
  env$SEED <- seed
  
  set.seed(env$SEED)
  
  env
  
  
}



###############################################################
#' If not downloaded, download and unzip file
#' 
###############################################################
getFile <- function(env){
  
  if (!file.exists(env$f_name) && !file.exists("final")){
    
    download.file(env$f_from, env$f_name)
    
    unzip(env$f_name)
  }
  
}




###############################################################
#' Rando read parts of the file
#' 
###############################################################
readRandomInternal <- function(env, totalLines) {
  
  fileReadRandom <- function(fn, pc, max){
 
      randomLines <- sort(sample(1:max, as.integer(max*pc), replace = F))
      linesRead <- readRandomRC(fn, randomLines)
      result <- list(randomLines, linesRead)
      result
      # result = ""
      # con <- file(fn, open="r")
      # last <- 0
      # for(i in randomLines){
      #   tryCatch({
      #     #temp <- read.table(con,skip=(i-last-1),nrow=1)
      #     temp <- read.csv2(con, skip = (i-last-1), nrows = 1, header = F, skipNul = T)
      #     result = paste(result, temp$V1)
      #   })
      #   last <- i
      # }
      # close(con)
      # return(result)
  }
  
  get_lines_blog = fileReadRandom(env$blog_fn, env$READ_FRACTION, totalLines$blog)
  
  get_lines_twitter = fileReadRandom(env$twitter_fn, env$READ_FRACTION, totalLines$twitter)
  
  get_lines_news = fileReadRandom(env$news_fn, env$READ_FRACTION, totalLines$news)
  
  lines <- list()
  lines$blog <- get_lines_blog
  lines$twitter <- get_lines_twitter
  lines$news <- get_lines_news

  obj_names <- c("RandomLines", "ReadLines")
  names(lines$blog) <- obj_names
  names(lines$twitter) <- obj_names
  names(lines$news) <- obj_names
  
  lines

}  
  
  

totalLines <- function(env) {

  #system.time({
    lines_blog <- readLinesRC(env$blog_fn)
    lines_twitter <- readLinesRC(env$twitter_fn)
    lines_news <- readLinesRC(env$news_fn)
  #})
  
    lines <- list()
    lines$blog <- lines_blog
    lines$twitter <- lines_twitter
    lines$news <- lines_news
    
    lines
    
}



completeCurrent <- function(x, ngrs){
  xx <- unlist(strsplit(x, " "))
  if (length(xx) > 4) xx <- xx[(length(xx)-3): length(xx)]
  reg <- paste('^',paste(xx, collapse = " ", sep=" "), sep='')
  if(length(xx) == 1){
    matches <- grep(reg, ngrs$adm_1$term)[1]
    if(!is.na(matches))
      return(list(term=ngrs$adm_1$term[matches], 
                  count=ngrs$adm_1$count[matches], 
                  support=ngrs$adm_1$support[matches], 
                  ngram=1)
      )
  } 
  if(length(xx) == 2){
    matches <- grep(reg, ngrs$lines$adm_2$term)[1]
    if(!is.na(matches))
      return(list(term=ngrs$adm_2$term[matches], 
                  count=ngrs$adm_2$count[matches], 
                  support=ngrs$adm_2$support[matches],
                  ngram=2)
      )
  }
  if(length(xx) == 3){
    matches <- grep(reg, ngrs$adm_3$term)[1]
    if(!is.na(matches))
      return(list(term=ngrs$adm_3$term[matches], 
                  count=ngrs$adm_3$count[matches], 
                  support=ngrs$adm_3$support[matches],
                  ngram=3)
      )
  }
  if(length(xx) == 4){
    matches <- grep(reg, ngrs$adm_4$term)[1]
    if(!is.na(matches))
      return(list(term=ngrs$adm_4$term[matches], 
                  count=ngrs$adm_4$count[matches], 
                  support=ngrs$adm_4$support[matches], 
                  ngram=4)
      )
  }
  if(length(xx) > 1) {
    xx <- xx[2 : length(xx)]
    return(completeCurrent(xx, ngrs))
  }
  return('Not Found')
}

findNext <- function(n, ngrs, current) {
 xx <- unlist(strsplit(n, " "))
 if(!is.na(current))
  xx[length(xx)] <- current
 if (length(xx) > 3) xx <- xx[(length(xx)-2): length(xx)]
 reg <- paste('^',paste(xx, collapse = " ", sep=" "), sep='')
 
 if(length(xx) == 1){
   matches <- grep(reg, ngrs$adm_2$term)[1]
   if(!is.na(matches))
     return(list(term=ngrs$adm_2$term[matches], 
                 count=ngrs$adm_2$count[matches], 
                 support=ngrs$adm_2$support[matches], 
                 ngram=2)
     )
 } 
 if(length(xx) == 2){
   matches <- grep(reg, ngrs$lines$adm_3$term)[1]
   if(!is.na(matches))
     return(list(term=ngrs$adm_3$term[matches], 
                 count=ngrs$adm_3$count[matches], 
                 support=ngrs$adm_3$support[matches],
                 ngram=3)
     )
 }
 
 if(length(xx) == 3){
   matches <- grep(reg, ngrs$adm_4$term)[1]
   if(!is.na(matches))
     return(list(term=ngrs$adm_4$term[matches], 
                 count=ngrs$adm_4$count[matches], 
                 support=ngrs$adm_4$support[matches],
                 ngram=4)
     )
 }
 
 if(length(xx) > 1) {
   xx <- xx[2 : length(xx)]
   return(findNext(xx, ngrs, current))
 }
 return('Not Found')
 
}

saveCurrent <- function(res, env){
  saveRDS(result$lines$adm_4, paste(env$sample_dir_name, "/adm_4.RDS", sep =""))
  saveRDS(result$lines$adm_3, paste(env$sample_dir_name, "/adm_3.RDS", sep =""))
  saveRDS(result$lines$adm_2, paste(env$sample_dir_name, "/adm_2.RDS", sep =""))
  saveRDS(result$lines$adm_1, paste(env$sample_dir_name, "/adm_1.RDS", sep =""))
}

loadCurrent <- function(env){
  res <- {}
  res$lines <- {}
  
  res$lines$adm_4 <- readRDS(paste(env$sample_dir_name, "/adm_4.RDS", sep =""))
  res$lines$adm_3 <- readRDS(paste(env$sample_dir_name, "/adm_3.RDS", sep =""))
  res$lines$adm_2 <- readRDS(paste(env$sample_dir_name, "/adm_2.RDS", sep =""))
  res$lines$adm_1 <- readRDS(paste(env$sample_dir_name, "/adm_1.RDS", sep =""))

  res
}

