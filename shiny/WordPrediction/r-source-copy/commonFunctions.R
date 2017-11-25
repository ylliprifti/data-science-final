setEnv <- function(wd, f_name, f_from, sample_file_name, sample_dir_name, blog_fn, news_fn, twitter_fn, read_fraction, seed){
  
  env <- list()
  env$wd <- wd
  
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

loadCurrent <- function(env){
  res <- {}
  res$lines <- {}
  
  res$lines$adm_4 <- readRDS(paste(env$sample_dir_name, "/adm_4.RDS", sep =""))
  res$lines$adm_3 <- readRDS(paste(env$sample_dir_name, "/adm_3.RDS", sep =""))
  res$lines$adm_2 <- readRDS(paste(env$sample_dir_name, "/adm_2.RDS", sep =""))
  res$lines$adm_1 <- readRDS(paste(env$sample_dir_name, "/adm_1.RDS", sep =""))

  res
}

