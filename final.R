rm(list=ls())
setwd("~/Dev/R/Capstone/data-science-final/")

source("r-source/params.R")
source("r-source/libraries.R")
source("r-source/commonFunctions.R")
source("r-source/nlpFunctions.R")

loadCpp()

env <- setEnv(working_dir, file_name, save_from, file_to_disk, sample_directory, blogs_fn, news_fn, twitter_fn, FRACTION_READ, SEED)
getFile(env)

result <- {}

result$totalLines <- totalLines(env)

result$lines <- readRandomInternal(env, totalLines = result$totalLines)

result$lines$all <- c(result$lines$twitter$ReadLines, result$lines$blog$ReadLines, result$lines$news$ReadLines)

result$lines$clean <- cleanData(result$lines$all, env)

result <- makeNGrams(result)

saveCurrent(result, env)

#pal=brewer.pal(8,"Blues")
#pal=pal[-(1:4)]
#wordcloud(result$lines$adm_4$term,result$lines$adm_4$count,max.words=100,random.order = F, colors=pal)
