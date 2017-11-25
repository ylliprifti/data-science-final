
library(shiny)
library(stringr)

source("r-source-copy/params.R")
source("r-source-copy/commonFunctions.R")

#file.copy(from="../../samples/", to="./", 
#          overwrite = TRUE, recursive = TRUE, 
#          copy.mode = TRUE)


local_env <- setEnv(working_dir, file_name, save_from, file_to_disk, sample_directory, blogs_fn, news_fn, twitter_fn, FRACTION_READ, SEED)
local_res <- loadCurrent(local_env)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
   output$predicted <-  renderPrint({ 
     
         cur_term = NA
         predictCurrent <- completeCurrent(input$inputString, local_res$lines)
         if(!identical(predictCurrent, 'Not Found')){
           get_term <- unlist(str_split(predictCurrent$term, " "))
           cur_term <- get_term[length(get_term)]
         }
         predictedNext <- findNext(input$inputString, local_res$lines, cur_term)
         
         final <- ""
         
         if(!identical(predictCurrent, 'Not Found')){
            #str_pad(x, width=8, side="right")
            tmp <- paste("NGram:", predictCurrent$ngram, " Freq:", predictCurrent$support, " |    ->")
            tmp <- str_pad(tmp, width = 35, side="right")
            final <- paste(tmp, " <b style='color:red'>" ,predictCurrent$term, "</b><br />")
         
        }
         else final <- paste("[Empty]", "<br /><br />")
         if(!identical(predictedNext, 'Not Found')){
          
           tmp <- paste("NGram:", predictedNext$ngram, " Freq:", predictedNext$support, " |    ->")
           tmp <- str_pad(tmp, width = 35, side="right")
           
            final <- paste(final, tmp," <b style='color:red'>" ,predictedNext$term, "</b><br />")
         }
         else final <- paste(final, "[Empty]", "<br /><br />")
         
         cat(final)
        
    })
   
   
   output$text1 <- renderText({input$inputString})
})
