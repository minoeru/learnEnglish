library(shiny)

ui <- fluidPage(
  titlePanel("Change"),
  fileInput("file", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
  actionButton("start","Make")
)

server <- function(input, output) {
  df <- NULL
  observeEvent(input$file, {
    df <<- read.csv(input$file$datapath)
    df_original <<- as.character(df$original)
  })
  
  observeEvent(input$start,{
    # print(df_original)
    change()
    print(word3)
    print(sentence6)
  })
    
  #不必要なの削除
  change <- function(){
    print(df_original)
    df_original_ <<- df_original[-which(df_original %in% c("Level: AWL"))]
    #使用対象によって変える
    df_original_ <<- df_original_[-which(df_original_ %in% c("CELESE AWL Sublist 1a page 1 of 5"))]
    df_original_ <<- df_original_[-which(df_original_ %in% c("CELESE AWL Sublist 1a page 2 of 5"))]
    df_original_ <<- df_original_[-which(df_original_ %in% c("CELESE AWL Sublist 1a page 3 of 5"))]
    df_original_ <<- df_original_[-which(df_original_ %in% c("CELESE AWL Sublist 1a page 4 of 5"))]
    df_original_ <<- df_original_[-which(df_original_ %in% c("CELESE AWL Sublist 1a page 5 of 5"))]
    
    print(df_original_)
    
    word <- sapply(1:length(df_original_), function(x) if(regexpr(": ", df_original_[x]) == 2 || regexpr(": ", df_original_[x]) == 3) df_original_[x])
    word2 <- unlist(word)
    word3 <<- sapply(1:length(word2), function(x){
      if(x < 10) substr(word2[x], 4, regexpr("\\(", word2[x]) - 2)
      else substr(word2[x], 5, regexpr("\\(", word2[x]) - 2)
    })
    sentence <- sapply(1:length(df_original_), function(x) if(regexpr(": ", df_original_[x]) != 2 && regexpr(": ", df_original_[x]) != 3) df_original_[x])
    sentence2 <- unlist(sentence)
    
    sentence2 <<- sentence2[-which(sentence2 %in% c("Level: AWL"))]
    
    # regexpr(": ", sentence2)
    
    
    sentence3 <- sapply(1:length(sentence2), function(x){
                          if(x != length(sentence2)){
                            if(regexpr(": ", sentence2[x + 1]) == -1 && regexpr("\\. ", sentence2[x + 1]) != 2) paste(sentence2[x] , sentence2[x + 1], sep = "")
                            else if(regexpr(": ", sentence2[x]) == -1 && regexpr("\\. ", sentence2[x + 1] )!= 2) NULL
                            else sentence2[x]
                          }
                          else sentence2[x]
                        })
    sentence3 <- unlist(sentence3)
    
    
    
    
    sentence4 <- sapply(1:length(sentence3), function(x)if(regexpr(": ", sentence3[x]) != -1 || regexpr("\\. ", sentence2[x]) == 2) sentence3[x])
    sentence4 <- unlist(sentence4)
    
   
    
    sentence5 <- sapply(1:length(sentence4),function(x){
      substr(sentence4[x],4, nchar(sentence4[x]))
      })
    
    sentence6 <<- sapply(1:length(sentence5),function(x){
      substr(sentence5[x],regexpr(":", sentence5[x]) +2, nchar(sentence5[x])) 
    })
    
  }

}

shinyApp(ui = ui, server = server)
