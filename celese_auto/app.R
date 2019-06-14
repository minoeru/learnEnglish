library(shiny)

ui <- fluidPage(
  titlePanel("Change"),
  fileInput("file", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
  selectInput("select", label = h6("Number of choices"), choices = list("1a" = 1, "1b" = 2, "2a" = 3, "2b" = 4, "3a" = 5, "3b" = 6, "4a" = 7, "4b" = 8, "5a" = 9, "5b" = 10, "6a" = 11, "6b" = 12, "7a" = 13, "7b" = 14, "8a" = 15, "8b" = 16, "9a" = 17, "9b" = 18), selected = 1),
  actionButton("start","Make")
)

server <- function(input, output) {
  df <- NULL
  observeEvent(input$file, {
    df <<- read.csv(input$file$datapath)
    df_original <<- as.character(df$original)
  })
  
  observeEvent(input$start,{
    change()
    word <<- sapply(1:(length(word3) * 4), function(x){
      word3[((x - 1) / 4) + 1]
    })
    word <<- unlist(word)
    change2()
    data_save()
    
  })
  
  all_words <<- lapply(1:18,function(n){
    sapply(1:5,function(x){
      ifelse(n %% 2 == 1 , sen <- "a" , sen <- "b")
      paste("CELESE AWL Sublist " , ceiling (n / 2)  , sen  , " page ",x," of 5",sep="")
    })
  })
  
  #不必要なの削除
  change <- function(){
    
    ans_num <<- as.numeric(input$select)
    words_select(ans_num)
    df_original_ <<- df_original[-which(df_original %in% c("Level: AWL"))]
    df_original_ <<- df_original_[-which(df_original_ %in% c(selected_words[1]))]
    df_original_ <<- df_original_[-which(df_original_ %in% c(selected_words[2]))]
    df_original_ <<- df_original_[-which(df_original_ %in% c(selected_words[3]))]
    df_original_ <<- df_original_[-which(df_original_ %in% c(selected_words[4]))]
    df_original_ <<- df_original_[-which(df_original_ %in% c(selected_words[5]))]

    # 単語の前の文字削除
    word1 <- sapply(1:length(df_original_), function(x) if(regexpr(": ", df_original_[x]) == 2 || regexpr(": ", df_original_[x]) == 3) df_original_[x])
    word2 <- unlist(word1)
    # 単語の後ろの文字削除
    word3 <<- sapply(1:length(word2), function(x){
      if(x < 10) substr(word2[x], 4, regexpr("\\(", word2[x]) - 2)
      else substr(word2[x], 5, regexpr("\\(", word2[x]) - 2)
    })
    # 単語削除
    sentence1 <- sapply(1:length(df_original_), function(x) if(regexpr(": ", df_original_[x]) != 2 && regexpr(": ", df_original_[x]) != 3) df_original_[x])
    sentence2 <- unlist(sentence1)
    # 邪魔な文字削除
    sentence2 <<- sentence2[-which(sentence2 %in% c("Level: AWL"))]
    # バラバラの文をつなげる
    sentence3 <- sapply(1:length(sentence2), function(x){
      if(x != length(sentence2)){
        if(regexpr(": ", sentence2[x + 1]) == -1 && regexpr("\\. ", sentence2[x + 1]) != 2) paste(sentence2[x] , sentence2[x + 1], sep = "")
        else if(regexpr(": ", sentence2[x]) == -1 && regexpr("\\. ", sentence2[x + 1] )!= 2) NULL
        else sentence2[x]
      }
      else sentence2[x]
    })
    sentence3 <- unlist(sentence3)
    # 文をつなげる
    sentence4 <- sapply(1:length(sentence3), function(x)if(regexpr(": ", sentence3[x]) != -1 || regexpr("\\. ", sentence2[x]) == 2) sentence3[x])
    sentence4 <- unlist(sentence4)
    # 文の番号を外す
    sentence5 <- sapply(1:length(sentence4),function(x){
      substr(sentence4[x],4, nchar(sentence4[x]))
    })
    # 文の前をはずす
    sentence <<- sapply(1:length(sentence5),function(x){
      substr(sentence5[x],regexpr(":", sentence5[x]) +2, nchar(sentence5[x])) 
    })
  }
  
  change2 <- function() {
    
    
    
    # 抜き出し単語の元
    deleted_word <<- (1:length(sentence))
    # 文から抜き出す関数
    changed_sentence <<- sapply(1:length(sentence), function(x){
      num <<- 0
      tmp <<-strsplit(sentence[x]," ")
      tmp <<- unlist(tmp)
      # 元の単語と完全一致
      num <<- match(word[x], tmp, nomatch = 0)
      if(num != 0){
        deleted_word[x] <<- tmp[num]
        deleted_word[x] <<- sub("\\.","",deleted_word[x])
        deleted_word[x] <<- sub(",","",deleted_word[x])
        tmp[num] <<- "_____"
        return( paste(tmp[1:length(tmp)],collapse = " ") ) 
      }
      # 元の単語の後ろ2文字以外一致
      num <- charmatch(substr(word[x],1,nchar(word[x]) - 2), tmp, nomatch = 0)
      if(num != 0){
        deleted_word[x] <<- tmp[num]
        deleted_word[x] <<- sub("\\.","",deleted_word[x])
        deleted_word[x] <<- sub(",","",deleted_word[x])
        tmp[num] <- "_____"
        return( paste(tmp[1:length(tmp)],collapse = " ") )
      }
      # 大文字対応
      tmp_word <<- paste( toupper(substring(word[x],1,1)), substring(word[x],2,nchar(word[x]) ) ,sep = "" )
      num <- charmatch(substr(tmp_word,1,nchar(tmp_word) - 2), tmp, nomatch = 0)
      if(num != 0){
        deleted_word[x] <<- tmp[num]
        deleted_word[x] <<- sub("\\.","",deleted_word[x])
        deleted_word[x] <<- sub(",","",deleted_word[x])
        tmp[num] <<- "_____"
        return( paste(tmp[1:length(tmp)],collapse = " ") ) 
      }
      # un対応
      tmp_word <<- paste("un",word[x],sep = "" )
      num <- charmatch(substr(tmp_word,1,nchar(tmp_word) - 2), tmp, nomatch = 0)
      if(num != 0){
        deleted_word[x] <<- tmp[num]
        deleted_word[x] <<- sub("\\.","",deleted_word[x])
        deleted_word[x] <<- sub(",","",deleted_word[x])
        tmp[num] <<- "_____"
        return( paste(tmp[1:length(tmp)],collapse = " ") ) 
      }
      # in対応
      tmp_word <<- paste("in",word[x],sep = "" )
      num <- charmatch(substr(tmp_word,1,nchar(tmp_word) - 2), tmp, nomatch = 0)
      if(num != 0){
        deleted_word[x] <<- tmp[num]
        deleted_word[x] <<- sub("\\.","",deleted_word[x])
        deleted_word[x] <<- sub(",","",deleted_word[x])
        tmp[num] <<- "_____"
        return( paste(tmp[1:length(tmp)],collapse = " ") ) 
      }
      # re対応
      tmp_word <<- paste("re",word[x],sep = "" )
      num <- charmatch(substr(tmp_word,1,nchar(tmp_word) - 2), tmp, nomatch = 0)
      if(num != 0){
        deleted_word[x] <<- tmp[num]
        deleted_word[x] <<- sub("\\.","",deleted_word[x])
        deleted_word[x] <<- sub(",","",deleted_word[x])
        tmp[num] <<- "_____"
        return( paste(tmp[1:length(tmp)],collapse = " ") ) 
      }
      return( paste(tmp[1:length(tmp)],collapse = " ") )
      
    })
  }
  
  words_select <- function(num){
    selected_words <- all_words[num]
    selected_words <<- unlist(selected_words)
  }
  
  data_save <- function(){
    
    df_data <<- data.frame(
      word,sentence,changed_sentence,deleted_word
    )
    write.csv(df_data,file = "changed_data.csv",row.names = FALSE)
  }
}

shinyApp(ui = ui, server = server)