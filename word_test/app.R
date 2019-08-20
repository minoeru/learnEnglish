library(shiny)

ui <- fluidPage(
  titlePanel("Study English"),
  tabsetPanel(type = "tabs",
              tabPanel("word",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("file_accept"),
                           uiOutput("question"),
                           uiOutput("input1"),
                           uiOutput("button"),
                           uiOutput("count")
                         ),
                         mainPanel(
                           uiOutput("selections")
                         )
                       )
              ),
              tabPanel("simple sentence",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("file_accept2"),
                           uiOutput("input2"),
                           uiOutput("button2"),
                           uiOutput("count2")
                         ),
                         mainPanel(
                           uiOutput("question2"),
                           uiOutput("selections2")
                         )
                       )
              )
  )
)

server <- function(input, output) {
  
  df <- NULL
  observeEvent(input$file, {
    df <<- read.csv(input$file$datapath)
    df_english <<- as.character(df$英語)
    df_japanese <<- as.character(df$日本語)
    df_finished <<- as.character(df$英語)
    deleted_word <<- as.character(df$deleted_word)
    changed_sentence <<- as.character(df$changed_sentence)
    df_finished2 <<- as.character(df$deleted_word)
  })
  
  ##################  単語画面  ##################  
  flag1 <- 0
  
  output$file_accept <- renderUI({
    return(list(
      fileInput("file", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      selectInput("select", label = h6("Number of choices"), choices = list("4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8), selected = 4),
      actionButton("start1","Game Start")
    ))
  })
  
  countNumber <- function(){
    num <<- 0
    lapply(1:length(df_finished), function(x) if(df_finished[x] == "") num <<- num + 1)
    output$count <- renderText({paste0("残り",(length(df_finished) - num),"問です")})
    if(num == length(df_finished)) flag1 <<- 1
  }
  
  createWord <- function(){
    while(1){
      seed <<- as.integer( runif(1, min = 1, max = (length(df_english) + 1) ) )
      if(df_finished[seed] != ""){
        df_finished[seed] <<- ""
        return(df_english[seed])
      }
    }
  }
  
  createSelection <- function(seed){
    rand <- as.integer(runif(100, min = 1, max = (length(df_english) + 1) ) ) 
    data <- c(seed,rand)
    data <- unique(data)
    data <- sapply(1:ans_num, function(x) data[x])
    data <- sort(data)
    return(data)
  }
  
  createData <- function(){
    countNumber()
    ans_num <<- as.numeric(input$select)
    
    if(flag1 == 0){
      output$question <- renderUI({ return(h1(createWord())) })
      output$input1 <- renderUI({ textInput("input_text", label = h3("Answer input"), value = "") })
      output$selections <- renderUI({
        selection <- createSelection(seed)
        return(lapply(1:ans_num,function(x)actionButton("x",df_japanese[selection[x]],width = "100%")))
      })
    }else{
      output$question <- renderUI({})
      output$input1 <- renderUI({})
      output$selections <- renderUI({})
      output$button <- renderUI({})
    }
  }
  
  
  observeEvent(input$start1,{
    output$file_accept <- renderUI({})
    output$button <- renderUI({ actionButton("answer","Answer") })
    createData()
  })
  
  observeEvent(input$answer,{
    if(input$input_text == df_japanese[seed]) createData()
  })
  
  ##################  例文画面   ##################  
  
  flag2 <- 0
  
  countNumber2 <- function(){
    num2 <<- 0
    lapply(1:length(df_finished2),function(x) if(df_finished2[x] == "") num2 <<- num2 + 1)
    output$count2 <- renderText({paste("残り",(length(df_finished2) - num2),"問です")})
    if(num2 == length(df_finished2)) flag2 <<- 1
  }
  
  createWord2 <- function(){
    while(1){
      seed <<- as.integer( runif(1, min = 1, max = (length(changed_sentence) + 1) ) )
      if(df_finished2[seed] != ""){
        df_finished2[seed] <<- ""
        return(changed_sentence[seed])
      }
    }
  }
  
  createSelection2 <- function(seed){
    rand <- as.integer(runif(100, min = 1, max = (length(changed_sentence) + 1) ) )
    data <- c(seed,rand)
    data <- unique(data)
    data <- sapply(1:ans_num2, function(x) data[x])
    data <- sort(data)
    return(data)
  }
  
  createData2 <- function(){
    countNumber2()
    ans_num2 <<- as.numeric(input$select2)
    if(flag2 == 0){
      output$question2 <- renderUI({ return(h1(createWord2())) })
      output$input2 <- renderUI({ textInput("input_text2", label = h3("Answer input"), value = "") })
      output$selections2 <- renderUI({
        selection <- createSelection2(seed)
        return(lapply(1:ans_num2, function(x) actionButton("x",deleted_word[selection[x]],width = "100%")))
      })
    }
    else{
      output$question2 <- renderUI({})
      output$input1 <- renderUI({})
      output$selections2 <- renderUI({})
      output$button2 <- renderUI({})
    }
  }
  
  output$file_accept2 <- renderUI({
    return(list(
      fileInput("file", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      selectInput("select2", label = h6("Number of choices"), choices = list("4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8), selected = 4),
      actionButton("start2","Game Start")
    ))
  })
  
  observeEvent(input$start2,{
    output$file_accept2 <- renderUI({})
    output$button2 <- renderUI({
      actionButton("answer2","Answer")
    })
    createData2()
  })
  
  observeEvent(input$answer2,{
    if(input$input_text2 == deleted_word[seed]) createData2()
  })
}
shinyApp(ui = ui, server = server)