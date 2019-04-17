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
              tabPanel("simple sentence"
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
  })
  
  flag1 <- 0
  question <- ""
  
  output$file_accept <- renderUI({
    return(list(
      fileInput("file", "Choose CSV File",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      selectInput("select", label = h6("Number of choices"), choices = list("4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8), selected = 4),
      actionButton("start1","Game Start")
    ))
  })
  
  count_number <- function(){
    num <<- 0
    for(i in 1:length(df_finished)){
      if(df_finished[i] == ""){
        num <<- num + 1
      }
    }
    output$count <- renderText({
      paste("残り",(length(df_finished) - num),"問です")
    })
    
    if(num == length(df_finished)){
      flag1 <<- 1
    }
  }
  
  create_word <- function(){
    while(1){
      seed <<- as.integer( runif(1, min = 1, max = (length(df_english) + 1) ) )
      if(df_finished[seed] != ""){
        df_finished[seed] <<- ""
        return(df_english[seed])
      }
    }
  }
  
  create_selection <- function(seed){
    rand <- as.integer(runif(100, min = 1, max = (length(df_english) + 1) ) ) 
    data <- c(seed,rand)
    data <- unique(data)
    data <- c(data[1],data[2],data[3],data[4])
    data <- sort(data)
    return(data)
  }
  
  create_data <- function(){
    count_number()
    if(flag1 == 0){
      output$question <- renderUI({
        question <- create_word()
        return(h1(question))
      })
      output$input1 <- renderUI({
        textInput("input_text", label = h3("Answer input"), value = "")
      })
      output$selections <- renderUI({
        selection <- create_selection(seed)
        return(list(
          
          actionButton("1",df_japanese[selection[1]],width = "100%"),
          actionButton("2",df_japanese[selection[2]],width = "100%"),
          actionButton("3",df_japanese[selection[3]],width = "100%"),
          actionButton("4",df_japanese[selection[4]],width = "100%")
        ))
      })
    }
    else{
      output$question <- renderUI({})
      output$input1 <- renderUI({})
      output$selections <- renderUI({})
      output$button <- renderUI({})
    }
  }
  
  check_data <- function(){
    ans <- input$input_text
    if(ans == df_japanese[seed]){
      create_data()
    }
  }
  
  observeEvent(input$start1,{
    output$file_accept <- renderUI({})
    output$button <- renderUI({
      actionButton("answer","Answer")
    })
    create_data()
  })
  
  observeEvent(input$answer,{
    check_data()
  })
}

shinyApp(ui = ui, server = server)
