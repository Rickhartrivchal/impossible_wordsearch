rm(list = ls()); gc()
setwd("~/my_code/impossible_wordsearch")
sapply(list.files(".", full.names = TRUE), FUN = function(x) {
  if (grepl(".R$", toupper(x))) {source(x)} })

library(shiny)

# TODO list
# - Make the plot letters bigger/nicer/general plot clean-up
# - Design the shiny app's "next game" logic
# - Make the shiny app have "next game" logic
# - Radio button toggle to set up adaptive mode
# - build logic + functions of adaptive mode
# - make a % chance that you will get an impossible ws + button to choose that
#         - thats the correct condition like an "E: none of these are correct"
# - make a toggle for that mode


ui <- basicPage(
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "game_type", choices = c("custom", "adaptive"),
                   label = "Select game type:"),
      numericInput("w", label = "Width: ", min = 1, max = 100, value = 10),
      numericInput("h", label = "Height: ", min = 1, max = 100, value = 10),
      
      textInput("word", label = "Word: ", value = "word"),
      actionButton(inputId = "press_build", "Build!")
    ),
    mainPanel(
      plotOutput("ws_output", click = "plot_click",
                  height = "800px"),
      textOutput("timeleft")
    )
  )
)
# I dont want the user input to be interesting, I want tactics to be interesting
# This could mean having a designated person input what the crowd is screaming
server <- function(input, output) {
  
  # output$ws_output <- renderPlot(ggplot())
  timer <- reactiveTimer(5000)
  
  # Output the time left.
  output$timeleft <- renderText({
    paste("Time left: ", seconds_to_period(timer()))
  })
  
  inputData <- reactive({
    
    timer()
    print("The detail of the pattern is movement")

    ws <- buildHardWs(w = input$w, h = input$h, word = input$word)
    list(ws = ws, word = input$word)
  })
  
  observeEvent(input$press_build, {
    output$ws_output <- renderPlot({
      timer()
      # invalidateLater(1000)
      renderUnsolved(ws = inputData()$ws, word = input$word) + 
        ggtitle(paste0((runif(n = 2) * 20) %>% round(0), collapse = ""))
    })
  })
  
  observeEvent(input$plot_click, {
    lat <- round(input$plot_click$x, 0)
    long <- round(input$plot_click$y, 0)
    print(c(lat, long))
    output$ws_output <- renderPlot({
      renderClicked(ws = inputData()$ws,
                    word = inputData()$word,
                    click_x = long, click_y = lat)
    })
  })
  
}
shinyApp(ui, server)