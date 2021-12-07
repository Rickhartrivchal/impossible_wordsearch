rm(list = ls()); gc()
setwd("~/my_code/impossible_wordsearch")
sapply(list.files(".", full.names = TRUE), FUN = function(x) {
  if (grepl(".R$", toupper(x))) {source(x)} })

library(shiny)

# TODO list

# - Configure Display
# - a. Shrink text formula so that it has right numbers plugged in it (treat 2:1 as baseline)
# - b. Try out forcing the resolution to be consistent by printing a temporary image and forcing the resolution


# - Configure gameplay
#   a. incorporate board refresher logic
#   b. adaptive game logic + radio button toggle
#   c. (stretch goal) incorporate "none of these are correct" button for really hard modes (% occurrence as toggle)

# - Write the readme
# - a. Explain the logic of how the word searches get built
# - b. The rules of the game
# - c. Intended applications (build upon the hypernormalization example, its crowd knowledge)


ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "game_type", choices = c("custom", "adaptive"),
                   label = "Select game type:"),
      numericInput("w", label = "Width: ", min = 1, max = 100, value = 10),
      numericInput("h", label = "Height: ", min = 1, max = 100, value = 10),
      
      textInput("word", label = "Word: ", value = "word"),
      actionButton(inputId = "press_build", "Build!"), width = 1
    ),
    mainPanel(
      plotOutput("ws_output", click = "plot_click",
                  height = "1000px"),
      textOutput("timeleft"), width = 11
    )
  )
)
# I dont want the user input to be interesting, I want tactics to be interesting
# This could mean having a designated person input what the crowd is screaming
server <- function(input, output) {
  
  # timer <- reactiveTimer(5000) # uncomment this
  
  # Output the time left.
  # output$timeleft <- renderText({
  #   paste("Time left: ", seconds_to_period(timer()))
  # })
  
  inputData <- reactive({
    
    # timer() uncomment this
    print("The detail of the pattern is movement")

    ws <- buildHardWs(w = input$w, h = input$h, word = input$word)
    list(ws = ws, word = input$word)
  })
  
  observeEvent(input$press_build, {
    output$ws_output <- renderPlot({
      # timer() uncomment this
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