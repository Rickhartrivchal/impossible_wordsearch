rm(list = ls()); gc()
setwd("~/my_code/impossible_wordsearch")
sapply(list.files(".", full.names = TRUE), FUN = function(x) {
  if (grepl(".R$", toupper(x))) {source(x)} })

library(shiny)

render4kPlot <- function(a_plot) {
  renderImage({
    
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, 
        width = 3840, 
        height = 2160,
        res = 183)
    print(a_plot)
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = "100%",
         height = "100%",
         alt = "The detail of the pattern is movement.")
  }, deleteFile = TRUE)
}

# TODO list

# - Configure Display
# - a. Shrink text formula so that it has right numbers plugged in it (treat 2:1 as baseline) (this could be resolution only)
# - b. Try out forcing the resolution to be consistent by printing a temporary image and forcing the resolution


# - Configure gameplay
#   a. incorporate board refresher logic
#   b. adaptive game logic + radio button toggle
#   c. (stretch goal) incorporate "none of these are correct" button for really hard modes (% occurrence as toggle)

# - Write the readme
# - a. Explain the logic of how the word searches get built
# - b. The rules of the game
# - c. Intended applications (build upon the hypernormalization example, its crowd knowledge)

# Optimized or 3840 x 2160

ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      radioButtons(inputId = "game_type", choices = c("custom", "adaptive"),
                   label = "Select game type:"),
      numericInput("w", label = "Width: ", min = 1, max = 100, value = 10),
      numericInput("h", label = "Height: ", min = 1, max = 100, value = 10),
      
      textInput("word", label = "Word: ", value = "word"),
      actionButton(inputId = "press_build", "Build!"), width = 2
    ),
    mainPanel(
      # plotOutput("ws_output", click = "plot_click",
      #             height = "1000px"),
      div(style = "background:red; height: 100vh", 
          imageOutput("myImage", click = "plot_click", height = "100%")
      ),
      # imageOutput("myImage",  click = "plot_click"),
      textOutput("timeleft"), width = 10
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
  
  inputData <- eventReactive(input$press_build, {
    
    # timer() uncomment this
    print("The detail of the pattern is movement")
    
    ws <- buildHardWs(w = input$w, h = input$h, word = input$word)
    unsolvedPlot <- renderUnsolved(ws   = ws,
                                   word = input$word)
    list(ws = ws, word = input$word, 
         unsolved = unsolvedPlot)
  })
  
  observeEvent(input$press_build, {
    output$ws_output <- renderPlot({
      # timer() uncomment this
      # invalidateLater(1000)
      inputData()$unsolved
    })
    
    # Plot the data ####
    output$myImage <- render4kPlot(inputData()$unsolved)
  })
  
  observeEvent(input$plot_click, {
    x    <- input$plot_click$x
    y    <- input$plot_click$y
    xmax <- input$plot_click$domain$right
    ymax <- input$plot_click$domain$bottom
    print(c((input$w * x / xmax) + .5, (input$h * (ymax - y) / ymax) + .5))
    output$myImage <- render4kPlot(
      renderClicked(ws      = inputData()$ws,
                    word    = inputData()$word,
                    click_x = (input$w * x / xmax) + .5,
                    click_y = (input$h * (ymax - y) / ymax) + .5
      )
    )
  })
  
}
shinyApp(ui, server)