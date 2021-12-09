rm(list = ls()); gc()
setwd("~/my_code/impossible_wordsearch")
sapply(list.files(".", full.names = TRUE), FUN = function(x) {
  if (grepl(".R$", toupper(x))) {source(x)} })

# Optimized or 3840 x 2160

# optimized for 3840 x 2160
# 48- * 853 for mobile
hRes <- 3840 # 3840
vRes <- 2160 # 2160

# 480 x 853 for mobile - then update wRes needed
wRes <- min(hRes / vRes, 1) %>% `*`(100) %>% round %>% paste0("%")

render4kPlot <- function(a_plot) {
  renderImage({
    
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, 
        width  = hRes, 
        height = vRes,
        res    = 183) # 183 
    print(a_plot)
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = wRes,
         height = "100%",
         alt = "The detail of the pattern is movement.")
  }, deleteFile = TRUE)
}



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
          imageOutput("myImage", click = "plot_click", height = "100%",
                      hover = "plot_hover")
      ),
      # imageOutput("myImage",  click = "plot_click"),
      verbatimTextOutput("plotInfo"), width = 10
    )
  )
)
# I dont want the user input to be interesting, I want tactics to be interesting
# This could mean having a designated person input what the crowd is screaming
server <- function(input, output, session) {
  
  # timer <- reactiveTimer(5000) # uncomment this
  
  # Output the time left.
  # output$timeleft <- renderText({
  #   paste("Time left: ", seconds_to_period(timer()))
  # })
  
  observe({
    # Massage inputs to avoid errors
    howBig  <- input$w * input$h
    tooBig  <- 150000
    howThin <- min(c(input$w, input$h))
    
    if (!is.na(howBig)) {
      # Lower dimensions if unreasonably big
      if (input$w > 700) {
        updateNumericInput(session, inputId = "w", value = 700)
      }
      if (input$h > 250) {
        updateNumericInput(session, inputId = "h", value = 250)
      }
      if (howBig > tooBig) {
        if (input$w > input$h) {
          updateNumericInput(session, inputId = "w", value = floor(tooBig / input$h))
        } else {
          updateNumericInput(session, inputId = "h", value = floor(tooBig / input$w))
        }
      }
      # Shorten word if it wont fit in given dimensions
      if (nchar(input$word) > howThin) {
        updateTextInput(session, inputId = "word", 
                        value = substr(input$word, 1, howThin))
      }
    }
    

    
    })
  
  inputData <- eventReactive(input$press_build, {
    # Build the WS
    if (nchar(input$word) <= 2) {
      overrideWord <- "manlet"
      ws <- buildHardWs(w = 6, h = 6, word = overrideWord)
      unsolvedPlot <- renderUnsolved(ws   = ws,
                                     word = overrideWord)
      list(ws = ws, word = overrideWord, 
           unsolved = unsolvedPlot)
    } else {
      ws <- buildHardWs(w = input$w, h = input$h, word = input$word)
      unsolvedPlot <- renderUnsolved(ws   = ws,
                                     word = input$word)
      list(ws = ws, word = input$word, 
           unsolved = unsolvedPlot)
    }
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
  
  output$plotInfo <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", e$x, " y=", e$y, "\n")
    }
    
    
    x    <- input$plot_hover$x
    y    <- input$plot_hover$y
    
    xmax <- input$plot_hover$domain$right
    ymax <- input$plot_hover$domain$bottom
    xpad <- xmax * .05
    ypad <- ymax * .05
    xmax <- input$plot_hover$domain$right - xpad
    ymax <- input$plot_hover$domain$bottom - ypad
    
    click_x <- scales::rescale(x, from = c(xpad, xmax), to = c(1, ncol(inputData()$ws)))
    click_y <- scales::rescale(y, from = c(ypad, ymax), to = c(nrow(inputData()$ws), 1))
    
    paste0("hover: ", xy_str(input$plot_hover), "\n",
           "conversion_x: ", round(as.numeric(click_x), 2),
           "; conversion_y:", round(as.numeric(click_y), 2))
  })
  
  observeEvent(input$plot_click, {
    x    <- input$plot_click$x
    y    <- input$plot_click$y
    
    xmax <- input$plot_click$domain$right
    ymax <- input$plot_click$domain$bottom
    
    xpad <- xmax * .05 # margin size in default theme
    ypad <- ymax * .05
    xmax <- input$plot_click$domain$right - xpad
    ymax <- input$plot_click$domain$bottom - ypad - 20 # axis label is 20 pixels in 4k
    
    click_x <- scales::rescale(x = x, 
                               from = c(xpad, xmax), 
                               to = c(1, ncol(inputData()$ws))) %>%
      as.numeric
    click_y <- scales::rescale(x = y, 
                               from = c(ypad, ymax), 
                               to = c(nrow(inputData()$ws), 1)) %>%
      as.numeric
  
    output$myImage <- render4kPlot(
      renderClicked(ws      = inputData()$ws,
                    word    = inputData()$word,
                    click_x = click_y,
                    click_y = click_x
      )
    )
  })
  
}
shinyApp(ui, server)