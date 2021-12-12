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
      div(style = "background:red; height: 100vh; width", 
          imageOutput("renderedWs", 
                      click  = "plot_click", 
                      height = "100%",
                      hover  = "plot_hover")
      ), width = 10)
  )
)

server <- function(input, output, session) {
  
  observe({
    # Massage inputs to avoid errors
    howBig  <- input$w * input$h
    tooBig  <- 150000
    tooWide <- 700
    tooTall <- 250
    howThin <- min(c(input$w, input$h))
    
    if (!is.na(howBig)) {
      # Shrink size if unreasonably big
      if (input$w > tooWide) {
        updateNumericInput(session, inputId = "w", value = tooWide)
      }
      if (input$h > tooTall) {
        updateNumericInput(session, inputId = "h", value = tooTall)
      }
      if (howBig > tooBig) {
        if (input$w > input$h) {
          updateNumericInput(session, inputId = "w", value = floor(tooBig / input$h))
        } else {
          updateNumericInput(session, inputId = "h", value = floor(tooBig / input$w))
        }
      }
      # Shrink word if too long to fit given dimensions
      if (nchar(input$word) > howThin) {
        updateTextInput(session, inputId = "word", 
                        value = substr(input$word, 1, howThin))
      }
    }
    

    
    })
  
  inputData <- eventReactive(input$press_build, {
    # Build the WS
    if (nchar(input$word) <= 2) {
      # Override when word too small
      overrideWord <- "manlet"
      ws <- buildHardWs(w = 6, h = 6, word = overrideWord)
      unsolvedPlot <- renderUnsolved(ws = ws, word = overrideWord)
      list(ws       = ws, 
           word     = overrideWord, 
           unsolved = unsolvedPlot)
    } else {
      ws <- buildHardWs(w = input$w, h = input$h, word = input$word)
      unsolvedPlot <- renderUnsolved(ws   = ws, word = input$word)
      list(ws       = ws, 
           word     = input$word, 
           unsolved = unsolvedPlot)
    }
  })
  
  observeEvent(input$press_build, {
    # Render unsolved wordsearch
    output$renderedWs <- render4kPlot(inputData()$unsolved)
  })
  
  # Render solved wordsearch upon click
  observeEvent(input$plot_click, {
    x    <- input$plot_click$x
    y    <- input$plot_click$y
    
    xmax <- input$plot_click$domain$right
    ymax <- input$plot_click$domain$bottom
    
    # Pad for margin size in default theme
    xpad <- xmax * .05
    ypad <- ymax * .05
    
    xmax <- input$plot_click$domain$right - xpad
    ymax <- input$plot_click$domain$bottom - ypad - 20 # axis label is 20 pixels in 4k
    
    # Rescale from pixels to coordinates
    click_x <- as.numeric(scales::rescale(x = x, 
                                          from = c(xpad, xmax), 
                                          to = c(1, ncol(inputData()$ws))))
    click_y <- as.numeric(scales::rescale(x = y, 
                                          from = c(ypad, ymax), 
                                          to = c(nrow(inputData()$ws), 1)))
    # Render
    output$renderedWs <- render4kPlot(
      renderClicked(ws      = inputData()$ws,
                    word    = inputData()$word,
                    click_x = click_y,
                    click_y = click_x
      )
    )
  })
  
}
shinyApp(ui, server)
