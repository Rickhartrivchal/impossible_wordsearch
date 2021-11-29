rm(list = ls()); gc()
sapply(list.files(".", full.names = TRUE), FUN = function(x) {
  if (grepl(".R$", toupper(x))) {source(x)} })

library(shiny)



ui <- basicPage(
  sidebarLayout(
    
    sidebarPanel(
      numericInput("w", label = "Width: ", min = 1, max = 100),
      numericInput("h", label = "Height: ", min = 1, max = 100),
      
      textInput("word", label = "Word: ", value = "word")
    ),
    mainPanel(
      plotOutput("ws_output", click = "plot_click", height = 50)
    )
  )
)
server <- function(input, output) {
  
  # Reactive table
  values <- reactiveValues()
  values$selected_table <- data.table(x = numeric(), 
                                      release = character())
  
  
  output$ws_output <- renderPlot({
    activeReleases <- values$selected_table$release
    plotTotalsBar(totalsDt = masterData()[["totalsDt"]], activeReleases = activeReleases)
  })
  
  observeEvent(input$plot_click, {
    clickedRelease <- whichMovieGotClicked(totalsDt = masterData()[["totalsDt"]], x_coord = input$plot_click$x)
    values$selected_table <- rbind(values$selected_table,
                                   data.table(x = input$plot_click$x,
                                              release = clickedRelease)) %>%
      .[, N := .N, by = release] %>% 
      .[N < 2] %>% .[, N := NULL]
  })
  
  # Active Movie Configuration
  output$plot2 <- renderPlot({
    activeReleases <- values$selected_table$release
    plotDirectorDataWalletShare(directorDt = masterData()[["directorDt"]][release %in% activeReleases]) + 
      scale_color_manual(values = masterPalette()[which(names(masterPalette()) %in% activeReleases)])
  })
  
  # Box office performance 
  output$plot3 <- renderPlot({
    activeReleases <- values$selected_table$release
    plotTotalBars(totalsDt = copy(masterData()[["totalsDt"]]) %>%
                    .[!(release %in% activeReleases), ':='(domestic_total = 0)],
                  activeRelease = activeReleases) + 
      scale_fill_manual(values = masterPalette()[which(names(masterPalette()) %in% activeReleases)])
  })
  
  # Tooltip configuration
  output$hover_info <- renderText({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      hoveredRelease <- whichMovieGotClicked(totalsDt = masterData()[["totalsDt"]], x_coord = hover$x)
      # print(activeReleases())
      # print(toolTipForMovie(directorDt = masterData(), releaseName = hoveredRelease))
    } else {
      "Use this bar to get more info and populate below graphs."
    }
  })
}
shinyApp(ui, server)