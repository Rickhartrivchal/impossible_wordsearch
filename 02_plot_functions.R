# Visualization functions
library(ggplot2)

renderUnsolved <- function(ws, word) {
  # creates a plot of the word search
  ggdf <- cbind.data.frame(letters = unlist(ws %>% as.data.frame) %>%
                             toupper,
                           row = sapply(1 : nrow(ws), rep, ncol(ws)) %>% 
                             as.data.frame %>% unlist,
                           col = rep(1 : ncol(ws), nrow(ws)))
  g <- ggplot(ggdf, aes(x = col, y = row, label = letters))
  g <- g+ geom_text(aes(family = "Decima Mono"))
  g <- g + xlab(paste0("85% of people can't find \"",
                       word,"\".  Can you?"))
  g <- g + theme(panel.background = element_blank(),
                 axis.title.y = element_blank(),
                 text = element_text(family="Decima Mono", size = 8),
                 panel.grid = element_blank(),
                 axis.text=element_blank())
  g
  
}

renderSolved <- function(ws, word, good_or_bad = "good") {
  coordDt <- findWord(ws, word)[order(x, y)]
  point1  <- coordDt[1, c(x, y)]
  point2  <- coordDt[.N, c(x, y)]
  plotCol <- ifelse(good_or_bad == "good", "green", "red")
  plotMsg <- ifelse(good_or_bad == "good", "GOOD JOB", "BAD JOB")
  msg_n   <- 15
  # creates a plot of the word search
  ggdf <- cbind.data.frame(letters = unlist(ws %>% as.data.frame) %>%
                             toupper,
                           row = sapply(1 : nrow(ws), rep, ncol(ws)) %>% 
                             as.data.frame %>% unlist,
                           col = rep(1 : ncol(ws), nrow(ws)))
  g <- ggplot(ggdf, aes(x = col, y = row, label = letters)) + 
    geom_text(aes(family = "Decima Mono")) + 
    xlab(paste0("85% of people can't find \"",
                word,"\".  Can you?")) + 
    geom_label(data = data.table(x = runif(msg_n, min = 0, max = nrow(ws)), 
                                 y = runif(msg_n, min = 0, max = ncol(ws))),
               aes(x = x, y = y),
               label = plotMsg, size = 5,
               color = plotCol, fill = "#b33877", label.size = 0, alpha = .5) + 
    geom_segment(aes(x = point1[1], xend = point2[1], 
                     y = point1[2], yend = point2[2]), 
                 color = "orange", size = 2, alpha = .1, lty = 3) + 
    theme(panel.background = element_blank(),
          axis.title.y = element_blank(),
          text = element_text(family="Decima Mono", size = 8),
          panel.grid = element_blank(),
          axis.text=element_blank())
  g
  
}

if (FALSE) {
  w <- 5
  h <- 5
  word <- "lex"
  ws <- buildHardWs(w, h, word)
  ws
  renderUnsolved(ws, word)
  renderSolved(ws, word, "good")
  renderSolved(ws, word, "bad")
}