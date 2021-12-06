# Visualization functions
library(ggplot2)

renderUnsolved <- function(ws, word) {
  # creates a plot of the word search
  ggdf <- data.table(letters = unlist(ws %>% as.data.frame) %>%
                       toupper,
                     col = sapply(1 : ncol(ws), rep, nrow(ws)) %>% 
                       as.data.frame %>% unlist,
                     row = rep(1 : nrow(ws), ncol(ws)))
  textSize <- 12 / (pmax(0, (nrow(ws) - 80) * .1) + 
    pmax(0, (ncol(ws) - 100) * .2) + 1)
  print(textSize)
  g <- ggplot(ggdf, aes(x = col, y = row, label = letters))
  g <- g+ geom_text(aes(family = "Decima Mono"), size = textSize)
  g <- g + xlab(paste0("85% of people can't find \"",
                       word,"\".  Can you?"))
  g <- g + theme(panel.background = element_blank(),
                 axis.title.y = element_blank(),
                 text = element_text(family="Decima Mono", size = 12),
                 panel.grid = element_blank(),
                 axis.text=element_blank())
  g
  
}

renderSolved <- function(ws, word, good_or_bad = "good") {
  coordDt <- findWord(ws, word)[order(x, y)]
  point1  <- coordDt[1, c(y, x)]
  point2  <- coordDt[.N, c(y, x)]
  plotCol <- ifelse(good_or_bad == "good", "green", "red")
  plotMsg <- ifelse(good_or_bad == "good", "GOOD JOB", "BAD JOB")
  msg_n   <- 15
  # creates a plot of the word search
  ggdf <- data.table(letters = unlist(ws %>% as.data.frame) %>%
                       toupper,
                     col = sapply(1 : ncol(ws), rep, nrow(ws)) %>% 
                       as.data.frame %>% unlist,
                     row = rep(1 : nrow(ws), ncol(ws)))
  g <- ggplot(ggdf, aes(x = col, y = row, label = letters)) + 
    geom_text(aes(family = "Decima Mono"), size = 12) + 
    xlab(paste0("85% of people can't find \"",
                word,"\".  Can you?")) + 
    geom_label(data = data.table(x = runif(msg_n, min = 2, max = ncol(ws)), 
                                 y = runif(msg_n, min = 2, max = nrow(ws))),
               aes(x = x, y = y),
               label = plotMsg, size = 12,
               color = plotCol, fill = "#b33877", label.size = 0, alpha = .5) + 
    geom_segment(aes(x = point1[1], xend = point2[1], 
                     y = point1[2], yend = point2[2]), 
                 color = "orange", size = 2, alpha = .1, lty = 1) + 
    theme(panel.background = element_blank(),
          axis.title.y = element_blank(),
          text = element_text(family="Decima Mono", size = 12),
          panel.grid = element_blank(),
          axis.text=element_blank())
  g
  
}

renderClicked <- function(ws, word, click_x, click_y) {
  coordDt <- findWord(ws = ws, word = word)
  coordDtClicked <- coordDt[x == round(click_x)][y == round(click_y)]
  if (coordDtClicked[, .N] > 0) {
    renderSolved(ws, word, "good")
  } else {
    renderSolved(ws, word, "bad")
  }
}



if (FALSE) {
  w <- 5
  h <- 5
  word <- "god"
  ws <- buildHardWs(w = w, h = h, word = word)
  renderUnsolved(ws, word)
  renderSolved(ws, word, "good")
  renderSolved(ws, word, "bad")
}