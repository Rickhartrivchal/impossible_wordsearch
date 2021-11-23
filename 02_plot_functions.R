# Visualization functions
library(ggplot2)

drawWordSearchSolution <- function(ws, word, startCoord, endCoord, good_or_bad = "good") {
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

if (FALSE) {
  # TODO: make a function that is the above but with the word highlighted
  # - what will it do if the word is found more than once?
  # - should lines to draw be fed in as groups as inputs? function to generate?
  # - 
}