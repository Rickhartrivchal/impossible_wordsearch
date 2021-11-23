# This script is a first-stab attempt at making a genetic algorithm for impossible word search
# TODO:
#  - make work for single-sovle ws
#  - solve distribution of letters--current implementation is a bad attempt at having
#    a caliper-type solution
# comment:
# -   # TODO: you could have all k's on left quadrant and all o's on right quadrant and get chosen
# as the best word search. this can be fixed by running the distribution score on 
# some subset of windows on the matrix.

library(genalg)

evaluateWs <- function(string = c(), w = inW, h = inH, word = inWord) {
  
  ppTolerance <- 0.25 # maximum %-points diference of each letter's frequency 
                      # before adding penalty
  
  # we know: w, h, word
  # input format: 
  # c([1,1], [1, 2],..., [1, n], [2, 1], ..., [n, n-1], [n, n])
  # need to output a number of occurrences
  charsUsed   <- unique(strsplit(word, "") %>% unlist)
  wordStrings <- charsUsed[as.integer(string)] # floor, not round
  initialWs   <- matrix(wordStrings, nrow = h, ncol = w) 
  countScore  <- countWsOccurs(df = initialWs, countWord = word)
  
  proportionTable <- data.table(chars_used = charsUsed) %>%
    .[, ':='(word_dist = strsplit(word, "") %>% 
               unlist %>% 
               '=='(chars_used) %>% 
               sum / nchar(word),
             string_dist = sum(wordStrings == 
                                 chars_used, na.rm = TRUE) / 
               length(wordStrings)),
      by = chars_used] %>%
    .[, dist := abs(word_dist - string_dist)]
  proportionScore <- 100 * proportionTable[, pmax(0, max(dist) - ppTolerance)]
  return((100 + countScore) + proportionScore)
}

chooseBestWs <- function(w, h, word) {
  rbgaResults <- rbga(stringMin = rep(1, w * h),
                      stringMax = rep(1 + uniqueN(strsplit(word, "") %>% unlist), w * h),
                      iters = 2, 
                      # monitorFunc=monitor, 
                      evalFunc=evaluateWs, verbose=TRUE, mutationChance=0.01)
  bestWs <- rbga.results$population[which.min(rbga.results$evaluations),]
  charsUsed   <- unique(strsplit(word, "") %>% unlist)
  wordStrings <- charsUsed[as.integer(bestWs)] # floor, not round
  initialWs   <- matrix(wordStrings, nrow = h, ncol = w) 
}



if (FALSE) {
  setwd("~/my_code/impossible_wordsearch/investigations")
  sapply(list.files(c(".", ".."), full.names = TRUE), FUN = function(x) 
    if (grepl(".R$", toupper(x))) {source(x)})

  w <- 5
  h <- 3
  inWord <- "kok"
  rbga.results = rbga(stringMin = rep(1, w * h),
                      stringMax = rep(1 + uniqueN(strsplit(inWord, "") %>% unlist), w * h),
                      iters = 2, 
                      # monitorFunc=monitor, 
                      evalFunc=evaluateWs, verbose=TRUE, mutationChance=0.01)
  evaluateWs(string = rbga.results$population[1,],
             w = w, h = h, word = inWord)
  
  
  
  monitor <- function(obj) {
    # plot the population
    xlim = c(obj$stringMin[1], obj$stringMax[1]);
    ylim = c(obj$stringMin[2], obj$stringMax[2]);
    plot(obj$population, xlim=xlim, ylim=ylim, 
         xlab="pi", ylab="sqrt(50)");
  }
  
  
  plot(rbga.results)
  plot(rbga.results, type="hist")
  plot(rbga.results, type="vars")
  
  # system.time(wordLocations(df = df, word = "kook"))
  # systmem.time(wordLocations.f(df = df, word = "kook"))
  
  wordLocations.f <- function(df, word) {
    M <- data.frame(matrix(vector(), 0, 2))
    totalOccurs <- is.it.in()
    for (i in 1 : nrow(df)) {
      for (j in 1 : ncol(df)) {
        
        df.tmp <- df
        df.tmp[i, i] <- NA
        # if (is.it.in(df, word, i, j) )
      }
    }
  }
}
