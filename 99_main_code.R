library(ggplot2)
library(data.table)
library(stringr)
library(magrittr)
library(foreach)

buildInitialWordsearch <- function(w, h, word) {
  # Creates an initial [h x w] wordsearch that the string word may or may not be in
  draws <- unique(sample(strsplit(tolower(word), split = ""))[[1]])
  return(
    as.matrix(
      data.frame(replicate(w, sample(draws, h, rep = T)))
    )
  )
}

addWordToWs <- function(ws, word) {
  # Randomly adds word to the wordsearch
  # - Doesn't test for single occurrence
  # - random components: 1. 50/50 chance it will be forwards or backwards
  #                      2. chance it will show up vert/horizontal/diag depends on dimensions
  #                         but should be more or less even distribution
  
  # Generate all maps
  matrixMaps <- list(horiz_mat  = nrow(ws) - row(ws),
                     vert_mat   = ncol(ws) - col(ws),
                     f_diag_mat = row(ws)  - col(ws),
                     b_diag_mat = nrow(ws) - row(ws) - col(ws))
  
  # Make a list of all strings 
  candidateStrings  <- 
    lapply(seq_along(matrixMaps), # indices not matrixMaps for passing name + value together
           FUN = function(types, maps, i) {
             map  <- maps[[i]]
             type <- types[i]
             allStrings <- lapply(split(ws, map), 
                                  FUN = paste0, collapse = "")
             # Filter to remove strings that can't fit word
             filteredStrings <- allStrings[sapply(allStrings, 
                                                  FUN = function(x) nchar(x) >= nchar(word))]
             # Each row of output corresponds to candidate string to add word to
             data.table(type = types[i], pos = names(filteredStrings), 
                        old_string = unlist(filteredStrings))
           }, types = names(matrixMaps), maps = matrixMaps) %>%
    rbindlist %>%
    .[, w := nchar(old_string)]
  
  # Flip a coin for reversing word and get characters
  wordChars <- strsplit(word, "")[[1]]
  if (round(runif(1)) == 1) {wordChars <- rev(wordChars)}
  
  # Randomly choose string + starting position
  chosenString <- candidateStrings[sample(.N, 1, prob = w)]
  chosenStart  <- sample(chosenString$w - nchar(word) + 1, 1)
  chosenEnd    <- chosenStart + length(wordChars) - 1 
  chosenMap    <- matrixMaps[[chosenString$type]]
  
  # Find the x and y-coordinates of the chosen line
  x_list    <- split(row(ws), chosenMap)[[chosenString$pos]][chosenStart : chosenEnd]
  y_list    <- split(col(ws), chosenMap)[[chosenString$pos]][chosenStart : chosenEnd]
  
  # Add the word now that we know the coordinates to add to
  for (i in 1 : length(wordChars)) {
    ws[x_list[i], y_list[i]] <- wordChars[i]
  }
  
  return(ws)
  
}

wordLocations <- function(ws, word) {
  # outputs a data.table of x and y matrix coordinates where word appears
  # TODO: testing, "wowow" for word "wow", test to make sure word is complete
  
  matrixMaps <- list(horiz_mat  = nrow(ws) - row(ws),
                     vert_mat   = ncol(ws) - col(ws),
                     f_diag_mat = row(ws)  - col(ws),
                     b_diag_mat = nrow(ws) - row(ws) - col(ws))
  
  wordCoordsFromMatrixMap <- function(matrixMap, wordSearch, word) {
    string_list <- split(wordSearch, matrixMap) %>% lapply(FUN = paste0, collapse = "")
    occurs      <- grep(pattern = word, x = string_list)
    x_list      <- split(row(wordSearch), matrixMap)[occurs]
    y_list      <- split(col(wordSearch), matrixMap)[occurs]
    words_list  <- string_list[occurs]
    starts_list <- str_locate_all(string = words_list, pattern = word) %>%
      lapply(function(x) x[, 1])
    
    indicesFromStarts <- function(startsVector, wordLength) {
      return(
        startsVector %>% 
          lapply(function(x) (x : (x + wordLength - 1))) %>%
          unlist %>% 
          unique
      )
    }
    
    # Filter down to when the word occurs in each string
    index_list   <- lapply(starts_list, indicesFromStarts, wordLength = nchar(word))
    x_list_index <- mapply(function(x, i) x[i], x = x_list, i = index_list) %>%
      unlist %>% as.numeric # as.numeric() call keeps columns intact when 0 rows
    y_list_index <- mapply(function(y, i) y[i], y = y_list, i = index_list) %>%
      unlist %>% as.numeric()
    output <- data.table(x = x_list_index,
                         y = y_list_index) %>% unique %>%
      .[order(x, y)]
    return(output)
  }
  
  reversedWord <- strsplit(word, "") %>% `[[`(1) %>% rev %>% paste(collapse = "")
  
  output <- rbind(
    lapply(matrixMaps, 
           FUN = wordCoordsFromMatrixMap, wordSearch = ws, word = word) %>%
      rbindlist,
    lapply(matrixMaps, 
           FUN = wordCoordsFromMatrixMap, wordSearch = ws, word = reversedWord) %>%
      rbindlist
  ) %>%
    unique
}

drawWordSearch <- function(ws, word) {
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

buildImpossibleWs <- function(w,h,word) {
  
  populationSize <- 10
  iter_n <- 1
  
  redrawCoords <- function(ws, coords, draws) {
    # outputs new ws with the input coords resampled from (single character vector) draws
    maxRedraws <- 50
    
    for (i in 1 : min(maxRedraws, nrow(coords))) {
      ws[coords[i, x], coords[i, y]] <- sample(draws, 1)
    }
    return(ws)
  }
  
  ws <- buildInitialWordsearch(w, h, word)
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  coordDt <- wordLocations(ws, word)
  while (nrow(coordDt) > 0 & iter_n < 100) {
    # re-shuffle the remaining coords
    coordDt <- wordLocations(ws, word)
    
    # print(paste0("Iteration ", iter_n, ". Word found at ", nrow(coordDt), " locations."))
    wsPopulation <- foreach(i = iter(1 : populationSize)) %do% {
      redrawCoords(ws, coordDt, draws)
    }
    wsSize <- lapply(wsPopulation, FUN = wordLocations, word = word) %>%
      lapply(FUN = nrow)
    
    # Pick the best one and move on
    ws <- wsPopulation[[which.min(wsSize)]]
    coordDt <- wordLocations(ws, word)
    # print(ws)
    iter_n <- iter_n + 1
  }
  return(ws)
}

buildHardWs <- function(w, h, word) {
  populationSize <- 10
  iter_n <- 1
  
  redrawCoords <- function(ws, coords, draws) {
    # outputs new ws with the input coords resampled from (single character vector) draws
    maxRedraws <- 50
    
    for (i in 1 : min(maxRedraws, nrow(coords))) {
      ws[coords[i, x], coords[i, y]] <- sample(draws, 1)
    }
    return(ws)
  }
  
  # Initialize an impossible word search
  ws      <- buildImpossibleWs(w, h, word)
  draws   <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  coordDt <- wordLocations(ws, word)
  
  # The while loop will keep going until it shows up exactly once
  while (nrow(coordDt) != nchar(word)) {
    print(paste0("Iteration ", iter_n, ". Word found at ", nrow(coordDt), " locations."))
    
    # If the word doesn't appear...
    if (nrow(coordDt) == 0) {
      # ...force the word in
      ws <- addWordToWs(ws, word)
      coordDt <- wordLocations(ws, word)
    } else {
    # Otherwise you accidentally made it occur more than once 
    # Find the best solution be finding which.min()[nchar > 0]
      wsPopulation <- foreach(i = iter(1 : populationSize)) %do% {
        redrawCoords(ws, coordDt, draws)
      }
      wsSize <- lapply(wsPopulation, FUN = wordLocations, word = word) %>%
        lapply(FUN = nrow)
      
      # Pick the best one and move on
      ws <- wsPopulation[[which.min(ifelse(wsSize == 0, 999, wsSize))]]
    }
    coordDt <- wordLocations(ws, word)
    # print(ws)
    iter_n <- iter_n + 1
  }
  return(ws)
}

if (FALSE) {
  rm(list = ls())
  setwd("~/my_code/impossible_wordsearch")
  sapply(list.files(".", full.names = TRUE), FUN = function(x) 
    if (grepl(".R$", toupper(x))) {source(x)})
  
  word <- "wow"
  w    <- 15
  h    <- 15
  ws   <- buildInitialWordsearch(w = w, h = h, word = word)
  microbenchmark(coordDt <- wordLocations(ws, word))
  microbenchmark(test1 <- buildImpossibleWs(w, h, word), times = 10)
  microbenchmark(test2 <- buildHardWs(w, h, word), times = 10)
  drawWordSearch(test1, "juggle")
}