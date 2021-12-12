library(ggplot2)
library(data.table)
library(stringr)
library(magrittr)
library(foreach)
library(scales)
library(iterators)
library(shiny)

splitWord <- function(word) {
  # Helper function to split word into character vector
  return(
    strsplit(tolower(word), split = "")[[1]]
  )
}

buildInitialWs <- function(w, h, draws) {
  # Creates an initial [h x w] wordsearch that may or may not contain the input word
  return(
    as.matrix(
      replicate(w, sample(draws, h, rep = T))
    )
  )
}

addWordToWs <- function(ws, word) {
  # Randomly adds word to the wordsearch
  # 1. 50/50 chance it will be forwards or backwards
  # 2. ~25% chance it will show up vert/horizontal/fdiag/bdiag, but depends on dimensions
  #    but should be more or less even distribution
  
  # Generate all maps
  matrixMaps <- list(horiz_mat  = nrow(ws) - row(ws),
                     vert_mat   = ncol(ws) - col(ws),
                     f_diag_mat = row(ws)  - col(ws),
                     b_diag_mat = nrow(ws) - row(ws) - col(ws))
  
  # Make a list of all strings 
  candidateStrings  <- 
    # Passing indices and not matrixMaps itself to get name + value together
    lapply(seq_along(matrixMaps), 
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
  
  # Flip a coin for reversing word's characters (wordChars)
  wordChars <- splitWord(word)
  if (round(runif(1)) == 1) {wordChars <- rev(wordChars)}
  
  # Randomly choose string + starting position to get indices
  chosenString <- candidateStrings[sample(.N, 1, prob = w)]
  chosenStart  <- sample(chosenString$w - nchar(word) + 1, 1)
  chosenEnd    <- chosenStart + length(wordChars) - 1 
  
  # Find the x- and y-coordinates of the chosen indices
  chosenMap <- matrixMaps[[chosenString$type]]
  x_list    <- split(row(ws), chosenMap)[[chosenString$pos]][chosenStart : chosenEnd]
  y_list    <- split(col(ws), chosenMap)[[chosenString$pos]][chosenStart : chosenEnd]
  
  # Add wordChars to selected coordinates
  for (i in 1 : length(wordChars)) {
    ws[x_list[i], y_list[i]] <- wordChars[i]
  }
  
  return(ws)
  
}

findWord <- function(ws, word) {
  # Returns data.table of x and y matrix coordinates where word appears
  
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

buildImpossibleWs <- function(w, h, word, pop_size = 5, redraw_size = 100) {
  
  iter_n <- 1
  
  redrawCoords <- function(ws, coords, draws) {
    # Outputs new ws with the values in coords resampled from (single character vector) draws
    redraws <- min(redraw_size, nrow(coords))
    for (i in 1 : redraws) {
      ws[coords[i, x], coords[i, y]] <- sample(draws, 1)
    }
    return(ws)
  }
  draws <- splitWord(word)
  ws <- buildInitialWs(w, h, draws)
  coordDt <- findWord(ws, word)
  while (nrow(coordDt) > 0 & iter_n < 100) {
    # re-shuffle the remaining coords
    coordDt <- findWord(ws, word)
    
    # print(paste0("Iteration ", iter_n, ". Word found at ", nrow(coordDt), " locations."))
    wsPopulation <- foreach(i = iter(1 : pop_size)) %do% {
      redrawCoords(ws, coordDt, draws)
    }
    wsSize <- lapply(wsPopulation, FUN = findWord, word = word) %>%
      lapply(FUN = nrow)
    
    # Pick the best one and move on
    ws <- wsPopulation[[which.min(wsSize)]]
    coordDt <- findWord(ws, word)
    # print(ws)
    iter_n <- iter_n + 1
  }
  return(ws)
}

buildHardWs <- function(w, h, word, pop_size = 5, redraw_size = 100) {
  # Makes the word show up exactly once

  redrawCoords <- function(ws, coords, draws) {
    # outputs new ws with the input coords resampled from (single character vector) draws
    for (i in 1 : min(redraw_size, nrow(coords))) {
      ws[coords[i, x], coords[i, y]] <- sample(draws, 1)
    }
    return(ws)
  }
  
  # Initialize word search
  iter_n  <- 1
  draws   <- splitWord(word)
  ws      <- buildInitialWs(w, h, draws)
  coordDt <- findWord(ws, word)
  
  # The while loop will keep going until it shows up exactly once
  while (nrow(coordDt) != nchar(word)) {
    print(paste0("Iteration ", iter_n, ". Word found at ", nrow(coordDt), " locations."))
    
    # If the word doesn't appear...
    if (nrow(coordDt) == 0) {
      # ...force the word in
      ws <- addWordToWs(ws, word)
      coordDt <- findWord(ws, word)
    } else {
    # Otherwise you accidentally made it occur more than once 
    # Find the best solution be finding which.min()[nchar > 0]
      wsPopulation <- foreach(i = iter(1 : pop_size)) %do% {
        redrawCoords(ws, coordDt, draws)
      }
      wsSize <- lapply(wsPopulation, FUN = findWord, word = word) %>%
        lapply(FUN = nrow)
      
      # Pick the best one and move on
      ws <- wsPopulation[[which.min(ifelse(wsSize == 0, 999, wsSize))]]
    }
    coordDt <- findWord(ws, word)
    # print(ws)

    iter_n <- iter_n + 1
  }
  return(ws)
}

if (FALSE) {
  # TODO: Explore what the best default values are for the paramters dictating how
  # ws's get shuffled around
  rm(list = ls())
  setwd("~/my_code/impossible_wordsearch")
  sapply(list.files(".", full.names = TRUE), FUN = function(x) 
    if (grepl(".R$", toupper(x))) {source(x)})
  
  word <- "Buster"
  w    <- 50
  h    <- 50
  ws   <- buildInitialWs(w = w, h = h, word = splitWord(word))
  microbenchmark(coordDt <- findWord(ws, word), times = 10)
  microbenchmark(test1 <- buildImpossibleWs(w, h, word), times = 10)
  test2 <- buildHardWs(w, h, word)
}