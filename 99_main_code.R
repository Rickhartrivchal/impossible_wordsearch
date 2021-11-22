library(genalg)
library(ggplot2)
library(data.table)
library(stringr)
library(magrittr)
library(foreach)

createInitialWordsearch <- function(w, h, word) {
  # Creates an initial [h x w] wordsearch that the string word may or may not be in
  draws <- unique(sample(strsplit(tolower(word),split = ""))[[1]])
  return(
    as.matrix(
      data.frame(replicate(w,sample(draws,h,rep=T)))
    )
  )
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


if (FALSE) {
  word <- "wow"
  w <- 5
  h <- 5
  ws <- createInitialWordsearch(w = w, h = h, word = word)
  microbenchmark(ok4 <- wordLocations(ws, word))
}


imp_ws_slow <- function(w,h,word) {
  i <- 1 # iteration counter
  if ((nchar(word)>w) & nchar(word)>h) {
    stop("word too long to uphold feasability facade in given dimensions")
  } else {
  ws <- createInitialWordsearch(h,w,word) # generate initial word search, may or may not contain word
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  coordDt <- wordLocations(ws, word)
  tdo <- coordDt[, .N] # count number of bad indices
  while(tdo > 0) {
    # systematically change the values of the indices present in swap spots 
    # to maximize the descent of nrow of swap.spots
    swap.spots <- coordDt
    one.swap.list <- list(ws) # base template for what the current iteration's best ws is
    z <- 1
    one.swap.index <- c()
    for (i in 1:nrow(swap.spots)) { # for every swap spot...
      current.character <- ws[swap.spots[i,1],
                                   swap.spots[i,2]]
      new.draws <- draws[-which(draws==current.character)]
      for (j in 1:length(new.draws)) { # ...try every character in new.draws
        df.tmp <- ws
        df.tmp[swap.spots[i,1],
               swap.spots[i,2]] <- new.draws[j] # and plug into ws.df.tmp
        one.swap.list[[z]] <- df.tmp;z <- z+1 # store new ws in one.swap.list
        one.swap.index <- c(one.swap.index, # store new score in one.swap.index
                            nrow(wordLocations(df.tmp,word)))
      }
    }
    which.is.best <- which(one.swap.index== 
                             min(one.swap.index))[[1]] # pick a winner

    ws <- one.swap.list[[which.is.best]] # thats the new word search
    i <- i+1
    print(paste0("After ",i," iterations objective not satisfied at ",
                 nrow(wordLocations(ws,word))," indeces"))
    print(ws)
    tdo <- nrow(wordLocations(ws,word))
    
  }
  }
  ws
}

shuffleOnCoords <- function(ws, coords, draws) {
  coords[, new_sample := sample(draws, .N, replace = TRUE)]
  for (i in 1 : nrow(coords)) {
    ws[coords[i, x], coords[i, y]] <- coords[i, new_sample]
  }
  return(ws)
}

imp_ws_juggle <- function(w,h,word) {
  populationSize <- 10
  
  i <- 1
  ws <- createInitialWordsearch(w, h, word)
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  coordDt <- wordLocations(ws, word)
  while (nrow(coordDt) > 0 & i < 100) {
    # re-shuffle the remaining coords
    coordDt <- wordLocations(ws, word)
    
    print(paste0("Iteration ", i, ". Word found at ", nrow(coordDt), " locations."))
    
    wsPopulation <- list()
    for (i in 1 : populationSize) {
      wsPopulation[[i]] <- shuffleOnCoords(ws, coordDt, draws)
    }
    wsSize <- lapply(wsPopulation, FUN = wordLocations, word = word) %>%
      lapply(FUN = nrow)
    
    # Pick the best one and move on
    ws <- wsPopulation[[which.min(wsSize)]]
    coordDt <- wordLocations(ws, word)
    print(ws)
    i <- i + 1
  }
  return(ws)
}

imp_ws_less_slow <- function(w,h,word) {
  i <- 1 # iteration counter
  if (nchar(word) > max(w, h, na.rm = TRUE)) {
    stop("word too long to uphold feasability facade in given dimensions")
  }
  
  ws <- createInitialWordsearch(w, h, word) # generate initial word search, may or may not contain word
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  tdo <- nrow(wordLocations(ws,word)) # count number of bad indices
  while(tdo > 0) {
    # systematically change the values of the indices present in swap spots 
    # to maximize the descent of nrow of swap.spots
    swap.spots <- wordLocations(ws,word) 
    one.swap.list <- list(ws) # base template for what the current iteration's best ws is
    z <- 1
    one.swap.index <- c()
    i <- 1
    get_this_lower <- tdo
    for (i in 1:nrow(swap.spots)) { # for every swap spot...
      current.character <- ws[swap.spots[i,x],
                              swap.spots[i,y]]
      new.draws <- draws[-which(draws==current.character)]
      for (j in 1:length(new.draws)) { # ...try every character in new.draws
        df.tmp <- ws
        df.tmp[swap.spots[i,x],
               swap.spots[i,y]] <- new.draws[j] # and plug into ws.df.tmp
        one.swap.list[[z]] <- df.tmp;z <- z+1 # store new ws in one.swap.list
        one.swap.index <- c(one.swap.index, # store new score in one.swap.index
                            nrow(wordLocations(df.tmp,word)))
      }
    }
    which.is.best <- which(one.swap.index== 
                             min(one.swap.index))[[1]] # pick a winner
    
    ws <- one.swap.list[[which.is.best]] # thats the new word search
    i <- i+1
    print(paste0("After ",i," iterations objective not satisfied at ",
                 nrow(wordLocations(ws,word))," indeces"))
    print(ws)
    tdo <- nrow(wordLocations(ws,word))
    
  }
  ws
}

imp_ws <- function(n,p,word) {
  i <- 1
  ws <- createInitialWordsearch(n,p,word)
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  i <- 1
  j <- 1
  while(nrow(wordLocations(ws,word))>0) {
    swap.spots <- wordLocations(ws,word)
    current.character <- ws[swap.spots[i,1],swap.spots[i,2]]
    new.draws <- draws[-which(draws==current.character)]
    ws.tmp <- ws
    ws.tmp[swap.spots[i,1],swap.spots[i,2]] <- new.draws[j]
    if (nrow(wordLocations(ws.tmp,word))<nrow(swap.spots)) {
      print(paste0("After ",i," iterations objective not satisfied at ",
                   nrow(wordLocations(ws,word))," indeces swap made at",
                   swap.spots[i,1]," ",swap.spots[i,2]))
      print(ws.tmp)
      i <- i+1
      ws <- ws.tmp
      i <- 1
      j <- 1
    } else {
      if (i>nrow(swap.spots)) {
        stop("what happened?")
      } else {
        if (j==length(new.draws)) {
          j <- 1
          i <- i+1
        } else {
          j <- j+1
        }
      }
    }
    
    
    
    
  }
}

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
  
  # TODO: you could have all k's on left quadrant and all o's on right quadrant and get chosen
  # as the best word search. this can be fixed by running the distribution score on 
  # some subset of windows on the matrix.
  
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
