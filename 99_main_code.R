library(genalg)
library(ggplot2)
library(data.table)
library(magrittr)

how.many <- function(s,sb) {
  # count number of times a substring appears in a string
  i <- 1
  l <- nchar(sb)
  count <- 0
  for (j in l : nchar(s)) {
    new.s <- substr(s, i, j)
    if (grepl(sb,new.s)==T) {
      count <- count+1
    }
    i <- i+1
  }
  count
}

f.diag <- function(df, pos.x, pos.y) {
  # return "\"-direction diagonal
  d <- row(df)-col(df)
  q <- d[pos.x,pos.y] %>% as.character
  diag_list <- split(df,d)
  diag_at_pos <- diag_list[which(names(diag_list)==q)][[1]] %>%
    paste(collapse="")
  diag_at_pos
}


b.diag <- function(df,pos.x,pos.y) {
  # return "/"-direction diagonal 
  d <- (nrow(df)-row(df))-col(df)
  q <- d[pos.x,pos.y] %>% as.character
  diag_list <- split(df,d)
  diag_at_pos <- diag_list[which(names(diag_list)==q)][[1]] %>%
    paste(collapse="")
  diag_at_pos
}

other_way <- function(x) {
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
}

firstCut <- function(string, substring) {
  # Cuts at first occurrence, returns index or zero if doesnt occur
  cutString <- unlist(strsplit(substring(string, 1), substring))
  if(length(cutString) < 2 ) {
    return(0)
  } else { 
    return(nchar(cutString[1]) + 1)
  }
}

countOccurs <- function(string, substring) {
  cutString <- unlist(strsplit(substring(string, 1), substring))
  return(
    length(cutString) - 1 + (string == substring)
  )
}

countWsOccurs <- function(df, countWord) {
  if (ncol(df) > nrow(df)) {
    df <- t(df)
  }
  
  w <- ncol(df)
  h <- nrow(df)
  
  horizs <- sapply(1 : h, FUN = function(x) paste(df[x,], collapse = ""))
  verts  <- sapply(1 : w, FUN = function(y) paste(df[, y], collapse = ""))
  fdiags <- sapply(1 : h, FUN = function(x) f.diag(df, x, 1))
  bdiags <- sapply(1 : h, FUN = function(x) b.diag(df, x, 1))
  
  base <- c(horizs, verts, fdiags, bdiags)
  base <- c(base, sapply(base, other_way))
  
  return(
    sum(sapply(base, countOccurs, substring = countWord))
  )
}

is.it.in.f <- function(df, word) {
  horizs <- sapply(1 : nrow(df), FUN = function(x) paste(df[x,], collapse = ""))
  verts  <- sapply(1 : ncol(df), FUN = function(y) paste(df[, y], collapse = ""))
  fdiags <- sapply(1 : nrow(df), FUN = function(x) f.diag(df, x, 1))
  bdiags <- sapply(1 : nrow(df), FUN = function(x) b.diag(df, x, 1))
  
  base <- c(horizs, verts, fdiags, bdiags)
  base <- c(base, sapply(base, other_way))
  
  sum(sapply(base, grepl, pattern = word))
}

is.it.in <- function(df,word,i,j) {
  # test if a word is in a matrix of letters anchored to input index
  if (i>nrow(df) || j>ncol(df)) {
    stop("index exceeds df dimensions")
  }
  horiz <- paste(df[i,],collapse="")
  vert <- paste(df[,j],collapse="")
  diag1 <- f.diag(df,i,j)
  diag2 <- b.diag(df,i,j)
  base <- c(horiz,vert,diag1,diag2)
  base <- c(base,sapply(base,other_way))
  sum(sapply(base,grepl,pattern=word))
}

how.many.ws <- function(df,word,i,j) {
  # count how many times a word appears in a ws off a given index
  if (i>nrow(df) || j>ncol(df)) {
    stop("index exceeds df dimensions")
  }
  horiz <- paste(df[i,],collapse="")
  vert <- paste(df[,j],collapse="")
  diag1 <- f.diag(df,i,j)
  diag2 <- b.diag(df,i,j)
  base <- c(horiz,vert,diag1,diag2)
  base <- c(base,sapply(base,other_way))
  count <- 0
  for (i in 1:length(base)) {
    count <- count+how.many(base[i],word)
  }
  count
}



where.is.it <- function(df,word) {
  # returns M x 2 df where each row is an index that contains the word
  M <- data.frame(matrix(vector(), 0, 2))
  for (i in 1 : nrow(df)) {
    for (j in 1 : ncol(df)) {
      df.tmp <- df
      df.tmp[i, j] <- NA
      if (is.it.in(df, word, i, j) > 0) {
         # test if this index lies along a line that contains word
        if (how.many.ws(df.tmp, word, i, j) != how.many.ws(df,word,i,j)) {
          # test to see if index contains element of word
        M <- rbind(M, c(i, j))
        }
      }
    }
  }
  
  M
}

ws.init <- function(w, h, word) {
  # gives an initial wordsearch the word may or may not be in
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  K <- data.frame(replicate(w,sample(draws,h,rep=T)))
  as.matrix(K)
}




imp_ws_slow <- function(w,h,word) {
  i <- 1 # iteration counter
  if ((nchar(word)>w) & nchar(word)>h) {
    stop("word too long to uphold feasability facade in given dimensions")
  } else {
  ws <- ws.init(h,w,word) # generate initial word search, may or may not contain word
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  tdo <- nrow(where.is.it(ws,word)) # count number of bad indices
  while(tdo>0) {
    # systematically change the values of the indices present in swap spots 
    # to maximize the descent of nrow of swap.spots
    swap.spots <- where.is.it(ws,word) 
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
                            nrow(where.is.it(df.tmp,word)))
      }
    }
    which.is.best <- which(one.swap.index== 
                             min(one.swap.index))[[1]] # pick a winner

    ws <- one.swap.list[[which.is.best]] # thats the new word search
    i <- i+1
    print(paste0("After ",i," iterations objective not satisfied at ",
                 nrow(where.is.it(ws,word))," indeces"))
    print(ws)
    tdo <- nrow(where.is.it(ws,word))
    
  }
  }
  ws
}

imp_ws_less_slow <- function(w,h,word) {
  i <- 1 # iteration counter
  if (nchar(word) > max(w, h, na.rm = TRUE)) {
    stop("word too long to uphold feasability facade in given dimensions")
  }
  
  ws <- ws.init(w, h, word) # generate initial word search, may or may not contain word
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  tdo <- nrow(where.is.it(ws,word)) # count number of bad indices
  while(tdo > 0) {
    # systematically change the values of the indices present in swap spots 
    # to maximize the descent of nrow of swap.spots
    swap.spots <- where.is.it(ws,word) 
    one.swap.list <- list(ws) # base template for what the current iteration's best ws is
    z <- 1
    one.swap.index <- c()
    i <- 1
    get_this_lower <- tdo
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
                            nrow(where.is.it(df.tmp,word)))
      }
    }
    which.is.best <- which(one.swap.index== 
                             min(one.swap.index))[[1]] # pick a winner
    
    ws <- one.swap.list[[which.is.best]] # thats the new word search
    i <- i+1
    print(paste0("After ",i," iterations objective not satisfied at ",
                 nrow(where.is.it(ws,word))," indeces"))
    print(ws)
    tdo <- nrow(where.is.it(ws,word))
    
  }
  ws
}

imp_ws <- function(n,p,word) {
  i <- 1
  ws <- ws.init(n,p,word)
  draws <- unique(sample(strsplit(tolower(word),split=""))[[1]])
  i <- 1
  j <- 1
  while(nrow(where.is.it(ws,word))>0) {
    swap.spots <- where.is.it(ws,word)
    current.character <- ws[swap.spots[i,1],swap.spots[i,2]]
    new.draws <- draws[-which(draws==current.character)]
    ws.tmp <- ws
    ws.tmp[swap.spots[i,1],swap.spots[i,2]] <- new.draws[j]
    if (nrow(where.is.it(ws.tmp,word))<nrow(swap.spots)) {
      print(paste0("After ",i," iterations objective not satisfied at ",
                   nrow(where.is.it(ws,word))," indeces swap made at",
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

string <- rbga.results$population[1,]
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
  
  # system.time(where.is.it(df = df, word = "kook"))
  # systmem.time(where.is.it.f(df = df, word = "kook"))
  
  where.is.it.f <- function(df, word) {
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
