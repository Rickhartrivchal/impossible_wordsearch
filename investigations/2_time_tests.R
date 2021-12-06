
if (FALSE) {
  rm(list = ls())
  library(microbenchmark)
  logDir <- "~"
  logName <- "timing_test.csv"
  setwd("~/my_code/impossible_wordsearch")
  sapply(list.files(".", full.names = TRUE), FUN = function(x) 
    if (grepl(".R$", toupper(x))) {source(x)})
  
  
  # Test 1: 25x25 "god"
  # Test 2: 5x5   "god"
  # Results:
  # - pop_size 5 always best, 100 seems best for redraw_size
  # - redraw_size only has an effect on large ws's (duh)
  w <- 5
  h <- 5
  word <- "god"
  test_dt <- data.table(
    expand.grid(
      pop_size = seq(5, 20, by = 5),
      redraw_size = seq(80, 200, by = 20),
      w = w,
      h = h,
      word = word
    )
  )
  test_dt[, eval20 := as.integer(NA)]
  foreach(i = 1 : nrow(test_dt)) %do% {
    test_dt$eval20[i] <- microbenchmark(buildHardWs(w = w, h = h,
                                                    word = word, 
                                                    pop_size = test_dt[i, pop_size],
                                                    redraw_size = test_dt[i, redraw_size]),
                                        times = 5)$time %>%
      mean %>% `/`(1000 * 1000)
  }
  if (file.exists(file.path(logDir, logName))) {
    test_dt <- rbind(test_dt, fread(file.path(logDir, logName)))
  }
  fwrite(test_dt, file.path(logDir, logName))
}