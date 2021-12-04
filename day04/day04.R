library(tidyverse)


read_input = function(file) {
  calls = readLines(file)[1] %>% str_split(",") %>% .[[1]] %>% as.integer()
  board_lines = readLines(file)[-(1:2)]
  boards = list()
  for(i in seq(1, length(board_lines), by=6)) {
    b = board_lines[i:(i+4)] %>%
      str_replace_all("  ", " ") %>%
      str_trim() %>%
      str_split(" ") %>%
      map(as.integer) %>%
      do.call(rbind, .) 
  
    boards = c(boards, list(b))
  }
  
  list(calls = calls, boards = boards)
}

test = read_input(here::here("day04/test.txt")) 

input = read_input(here::here("day04/input.txt")) 


## Task 1

bingo = function(calls, boards) {
  marks = map(boards, ~ {.x[] = rep(0, length(.x));  .x})
  for (call in calls) {
    for(j in seq_along(boards)) {
      i = which(boards[[j]] == call)
      marks[[j]][i] = 1
  
      rows = apply(marks[[j]], 1, function(x) all(x == 1))
      cols = apply(marks[[j]], 2, function(x) all(x == 1))
      
      if (any(rows) || any(cols)) {
        # Calculate score
        unmarked = sum( boards[[j]][ marks[[j]] == 0 ] )
        
        print(unmarked)
        
        return(unmarked * call)
      }
    }
  }
}

bingo(test$calls, test$boards)
bingo(input$calls, input$boards)


## Task 2

bingo2 = function(calls, boards) {
  all_boards = seq_along(boards)
  
  marks = map(boards, ~ {.x[] = rep(0, length(.x));  .x})
  for (call in calls) {
    for(j in seq_along(boards)) {
      i = which(boards[[j]] == call)
      marks[[j]][i] = 1
      
      rows = apply(marks[[j]], 1, function(x) all(x == 1))
      cols = apply(marks[[j]], 2, function(x) all(x == 1))
      
      if (any(rows) || any(cols)) {
        # Calculate score
        unmarked = sum( boards[[j]][ marks[[j]] == 0 ] )
        
        cat("Board ", j, "wins: ", unmarked * call, "\n")
        
        all_boards = setdiff(all_boards, j)
        print(all_boards)
        if (length(all_boards) == 0)
          return(unmarked * call)
      }
    }
  }
}

bingo2(test$calls, test$boards)
bingo2(input$calls, input$boards)
