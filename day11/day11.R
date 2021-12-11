library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    str_split("") %>%
    map(as.integer) %>%
    do.call(rbind, .)
}

test = read_data(here::here("day11/test.txt"))  

input = read_data(here::here("day11/input.txt")) 

## Task 1

f = function(x, steps = 100)  {
  n = 0
  
  for(step in seq_len(steps)) {
    # Step 1 - Inc
    x = x + 1
    
    # Step 2 - Flashes
    
    repeat {
      ind = which(x > 9, arr.ind = TRUE)
      if (nrow(ind) == 0)
        break
      
      i = ind[1,1]
      j = ind[1,2]
      x[i,j] = NA
      for(dx in c(-1,0,1)) {
        for(dy in c(-1,0,1)) {
          if (dx ==0 & dy == 0)
            next
          
          if (i+dx < 0 | i+dx > nrow(x)) next
          if (j+dy < 0 | j+dy > ncol(x)) next
          
          x[i+dx,j+dy] = x[i+dx,j+dy] + 1
        }
      }
    }
    
    n = n + sum(is.na(x))
    x[is.na(x)] = 0
  }
  
  n
}

f(test)
f(input)


## Task 2

g = function(x)  {
  step = 1
  
  repeat {
    cat("Step 1: ", step, "\n")
    
    # Step 1 - Inc
    x = x + 1
    
    # Step 2 - Flashes
    
    repeat {
      ind = which(x > 9, arr.ind = TRUE)
      if (nrow(ind) == 0)
        break
      
      i = ind[1,1]
      j = ind[1,2]
      x[i,j] = NA
      for(dx in c(-1,0,1)) {
        for(dy in c(-1,0,1)) {
          if (dx ==0 & dy == 0)
            next
          
          if (i+dx < 0 | i+dx > nrow(x)) next
          if (j+dy < 0 | j+dy > ncol(x)) next
          
          x[i+dx,j+dy] = x[i+dx,j+dy] + 1
        }
      }
    }
    
    n = sum(is.na(x))
    if (n == length(x))
      break
    
    x[is.na(x)] = 0
    
    step = step+1
  }
  
  step
}

g(test)
g(input)
