library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file)  %>%
    str_split("") %>%
    do.call(rbind, .)
}

test  = read_data(here::here("day25/test.txt"))
test2  = read_data(here::here("day25/test2.txt"))
input = read_data(here::here("day25/input.txt")) 


## Task 1

f = function(x) {
  n = 0
  repeat {
    
    prev_x = x
    
    ## East move
    new_x = x
    for(i in seq_len(nrow(x))) {
      for(j in seq_len(ncol(x))) {
        ip = (i %% nrow(x)) + 1
        jp = (j %% ncol(x)) + 1
        
        if (x[i,j] == ">" && x[i, jp] == ".") {
          new_x[i,j] = "."
          new_x[i,jp] = ">" 
        }
      }
    }
    (x = new_x)
    
    ## South move
    new_x = x
    for(i in seq_len(nrow(x))) {
      for(j in seq_len(ncol(x))) {
        ip = (i %% nrow(x)) + 1
        jp = (j %% ncol(x)) + 1
        
        if (x[i,j] == "v" && x[ip, j] == ".") {
          new_x[i,j] = "."
          new_x[ip,j] = "v" 
        }
      }
    }
    x = new_x
    
    n = n+1
    if (identical(x, prev_x))
      break
  }
  
  n
}
  
f(test)
f(input)
  
## Task 2
  
  
  
  