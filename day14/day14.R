library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  x = readLines(file)
  
  i = which(x == "")
  
  list(
    template = x[1],
    pats = x[3:length(x)] %>%
      str_split(" -> ") %>%
      do.call(rbind, .)
  )
}

test  = read_data(here::here("day14/test.txt"))  
input = read_data(here::here("day14/input.txt")) 

## Task 1

f = function(x, n) {
  

  pats = x$pats[,2] %>%
    setNames(x$pats[,1])

  text = str_split(x$template, "") %>%
    .[[1]]
  
  for(i in 1:n) {
    newtext = c()
    
    for(i in seq_along(text)[-length(text)]) {
      cur = paste(text[i:(i+1)],collapse="")
      newtext = c(newtext, text[i])
      
      if (cur %in% names(pats)) {
        newtext = c(newtext, pats[cur])
      }
    }
    text = c(newtext, text[length(text)])
  }
  
  return(text)
}

mc_lc_diff = function(x) {
  x %>%
    table() %>%
    sort() %>%
    {.[length(.)] - .[1]}
}

f(test) %>%
  mc_lc_diff()

f(input) %>%
  mc_lc_diff()



## Task 2

to_pairs = function(chars) {
  paste0(lag(chars, 1)[-1], chars[-1])
}

init_name = function(x, name) {
  if (is.na(x[name]))
    x[name] = 0
  
  x
}

g = function(x, n=10) {
  
  chars = x$template %>%
    str_split("", simplify = TRUE) %>%
    c()

  match = x$pats[,1]
  insert = x$pats[,2]
  
  lookup = match %>%
    str_split("") %>%
    map2(insert, ~ c(paste0(.x[1], .y), paste0(.y, .x[2]))) %>%
    setNames(match)
  
  possible = c(
    unlist(lookup), names(lookup)
  ) %>%
    unique()
  
  
  init = to_pairs(chars)
  
  state = rep(bit64::as.integer64(0), length(possible)) %>%
    setNames(possible)
  
  for(val in init) {
    state[val] = state[val] + bit64::as.integer64(1)
  }
  
  for(i in seq_len(n)) {
    
    new_state = state
    
    for(j in seq_along(lookup)) {
      pattern = names(lookup)[j]
      vals = lookup[[j]]
      
      stopifnot(!is.na(state[pattern]))
      
      if (state[pattern] != 0) {
        new_state[pattern] = new_state[pattern] - state[pattern]
        new_state[vals[1]] = new_state[vals[1]] + state[pattern]
        new_state[vals[2]] = new_state[vals[2]] + state[pattern]
      }
    }
    
    state = new_state
  }
  
  state
}


count2 = function(x, first) {
  poss = names(x) %>%
    str_split("") %>%
    unlist() %>%
    unique()
  
  count = rep(bit64::as.integer64(0),length(poss)) %>%
    setNames(poss)
  
  for(i in seq_along(x)) {
    last = names(x)[i] %>% str_split("") %>% unlist() %>% .[2]
    count[last] = count[last] + bit64::as.integer64(x[i])
  }
  
  count[first] = count[first] + 1 # First 
  
  count
}






g(test, 10) %>% count2("N") %>% sort() %>% {.[length(.)] - .[1]}

g(test, 40) %>% count2("N") %>% sort() %>% {.[length(.)] - .[1]}

g(input, 40) %>% count2("C") %>% sort() %>% {.[length(.)] - .[1]}



