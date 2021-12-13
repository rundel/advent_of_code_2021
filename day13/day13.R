library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  x = readLines(file)
  
  i = which(x == "")
  
  list(
    pts = x[1:(i-1)] %>%
      str_split(",") %>%
      map(as.integer) %>%
      do.call(rbind,.) %>%
      as_tibble() %>%
      setNames(c("x","y")),
    folds = x[(i+1):length(x)] %>%
      str_replace("fold along ", "") %>%
      str_split("=") %>%
      do.call(rbind, .) %>%
      as_tibble() %>%
      setNames(c("axis","val")) %>%
      mutate(val = as.integer(val))
  )
}

test = read_data(here::here("day13/test.txt"))  
input = read_data(here::here("day13/input.txt")) 


## Task 1

f = function(x) {
  pts = x$pts
  fold_axis = x$folds$axis[1]
  fold_value = x$folds$val[1]
  
  swap = c("x"="y", "y"="x")
  
  pts[[fold_axis]] = abs(pts[[fold_axis]] - fold_value) 
  
  pts %>%
    distinct() %>%
    nrow()
}

f(test)
f(input)


## Task 2

g = function(x) {
  pts = x$pts
  for(i in seq_len(nrow(x$folds))) {
    fold_axis = x$folds$axis[i]
    fold_value = x$folds$val[i]
  
    if (fold_axis == "y")
      pts[[fold_axis]] = abs(pts[[fold_axis]] - fold_value)-1
    if (fold_axis == "x")
      pts[[fold_axis]] = abs(-pts[[fold_axis]] + fold_value)-1
    
    pts = pts %>% distinct()
    #print(pts %>% ggplot(aes(x=x,y=y)) + geom_point())
  }
  
  pts %>%
    mutate(x = -x) %>% # got coords flipped for some reason
    ggplot(aes(x=x,y=y)) + 
      geom_point()
}

g(test)
g(input)
  
  
