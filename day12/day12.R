library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    str_split("-") %>%
    do.call(rbind, .)
}

test = read_data(here::here("day12/test.txt"))  
test2 = read_data(here::here("day12/test2.txt"))  
test3 = read_data(here::here("day12/test3.txt"))  

input = read_data(here::here("day12/input.txt")) 




## Task 1

f = function(x) {
  pad = list(x)
  for(i in 2:4) {
    x2 = x[str_detect(x[,1], "[A-Z]+") | str_detect(x[,2], "[A-Z]+") ,]
    x2[str_detect(x2, "[A-Z]+")] = paste0(x2[str_detect(x2, "[A-Z]+")], i) 
    pad[[i]] = x2
  }
  
  x_pad = do.call(rbind, pad)
  
  igraph::graph_from_edgelist(
    x_pad, 
    directed = FALSE
  ) %>%
    igraph::all_simple_paths(from = "start", to = "end") %>%
    map(names) %>%
    map(str_replace, pattern = "([A-Z]+)\\d", replacement = "\\1") %>%
    unique()
}

length(f(test))
length(f(test2))
length(f(test3))


paths = f(input)
length(paths)

## Task 2

g = function(x) {
  big_caves = x[str_detect(x[], "[A-Z]+")] %>% 
    unique()
  
  small_caves = x[str_detect(x[], "[a-z]+")] %>% 
    unique() %>%
    setdiff(c("start", "end"))
  

  small_cave_visits = rep(0, length(small_caves)) %>%
    setNames(small_caves)

  caves = unique(c(x)) %>%
    setdiff("end")
  
  moves = map(
    caves,
    ~ unique(c(x[x[,1] == .x,], x[x[,2] == .x,])) %>%
      setdiff(c(.x, "start"))
  ) %>%
    setNames(caves)
  
  traverse = function(cave, path, repeated = FALSE) {
    
    if (cave == "end") {
      return(1)
    }
    
    if (cave == tolower(cave) && cave %in% path) {
      if (repeated == TRUE)
        return(0)
      else 
        repeated = TRUE
    }
    
    path = c(path, cave)
    #cat(cave,"|", path, "|", repeated, "\n")
  
    count = 0
    for(move in moves[[cave]]) {
      count = count + traverse(move, path, repeated)
    }
    
    return(count)
  }
  
  return(
    traverse("start", character(), FALSE)
  )
}

g(test)
g(test2)
g(test3)


g(input)
