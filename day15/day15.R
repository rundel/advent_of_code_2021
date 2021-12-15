library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    str_split("") %>%
    map(as.integer) %>%
    do.call(rbind, .)
}

test  = read_data(here::here("day15/test.txt"))
test2  = read_data(here::here("day15/test2.txt"))
input = read_data(here::here("day15/input.txt")) 

## Task 1

to_df = function(x) {
  tibble::tibble(
    i = rep(rep(1:nrow(x)), ncol(x)),
    j = rep(1:nrow(x), rep(ncol(x), nrow(x))),
    risk = map2_int(i, j, ~ x[.x, .y])
  )
}


f = function(x) {
  d = x %>% 
    to_df()
  
  d2 = d %>%
    mutate(
      shift = list(list(
        c(di=1,dj=0), c(di=-1,dj=0), c(di=0,dj=1), c(di=0,dj=-1)
      )
      )) %>%
    unnest_longer(shift) %>%
    unnest_wider(shift) %>%
    mutate(
      i2 = i + di,
      j2 = j + dj
    ) %>%
    select(-di, -dj) %>%
    filter(
      i2 > 0, i2 <= nrow(x), j2 > 0, j2 <= ncol(x) 
    ) %>%
    mutate(
      V1 = paste0(i,",",j),
      V2 = paste0(i2,",",j2)
    ) %>%
    select(-i,-j, -risk) %>%
    left_join(
      d, by = c(i2 = "i", j2 = "j")
    ) %>%
    select(
      V1, V2, weight = risk
    )
  g = igraph::graph_from_data_frame(d2, directed = TRUE)
  
  end = paste0(nrow(x), ',', ncol(x))
  
  path = igraph::shortest_paths(g, "1,1", end) %>%
    {.$vpath[[1]]} %>%
    names()
  
  risk = 0
  for(i in seq_len(length(path)-1)) {
    s = d2 %>%
      filter(V1 == path[i], V2 == path[i+1]) %>%
      pull(weight)
    risk = risk + s
  }
  
  risk
}


f(test)
f(input)


## Task 2

g = function(x) {
  x = cbind(x, x+1L, x+2L, x+3L, x+4L)
  x = rbind(x, x+1L, x+2L, x+3L, x+4L)

  x[x>9] = x[x>9] %% 10L + 1L

  x
}

g(test) %>% f()

g(input) %>% f()
