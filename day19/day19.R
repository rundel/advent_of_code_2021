library(tidyverse)
library(igraph)

read_data = function(file) {
  #scan(file, sep=",")
  readr::read_file(file) %>%
    str_split("\n\n") %>%
    .[[1]] %>%
    str_split("\n") %>%
    map(
      function(x) {
        x[-1] %>%
          str_split(",") %>%
          map(as.integer)  %>%
          do.call(rbind, .)
      }
    )
}

test  = read_data(here::here("day19/test.txt"))
test_sim = read_data(here::here("day19/test_rot.txt"))
input = read_data(here::here("day19/input.txt")) 


rot_3d = function(z,y,x) {
  z=pi*z/180; y=pi*y/180; x=pi*x/180;
  
  matrix(c(
    cos(z), -sin(z), 0,
    sin(z),  cos(z), 0,
         0,       0, 1
  ), byrow = TRUE, ncol = 3) %*%
  matrix(c(
     cos(y),  0, sin(y),
          0,  1,      0,
    -sin(y),  0, cos(y)
  ), byrow = TRUE, ncol = 3) %*%
  matrix(c(
    1,      0,       0,
    0, cos(x), -sin(x),
    0, sin(x),  cos(x)
  ), byrow = TRUE, ncol = 3) %>%
    apply(c(1,2), round, digits=8)
}

d = expand.grid(x = c(0,90,180,270), y = c(0,90,180,270), z = c(0,90,180,270))
rot_mats = d %>%
  pmap(rot_3d) %>%
  tibble(x=.) %>%
  distinct() %>%
  pull(x)
  

apply_rots = function(x) {
  map(rot_mats, ~ x %*% .x)
}

compare = function(x, y) {
  x = apply(x, 1, paste, collapse=",")
  y = apply(y, 1, paste, collapse=",")
  
  print(x)
  print(y)
  
  length(intersect(x,y))
}

compare = function(x, y) {
  colnames(x) = c("x","y","z")
  colnames(y) = c("x","y","z")
  
  xn = x %>% as_tibble() %>% arrange(x,y,z)
  yn = y %>% as_tibble() %>% arrange(x,y,z)
  
  res = (as.matrix(xn) - as.matrix(yn)) %>%
    apply(2, function(v) unique(v)) 
  
  if (length(unlist(res)) == 3)
    res
  else
    NULL
}

  
  



## Task 1


find_intersect = function(x, y) {
  d1 = dist(x) %>% as.matrix()
  d2 = dist(y) %>% as.matrix()
  
  m1 = map_lgl(d1, ~ .x %in% d2) %>% 
    matrix(nrow=nrow(d1),ncol=ncol(d1))
  diag(m1) = FALSE
  
  sub1 = apply(m1, 1, any) %>% which()
    
  m2 = map_lgl(d2, ~ .x %in% d1) %>% 
    matrix(nrow=nrow(d2),ncol=ncol(d2))
  diag(m2) = FALSE
  
  sub2 = apply(m2, 1, any) %>% which()
  
  x = x[sub1,]
  y = y[sub2,]
  
  if (length(sub1) == 0) {
    return(list())
  }
  if (length(sub1) != length(sub2)) {
    return(list("Sub mismatch"))
  }
  
  
  
  y_rot = apply_rots(y)
  checks = map(y_rot, compare, x=x)
  rot_i = map_lgl(checks, ~!is.null(.x)) %>% which()
  
  if (length(rot_i) == 0)
    return(list("Check failure"))
  
  offset =  checks[[rot_i]]
    
  list(
    n = length(sub1),
    sub1 = sub1,
    sub2 = sub2,
    rot_i = rot_i,
    offset = offset
  )
}

correct = function(x, rot, offset) {
  offset = matrix(1, nrow = nrow(x)) %*% offset
  x %*% rot + offset
}

find_paths = function(pairs) {
  g = pairs %>%
    select(i,j) %>%
    igraph::graph_from_data_frame(directed=FALSE)
  
  igraph::shortest_paths(g, from = "1")$vpath %>%
    map(names) %>%
    map(as.integer)
}

f = function(x) {
  
  pairs = expand.grid(
    i = seq_along(x),
    j = seq_along(x)
  ) %>%
    as_tibble() %>%
    filter(j != i) %>%
    arrange(i,j)
    
  pairs$res = pmap(pairs, ~ {find_intersect(x[[.x]], x[[.y]])})

  pairs = pairs %>%
    filter(map_int(res, length) > 1)
  
  paths = find_paths(pairs)[-1] # Ignore 1 -> 1
  
  final = x
  for(path in paths) {
    path = rev(path)
    
    start = path[1]
    prev = path[1]
    for(cur in path[-1]) {
      res = pairs %>%
        filter(i == cur & j == prev) %>%
          pull(res) %>%
          .[[1]]
        
        final[[start]] = correct(final[[start]], rot_mats[[res$rot_i]], res$offset)
      
      prev = cur
    }
  }
  
  do.call(rbind, final) %>%
    as_tibble() %>%
    distinct()
  
}

f(test)
f(input)


## Task 2

g = function(x) {
  pairs = expand.grid(
    i = seq_along(x),
    j = seq_along(x)
  ) %>%
    as_tibble() %>%
    filter(j != i) %>%
    arrange(i,j)
  
  pairs$res = pmap(pairs, ~ {find_intersect(x[[.x]], x[[.y]])})
  
  pairs = pairs %>%
    filter(map_int(res, length) > 1)
  
  paths = find_paths(pairs)[-1] # Ignore 1 -> 1
  
  final = list(matrix(c(0,0,0), nrow=1))
  for(path in paths) {
    path = rev(path)
    
    start = path[1]
    prev = path[1]
    final[[start]] = matrix(c(0,0,0), nrow=1)
    
    for(cur in path[-1]) {
      res = pairs %>%
        filter(i == cur & j == prev) %>%
        pull(res) %>%
        .[[1]]
      
      final[[start]] = correct(final[[start]], rot_mats[[res$rot_i]], res$offset)
      prev = cur
    }
  }
 
  final 
}

g(test) %>%
  do.call(rbind, .) %>%
  dist(method = "manhattan") %>%
  max()

g(input) %>%
  do.call(rbind, .) %>%
  dist(method = "manhattan") %>%
  max()
