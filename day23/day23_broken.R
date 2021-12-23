library(tidyverse)
library(igraph)

g = tribble(
  ~x,   ~y,
  "h1",  "h2",
  "h2",  "h3",
  "h3",  "h4",
  "h4",  "h5",
  "h5",  "h6",
  "h6",  "h7",
  "h7",  "h8", 
  "h8",  "h9",
  "h9",  "h10",
  "h10", "h11",
  "h3",  "a1",
  "a1",  "a2",
  "h5",  "b1",
  "b1",  "b2",
  "h7",  "c1",
  "c1",  "c2",
  "h9",  "d1",
  "d1",  "d2"
) %>%
  igraph::graph_from_data_frame(directed = FALSE)

paths = list()

for(v1 in names(V(g))) {
  paths[[v1]] = list()
  for(v2 in names(V(g))) {
    if (v1 != v2)
      paths[[v1]][[v2]] = shortest_paths(g, v1, v2)$vpath[[1]] %>% names()
  }
}

hall_dests = c("h1", "h2", "h4", "h6", "h8", "h10", "h11")

costs = c(a=1, b=10, c=100, d=1000)

#############
#...........#
###B#C#C#B###
  #D#D#A#A#
  #########


init_state = tribble(
  ~pod, ~room, ~been_hall, ~done,
   "b", "d1",   0,          0,
   "a", "d2",   0,          0,
   "b", "a1",   0,          0,
   "d", "a2",   0,          0,
   "c", "b1",   0,          0,
   "d", "b2",   0,          0,
   "c", "c1",   0,          0,
   "a", "c2",   0,          0
)


check_done = function(d) {
  d %>%
    mutate(done = str_detect(room, pod)) %>%
    pull(done) %>%
    all()
}

min_score = 1e6

find_moves = function(state, cost) {
  map_dfr(
    1:nrow(state),
    function(i) {
      cur_pod = state$pod[i]
      cur_been_hall = state$been_hall[i]
      cur_room = state$room[i]
      
      if (state$done[i]) 
        return(
          tibble(i = numeric(), pod=character(), dest = character(), cost = numeric(), state = list())
        )
      
      dests = setdiff(paste0(cur_pod, 1:2), state$room)
      if (length(dests) == 2)
        dests = dests[2]
      
      if (!cur_been_hall)
        dests = c(dests, hall_dests)
      
      tibble(
        i = i,
        pod = cur_pod,
        path = map2(cur_room, dests, ~ paths[[.x]][[.y]]) %>%
          keep( ~ all(!.x %in% state$room[-i]))
      ) %>%
        filter(!map_lgl(path, is.null)) %>%
        mutate(
          dest = map_chr(path, ~ .x[length(.x)]),
          cost = cost + map_dbl(path, ~ costs[cur_pod] * (length(.x) - 1)),
          state = list(state)
        ) %>%
        select(-path)
    }
  )
}

do_move = function(i, pod, dest, cost, state) {
  
  cat("Move", pod, state$room[i], "->", dest, "|", cost, "\n")
  
  state$room[i] = dest
  
  if (str_detect(state$room[i], pod))
    state$done[i] = 1
  
  if (str_detect(state$room[i], "h"))
    state$been_hall[i] = 1
  
  state
}


move = function(state = init_state) {
  
  queue = find_moves(state, 0) %>%
    arrange(cost)
  
  repeat {
    #print(queue)
    cur = queue[1,]
      
    
    
    cost = cur$cost
    state = pmap_dfr(cur, do_move)
    
    if (cost == 53)
      browser()
    
    if (check_done(state)) {
      print(state)
      print(cost)
      next
    }
    
    
    new_moves = find_moves(state, cost)
    
    queue = bind_rows(
      new_moves,
      queue[-1,]
    ) %>%
      arrange(cost) %>%
      distinct()
    
  }
}

move(init_state)
