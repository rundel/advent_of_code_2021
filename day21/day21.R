library(tidyverse)
library(raster)

read_data = function(file) {
  #scan(file, sep=",")
  d = readLines(file)
}

test = read_data(here::here("day21/test.txt"))
input = read_data(here::here("day21/input.txt"))

## Task 1

die = 1:3

pos = c(4,8)
pos = c(1,5)
score = c(0,0)

mod_fun = function(val, max) {
  (val - 1) %% max + 1
}

turn = 1
cur = 1
repeat {
  pos[cur] = (pos[cur] + sum(die)) %>% mod_fun(10)
  score[cur] = score[cur] + pos[cur]
  
  if (any(score >= 1000))
    break
  
  cat(turn,'|', pos, '|', cur, '|', score, "\n")
  
  die = mod_fun(die + 3, 100)
  turn = turn+1
  cur = mod_fun(turn, 2)
}

print(score)
print(min(score) * 3*turn)





## Task 2

#pos = c(4,8) # Test
pos = c(1,5) # Input

score = c(0,0)

map = list()
for(x in 1:10) {
  map[[x]] = list()
  for(y in 1:10) {
    map[[x]][[y]] = list()
    for(i in 1:30) {
      map[[x]][[y]][[i]] = list()
      for(j in 1:30) {
        map[[x]][[y]][[i]][[j]] = NA
      }
    }
  }
}

check = function(pos, score) {
  x = pos[1]
  y = pos[2]
  i = score[1]+1
  j = score[2]+1
  
  stopifnot(x>=1, x<=10, y>=1, y<=10, i>=1, i<=21, j>=1, j<=21)

  map[[x]][[y]][[i]][[j]]
}

assign = function(pos, score, win) {
  x = pos[1]
  y = pos[2]
  i = score[1]+1
  j = score[2]+1
  
  stopifnot(x>=1, x<=10, y>=1, y<=10, i>=1, i<=21, j>=1, j<=21)
  
  map[[x]][[y]][[i]][[j]] <<- win
}






state = list()

f = function(pos, score, turn = 1) {
  #cur = mod_fun(turn, 2)
  cur = 1
  
  if (score[1] >= 21)
    return( bit64::as.integer64(c(1,0)) )
  
  if (score[2] >= 21)
    return( bit64::as.integer64(c(0,1)) )
  
  hash = check(pos, score)
  if (bit64::is.integer64(hash))
    return(hash)
  
  wins = bit64::as.integer64(c(0,0))
  for(d1 in 1:3) {
    for(d2 in 1:3) {
      for(d3 in 1:3) {
        new_pos = pos
        new_score = score
        
        new_pos[cur] = mod_fun(new_pos[cur] + d1+d2+d3, 10)
        new_score[cur] = new_score[cur] + new_pos[cur]
        
        wins = wins + rev(f(rev(new_pos), rev(new_score), turn+1)) # need this so both sides hit the cache properly
      }
    }
  }
  
  assign(pos, score, wins)
  
  return(wins)
}

f(pos, score)

