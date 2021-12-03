library(tidyverse)

test = readLines(here::here("day03/test.txt")) %>%
  str_split("") %>%
  do.call(rbind, .)

input = readLines(here::here("day03/input.txt")) %>%
  str_split("") %>%
  do.call(rbind, .)

## Task 1

f = function(x) {
  t = table(x)
  which.max(t) - 1
}

gamma = apply(input, 2, f)
eps = abs(gamma - 1)

to_int = function(x) {
  n = length(x)
  sum(x * 2^(n:1-1))
}

to_int(gamma) * to_int(eps)


# Task 2

o2 = function(m) {
  col = 1
  
  while(nrow(m) > 1) {
    t = table(m[,col])
    if (length(t) == 2 && t[1] == t[2]) {
      mcv = 1
    } else if (length(t) == 1) {
      mcv = as.integer(names(t))
    } else {
      mcv = which.max(t)-1
    }

    m = m[m[,col] == mcv, ,drop = FALSE]
    col = col+1
  }
  
  return(m)
}

co2 = function(m) {
  col = 1
  
  while(nrow(m) > 1) {
    t = table(m[,col])
    if (length(t) == 2 && t[1] == t[2]) {
      mcv = 0
    } else if (length(t) == 1) {
      mcv = as.integer(names(t))
    } else {
      mcv = abs(which.max(t)-1 -1)
    }

    m = m[m[,col] == mcv, ,drop = FALSE]
    col = col+1
  }
  
  return(m)
}

o2(input) %>% as.integer() %>% to_int()  * co2(input) %>% as.integer() %>% to_int()
