library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    str_split("")
}

test = read_data(here::here("day10/test.txt"))  

input = read_data(here::here("day10/input.txt")) 

points = c(
  ")" = 3,
  "]" = 57,
  "}" = 1197,
  ">" = 25137
)

lookup = c("]" = "[", ")" = "(", "}" = "{", ">" = "<")
r_lookup = c("[" = "]", "(" = ")", "{" = "}", "<" = ">")

## Task 1

f = function(x) {
  stack = c()
  
  corrupt = FALSE
  for(val in x) {
    if (val  %in% c("[", "(", "{", "<")) {
      stack = c(stack, val)
    } else {
      if (lookup[ val ] != stack[length(stack)]) {
        corrupt = TRUE
        break
      } else {
        stack = stack[-length(stack)]
      }
    }
    #print(state)
  }
  
  if (corrupt)
    return(points[val])
  else
    0
}

map_dbl(test, f) %>%
  sum()

map_dbl(input, f) %>%
  sum()



## Task 2

test_inc = test[map_dbl(test, f) == 0]

input_inc = input[map_dbl(input, f) == 0]

g = function(x) {
  stack = c()
  
  corrupt = FALSE
  for(val in x) {
    #cat("here:", val, lookup[val], "\n")
    
    if (val  %in% c("[", "(", "{", "<")) {
      stack = c(stack, val)
    } else {
      stack = stack[-length(stack)]
    }
  }
  
  r_lookup[rev(stack)]
}

points = c(
  ")" =  1,
  "]" =  2,
  "}" =  3,
  ">" =  4
)

score = function(pts) {
  s = 0
  for(p in pts) {
    s = s * 5 + p
  }
  
  s
}

map_dbl(
  input_inc,
  ~ {
    g(.x) %>%
      {points[.]} %>% 
      score()
  }
) %>%
  sort() %>%
  median()

  


