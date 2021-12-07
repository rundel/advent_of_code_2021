library(tidyverse)

read_data = function(file) {
  readLines(file) %>%
    str_split(",") %>%
    .[[1]] %>%
    as.integer()
}

test = read_data(here::here("day07/test.txt"))  

input = read_data(here::here("day07/input.txt")) 

## Task 1

sum(abs(input- median(input)))

## Task 2

f = function(x) {
  min_score = 1e12
  
  for(i in  min(x):max(x)) {
    n = abs(x - i)
    score = sum( n * (n+1)/2)
    if (score < min_score) {
      min_score = score
      print(score)
    }
  }
}

f(test)
f(input)
