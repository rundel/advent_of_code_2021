library(tidyverse)

read_data = function(file) {
  readLines(file) %>%
    str_split(",") %>%
    .[[1]] %>%
    as.numeric()
}

test = read_data(here::here("day06/test.txt"))  

input = read_data(here::here("day06/input.txt")) 

## Task 1

#d = test
d = input
for(i in 1:80) {
  d = d-1
  n = sum(d<0)
  d[d<0] = 6
  d = c(d, rep(8, n))
}

length(d)

## Task 2

pop = c(0, table(input), 0, 0, 0) 
#pop = c(0, table(test), 0, 0, 0, 0) 
names(pop) = 0:8

for(i in 1:256) {
  pop[7+1] = pop[7+1] + pop[0+1]
  pop = c(pop[-1], pop[1])
}
formatC(sum(pop), digits = 16)

