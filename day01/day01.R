library(tidyverse)

test = readLines(here::here("day01/test.txt")) %>%
  as.integer()


input = readLines(here::here("day01/input.txt")) %>%
  as.integer()

## Task 1

n_inc = function(x) {
  sum(diff(x) > 0)
}

n_inc(test)
n_inc(input)


## Task 2


window_sum = function(x) {
  (x + lag(x, 1) + lag(x, 2)) %>%
  na.omit()
}

n_inc(window_sum(test))
n_inc(window_sum(input))



