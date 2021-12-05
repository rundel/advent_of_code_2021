library(tidyverse)

read_data = function(file) {
  readLines(file) %>%
    tibble(x = .) %>%
    separate(x, into = c("x1","y1","x2","y2")) %>%
    mutate(
      type = case_when(
        x1 == x2 ~ "H",
        y1 == y2 ~ "V"
      )
    )
}

test = read_data(here::here("day05/test.txt"))  

input = read_data(here::here("day05/input.txt")) 

## Task 1
 
find_vents = function(x) {
  x %>%
    filter(!is.na(type)) %>%
    rowwise() %>%
    summarize(
      x = x1:x2,
      y = y1:y2,
    ) %>%
    ungroup() %>%
    count(x, y) %>%
    filter(n >=  2) %>%
    nrow()
}

find_vents(test)
find_vents(input)

## Task 2

find_vents2 = function(x) {
  x %>%
    rowwise() %>%
    summarize(
      x = x1:x2,
      y = y1:y2,
    ) %>%
    ungroup() %>%
    count(x, y) %>%
    filter(n >=  2) %>%
    nrow()
}

find_vents2(test)
find_vents2(input)
