library(tidyverse)

test = readLines(here::here("day02/test.txt")) %>%
  str_split(" ")

input = readLines(here::here("day02/input.txt")) %>%
  str_split(" ")

## Task 1

pos = 0
depth = 0

for(i in input) {
  print(i)
  if (i[1] == "forward") pos = pos + as.integer(i[2])
  else if (i[1] == "down") depth = depth + as.integer(i[2])
  else if (i[1] == "up") depth = depth - as.integer(i[2])
}

cat(pos, depth,"\n")
cat(pos * depth, "\n")



## Task 2

aim = 0
pos = 0
depth = 0

for(i in input) {
  print(i)
  if (i[1] == "forward") {
    pos = pos + as.integer(i[2])
    depth = depth + aim * as.integer(i[2])
  } else if (i[1] == "down") {
    aim = aim + as.integer(i[2])
  } else if (i[1] == "up") {
    aim = aim - as.integer(i[2])
  }
}

cat(pos, depth,"\n")
cat(pos * depth, "\n")