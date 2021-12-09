library(tidyverse)

read_data = function(file) {
  readLines(file) %>%
    str_split("") %>%
    map(as.integer) %>%
    do.call(rbind, .) 
}

test = read_data(here::here("day09/test.txt"))  

input = read_data(here::here("day09/input.txt")) 


## Task 1

sub = function(x, i, j) {
  stopifnot(i > 0, j > 0)
  x[i,j]
}
safe_sub = possibly(sub, NA)


f = function(x) {
  low_pts = c()
  
  for(i in seq_len(nrow(x))) {
    for(j in seq_len(ncol(x))) {
      pt = x[i,j]
    
      vals = c(
        safe_sub(x, i, j-1),
        safe_sub(x, i, j+1),
        safe_sub(x, i-1, j),
        safe_sub(x, i+1, j)
      )
      
      vals = vals[!is.na(vals)]
    
      if (all(pt < vals)) {
        low_pts = c(low_pts, pt)
      }
    }
  }
  
  sum(low_pts+1)
}

f(test)
f(input)

## Task 2

r = raster::raster(input)
r[r != 9] = 1
r[r == 9] = 0


clust = raster::clump(r, directions=4)

sort( table(clust[]), decreasing = TRUE )[1:3] %>%
  prod()
