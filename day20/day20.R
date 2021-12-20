library(tidyverse)
library(raster)

read_data = function(file) {
  #scan(file, sep=",")
  d = readLines(file)
  
  list(
    alg = d[1] %>% str_split("") %>% .[[1]],
    img = d[3:length(d)] %>%
      str_split("") %>%
      do.call(rbind, .)
  )
}

test  = read_data(here::here("day20/test.txt"))
input = read_data(here::here("day20/input.txt")) 

## Task 1

apply_alg = function(x) {
  i = to_bin(x)
  val = alg[i+1]
  #cat(i, val, ifelse(val == "#", 1, 0), "\n")
  ifelse(val == "#", 1, 0)
}

to_bin = function(x) {
  sum(2^seq(length(x)-1, 0) * x)
}

x = test
x = input

alg = x$alg 

ext = matrix(c(0,0,ncol(x$img),nrow(x$img)),2,2) 

grow = matrix(c(-1,-1,1,1),2,2)

#x$img = matrix(c(0,0,0,1,0,0,0,1,0), 3, 3, byrow=TRUE)

img = ifelse(x$img == "#", 1, 0) %>%
  raster::raster(xmn=0, xmx=ncol(x$img), ymn=0, ymx=nrow(x$img)) %>%
  extend((ext + 5*grow) %>% extent(), value=0) %>%
  raster::focal(w = matrix(1, 3,3), fun=apply_alg, pad=FALSE, padValue=0) %>%
  raster::focal(w = matrix(1, 3,3), fun=apply_alg, pad=FALSE, padValue=0)

sum(img[], na.rm=TRUE)

plot(img)


## Task 2

x = test
x = input

alg = x$alg 

ext = matrix(c(0,0,ncol(x$img),nrow(x$img)),2,2) 

grow = matrix(c(-1,-1,1,1),2,2)

#x$img = matrix(c(0,0,0,1,0,0,0,1,0), 3, 3, byrow=TRUE)

img = ifelse(x$img == "#", 1, 0) %>%
  raster::raster(xmn=0, xmx=ncol(x$img), ymn=0, ymx=nrow(x$img)) %>%
  extend((ext + 110*grow) %>% extent(), value=0) 


for(i in 1:50) {
  cat(i,'\n')
  plot(img)
  img = raster::focal(img, w = matrix(1, 3,3), fun=apply_alg, pad=FALSE, padValue=0)
    
}
plot(img)

sum(img[], na.rm=TRUE)
