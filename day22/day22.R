library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    str_match("(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)") %>%
    .[,-1]
}

test  = read_data(here::here("day22/test.txt"))
test2 = read_data(here::here("day22/test2.txt"))
test3 = read_data(here::here("day22/test3.txt"))
input = read_data(here::here("day22/input.txt")) 

## Task 1

f = function(x) {
  arr = array(0, dim=c(101,101,101))
  
  for(i in 1:nrow(x)) {
    cat(i,"/", nrow(x), "\n")
    val = ifelse(x[i,1] == "on", 1, 0)
    
    xs = intersect(seq(x[i,2], x[i,3]), -50:50) + 51
    ys = intersect(seq(x[i,4], x[i,5]), -50:50) + 51
    zs = intersect(seq(x[i,6], x[i,7]), -50:50) + 51
    
    
    
    for(m in xs) {
      for(n in ys) {
        #for(o in zs) {
          arr[m,n,zs] = val
        #}
      }
    }
  }
  
  sum(arr)
}

f(test)
#f(test2)
#f(input)


## Task 2

d = input %>%
  as.data.frame() %>%
  setNames(c("type", "x1", "x2", "y1", "y2", "z1", "z2")) %>%
  mutate(across(!type, as.numeric)) %>%
  as_tibble() %>%
  mutate(
    x2 = x2+1,
    y2 = y2+1,
    z2 = z2+1
  )

x_pts = c(d$x1, d$x2) %>% unique() %>% sort()
y_pts = c(d$y1, d$y2) %>% unique() %>% sort()
z_pts = c(d$z1, d$z2) %>% unique() %>% sort()

arr = array(0, c(length(x_pts)-1, length(y_pts)-1, length(z_pts)-1))

for (i in seq_len(nrow(d))) {
  x_range = seq( which(d$x1[i] == x_pts), which(d$x2[i] == x_pts)-1 )
  y_range = seq( which(d$y1[i] == y_pts), which(d$y2[i] == y_pts)-1 )
  z_range = seq( which(d$z1[i] == z_pts), which(d$z2[i] == z_pts)-1 )
  
  val = ifelse(d$type[i] == "on", 1, 0)
  
  for (x in x_range) {
    for(y in y_range) {
      for(z in z_range) {
        arr[x,y,z] = val
      }
    }
  }
}

z = which(arr==1, arr.ind=TRUE) %>%
  as_tibble() %>%
  setNames(c("xi","yi","zi")) %>% 
  mutate(
    xd = x_pts[xi+1] - x_pts[xi],
    yd = y_pts[yi+1] - y_pts[yi],
    zd = z_pts[zi+1] - z_pts[zi],
    area = xd * yd * zd
  )

z$area %>%
  sum() %>%
  bit64::as.integer64()

