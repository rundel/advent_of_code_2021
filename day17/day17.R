library(tidyverse)

#read_data = function(file) {
#  #scan(file, sep=",")
#  readLines(file) %>%
#    str_split("") %>%
#    map(as.integer) %>%
#    do.call(rbind, .)
#}
#
#test  = read_data(here::here("day17/test.txt"))
#input = read_data(here::here("day17/input.txt")) 

test = c(20,30,-10,-5)
input = c(257,286,-101,-57)

## Task 1

step = function(pos, vel) {
  pos[1] = pos[1] + vel[1]
  pos[2] = pos[2] + vel[2]
  
  vel[1] = ifelse(vel[1] == 0, 0, sign(vel[1]) * (abs(vel[1])-1))
  vel[2] = vel[2] - 1
  
  return(list(pos=pos, vel=vel))
}

check_target = function(pos, ta) {
  pos[1]>=ta[1] & pos[1]<=ta[2] & pos[2]>=ta[3] & pos[2]<=ta[4]
}

check_fail = function(pos, ta) {
  pos[1] > ta[2] | pos[2] < ta[3]
}


check_vel = function(vel, ta) {
  pos = c(0,0)
  
  max_y = 0
  
  repeat {
    r = step(pos, vel)
    pos = r$pos
    vel = r$vel
    
    #print(pos)
    
    max_y = max(pos[2], max_y)
    
    if (check_target(pos, ta))
      return(max_y)
    
    if (check_fail(pos, ta))
      return(NA)
  }
}

check_vel(c(7,2), test)
check_vel(c(17,-4), test)

m = 0
for(x in 1:500) {
  for(y in -500:500) {
    m = max(m, check_vel(c(x,y), input), na.rm=TRUE)
  }
}
m



## Task 2

m = 0
for(x in 1:500) {
  for(y in -500:500) {
    z = check_vel(c(x,y), input)
    
    if (!is.na(z)) {
      print(c(x, y, z))
      m = m+1
    }
  }
}
m
  

