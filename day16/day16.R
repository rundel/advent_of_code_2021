library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    str_split("") %>%
    map(as.integer) %>%
    do.call(rbind, .)
}

test  = read_data(here::here("day16/test.txt"))
test2  = read_data(here::here("day16/test2.txt"))
input = read_data(here::here("day16/input.txt")) 

split = function(x) {
  str_split(x, "") %>%
    .[[1]]
}

hex_to_bin = function(x) {
  lookup = list(
    "0" = c(0,0,0,0),
    "1" = c(0,0,0,1),
    "2" = c(0,0,1,0),
    "3" = c(0,0,1,1),
    "4" = c(0,1,0,0),
    "5" = c(0,1,0,1),
    "6" = c(0,1,1,0),
    "7" = c(0,1,1,1),
    "8" = c(1,0,0,0),
    "9" = c(1,0,0,1),
    "A" = c(1,0,1,0),
    "B" = c(1,0,1,1),
    "C" = c(1,1,0,0),
    "D" = c(1,1,0,1),
    "E" = c(1,1,1,0),
    "F" = c(1,1,1,1)
  )
  
  map(x, ~lookup[[.x]]) %>%
    unlist()
}


hex_to_bin("D2FE28" %>% str_split("") %>% .[[1]])

bin_int = function(x) {
  sum(x * 2^(seq(length(x)-1,0)))
}

type4 = function(x) {
  bin_num = c()
  
  repeat {
    if (length(x) < 5)
      break
    cur = x[1:5]
    x = x[-(1:5)]
    
    bin_num = c(bin_num, cur[-1])
    
    if (cur[1] == 0)
      break
  }
  
  list(x = x, num = bin_int(bin_num))
}


type4("101111111000101000" %>% str_split("") %>% .[[1]] %>% as.integer())


process_packet = function(x, version_sum = 0) {
  if(length(x) == 0) {
    return(
      list(x=x, version_sum = version_sum)
    )
  }
  
  print(x)
  version = bin_int(x[1:3])
  type = bin_int(x[4:6])
  x = x[-(1:6)]
  
  version_sum = version_sum + version
  
  
  
  cat("Packet: ver=",version, "type=", type, "\n")
  
  if (type == 4) {
    r = type4(x)
    x = r$x
    cat("Num: ", r$num, "\n")
  } else {
    length_type_id = x[1]
    x = x[-1]
    
    if (length_type_id == 0) {
      print("l")
      total_length = bin_int(x[1:15])
      x = x[-(1:15)]
      rest = x[-(1:total_length)]
      x = x[1:total_length]
      
    
      while(length(x > 0)) {
        z = process_packet(x, version_sum = version_sum)
        x = z$x
        version_sum = z$version_sum
      }
      
      x = rest
      
    } else {
      n_subpackets = bin_int(x[1:11])
      x = x[-(1:11)]

      
      for(i in 1:n_subpackets) {
        z = process_packet(x, version_sum = version_sum)
        x = z$x
        version_sum = z$version_sum
      }
    } 
  }
  
  return(
    list(x=x, version_sum = version_sum)
  )
}

"EE00D40C823060" 




## Task 1

f = function(x) {
 x %>%
    split() %>%
    hex_to_bin() %>%
    process_packet()
}

f('38006F45291200')

f("8A004A801A8002F478")
f("620080001611562C8802118E34")
f("C0015000016115A2E0802F182340")
f("A0016C880162017C3686B18A3D4780")

f(readLines("day16/input.txt"))


## Task 2

ops = list(
  "0" = sum,
  "1" = prod,
  "2" = min,
  "3" = max,
  "5" = function(x, y) {as.integer(x > y)},
  "6" = function(x, y) {as.integer(x < y)},
  "7" = function(x, y) {as.integer(x == y)}
)


process_packet2 = function(x) {
  #print(x)
  version = bin_int(x[1:3])
  type = bin_int(x[4:6])
  x = x[-(1:6)]
  
  nums = c()
  if (type == 4) {
    r = type4(x)
    return(r)
  } else {
    length_type_id = x[1]
    x = x[-1]
    
    if (length_type_id == 0) {
      total_length = bin_int(x[1:15])
      x = x[-(1:15)]
      rest = x[-(1:total_length)]
      x = x[1:total_length]
      
      nums = c()
      while(length(x > 0)) {
        z = process_packet2(x)
        x = z$x
        nums = c(nums, z$num)
      }
      
      x = rest
      
    } else {
      n_subpackets = bin_int(x[1:11])
      x = x[-(1:11)]
      
      
      for(i in 1:n_subpackets) {
        z = process_packet2(x)
        x = z$x
        nums = c(nums, z$num)
      }
    } 
  }
  

  f = ops[[as.character(type)]]
  return(
    list(x=x, num = do.call(f, as.list(nums)))
  )
}


g = function(x) {
  x %>%
    split() %>%
    hex_to_bin() %>%
    process_packet2()
}

g("C200B40A82")
g("04005AC33890")
g("9C0141080250320F1802104A08")

g(readLines("day16/input.txt"))
