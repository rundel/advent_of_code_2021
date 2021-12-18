library(tidyverse)



read_data = function(file) {
  readLines(file)
}



test  = read_data(here::here("day18/test.txt"))
test2  = read_data(here::here("day18/test2.txt"))
input = read_data(here::here("day18/input.txt")) 


split = function(x) {
  split_val = function(x) {
    x = as.integer(x)
    v1 = floor(x/2)
    v2 = x-v1
    paste0("[",v1,",",v2,"]")
  }

  m = str_extract(x, "\\d{2,}")
  
  if (is.na(m))
    x
  else
    str_replace(x, m, split_val(m))
}

"[[[[0,7],4],[15,[0,13]]],[1,1]]" %>% split()



explode = function(x) {
  x = str_split(x, "") %>% .[[1]]
  
  depth = 0
  s=-1
  e=-1
  for(c in seq_along(x)) {
    if (x[c] == "[")
      depth = depth + 1
    if (x[c] == "]")
      depth = depth - 1
    
    if (depth > 4) {
      if (!str_detect(paste(x[c:length(x)], collapse=""), "^\\[\\d+,\\d+\\]"))
        next
      
      s = c
      e = which(x[-(1:c)] == "]")[1] +c
      
      break
    }
  }
  if (s==-1)
    return(paste(x, collapse=""))
  
  
  pair = x[(s+1):(e-1)] %>%
    paste(collapse="") %>%
    str_split(",") %>%
    .[[1]] %>%
    as.integer()
  
  before = x[1:(s-1)] %>% paste(collapse="") 
  left = str_match(before, "(\\d+)[\\[\\],]*$") %>% .[,2] %>% as.integer()
  after = x[(e+1):length(x)] %>% paste(collapse="")
  right = str_match(after, "^[\\[\\],]*(\\d+)")  %>% .[,2] %>% as.integer()
  
  if (!is.na(left)) {
    before = str_replace(
      before,
      "(\\d+)([\\[\\],]*)$",
      paste0(left+pair[1],"\\2")
    )
  }
  
  if (!is.na(right)) {
    after = str_replace(
      after,
      "^([\\[\\],]*)(\\d+)",
      paste0("\\1",  right+pair[2])
    )
  }
  
  paste0(before, 0, after)
}

"[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" %>% explode()
"[[[[[9,8],1],2],3],4]" %>% explode()
"[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" %>% explode()


identical(
  "[[[[[9,8],1],2],3],4]" %>% explode(),
  "[[[[0,9],2],3],4]"
)

identical("[7,[6,[5,[4,[3,2]]]]]" %>% explode(),  "[7,[6,[5,[7,0]]]]")
identical("[[6,[5,[4,[3,2]]]],1]" %>% explode(), "[[6,[5,[7,0]]],3]")
identical("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" %>% explode(), "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
identical("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" %>% explode(), "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")





add = function(x,y) {
  paste0(
    "[",x,",",y,"]"
  )
}


f = function(x,y) {
  z = add(x,y)
  
  repeat {
    start = z
    z = explode(z)
    if (z == start) {
      z = split(z)
      #if (z != start)
      #  cat("Split:", z,"\n")
    } else {
      #cat("Explode:", z,"\n")
    }
    if (z == start)
      break
  }
  
  z
}

x = "[[[[4,3],4],4],[7,[[8,4],9]]]" 
y = "[1,1]"

f("[[[[4,3],4],4],[7,[[8,4],9]]]", "[1,1]")

f_sum = function(x) {
  z = x[1]
  for(i in 2:length(x)) {
    z = f(z, x[i])
    #print(z)
  }
  z
}



calc_mag = function(x) {
  repeat {
    start = x
    
    m = str_extract(x, "\\d+,\\d+")
    new_val = str_split(m,",") %>%
      .[[1]] %>%
      as.integer() %>%
      {3*.[1] + 2*.[2]}
    
    x = str_replace(
      x,
      paste0("\\[", m, "\\]"),
      as.character(new_val)
    )
    
    if (x == start)
      break
  }
  
  as.integer(x)
}


f_sum(test) %>% calc_mag()
f_sum(test2) %>% calc_mag()

f_sum(input) %>% calc_mag()

## Task 2

g_sum = function(x) {
  expand.grid(i = seq_along(x), j = seq_along(x)) %>%
    filter(i!=j) %>%
    rowwise() %>%
    mutate(
      mag = f(x[i], x[j]) %>% calc_mag()
    ) %>%
    arrange(desc(mag))
}

g_sum(test)
g_sum(input)
