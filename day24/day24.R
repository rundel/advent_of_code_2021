library(tidyverse)

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file)  %>%
    str_split(" ")
}

test  = read_data(here::here("day24/test.txt"))
input = read_data(here::here("day24/input.txt")) 

## Task 1


alu_to_r = function(instr) {
  i=1
  map_chr(
    instr,
    function(cmd) {
      if (cmd[1] == "inp") {
        i <<- i+1
        glue::glue("{cmd[2]} = input[{i-1}]")
      } else if (cmd[1] == "add") {
        glue::glue("{cmd[2]} = ({cmd[2]} + {cmd[3]})")
      } else if (cmd[1] == "mul") {
        glue::glue("{cmd[2]} = ({cmd[2]} * {cmd[3]})")
      } else if (cmd[1] == "div") {
        glue::glue("{cmd[2]} = (floor({cmd[2]} / {cmd[3]}))")
      } else if (cmd[1] == "mod") {
        glue::glue("{cmd[2]} = ({cmd[2]} %% {cmd[3]})")
      } else if (cmd[1] == "eql") {
        glue::glue("{cmd[2]} = (as.integer({cmd[2]} == {cmd[3]}))")
      }
    }
  )
}

optimize = function(code) {
  
  # Remove div 1
  code = code[!str_detect(code, "[wxyz] = floor\\([wxyz] / 1\\)")]
  
  # Find !=
  res = c()
  for(i in which(code == "x = (as.integer(x == w))")) {
    if  (code[i+1] == "x = (as.integer(x == 0))")
      res = c(res, i)
  }
  
  if (length(res) != 0) {
    code[res] = "x = (as.integer(x != w)) # opt"
    code = code[-(res+1)]
  }
  
  # Assign 0
  code =  str_replace_all(code, "([wxyz]) = \\([wxyz] \\* 0\\)", "\\1 = (0)")
  
  # Collapse related
  
  for(i in (length(code)-1):1) {
    m1 = str_match(code[i],   "([wxyz] = )(\\(.*\\))") %>% .[-1]
    m2 = str_match(code[i+1], "([wxyz] = )(\\(.*\\))") %>% .[-1]
    
    if (!any(is.na(m1)) && !any(is.na(m2)) && m1[1] == m2[1]) {
      new = paste0(m1[1], str_replace(m2[2], "[wxyz]", m1[2]))
      
      code[i] = new
      code = code[-(i+1)]
    }
  }
  
  # Remove zero addition

  code = str_replace(code, "\\(\\(0\\) \\+ ([wxyz]|\\d+)\\)", "\\1")
    
  code
}


code = alu_to_r(input)
code %>% optimize() 


eval_code = function(code, input) {
  
  env = new.env()
  assign("w", 0, envir = env)
  assign("x", 0, envir = env)
  assign("y", 0, envir = env)
  assign("z", 0, envir = env)
  
  parse(text = code) %>%
    eval(envir = env)
  
  as.list(env) %>% unlist()
}

eval_code2 = function(code, input) {
  
  code = c(
    "f=function(input) {",
    "w=0;x=0;y=0;z=0",
    code,
    "c(w=w,x=x,y=y,z=z)",
    "}"
  )
  
  env = new.env()
  
  parse(text = code) %>%
    eval(envir = env)
  
  compiler::cmpfun(
    as.list(env)$f
  )
}

eval_split = function(code) {
  code = str_replace(code, "input\\[\\d+\\]", "input")
  
  code = c(
    "f=function(other,input,w,x,y,z) {",
    code,
    "list(other=other,input=input,w=w,x=x,y=y,z=z)",
    "}"
  )
  
  env = new.env()
  
  parse(text = code) %>%
    eval(envir = env)
  
  compiler::cmpfun(
    as.list(env)$f
  )
}


split_by_inp = function(code) {
  i = which(str_detect(code, "input"))
  
  map2(
    i, c(i[-1]-1, length(code)),
    ~ code[.x:.y]
  )
}

secs = alu_to_r(input) %>% optimize() %>% split_by_inp()

## Task 1

res = tibble(
  other = "",
  w = 0,
  x = 0,
  y = 0,
  z = 0
)

for(i in 1:14) {
  f = eval_split(secs[[i]])
  cat(i, nrow(res), "\n")
  
  res = res %>%
    expand_grid(tibble(input = 9:1), .) %>%  
    do.call(f, .) %>% # vectorization f4w!
    as_tibble() %>%
    mutate(
      other = paste0(other, input)
    ) %>%
    select(-input) %>%
    distinct(w,x,y,z, .keep_all = TRUE)
}
res %>% filter(z == 0) %>% arrange(desc(other))

## Task 2

res = tibble(
  other = "",
  w = 0,
  x = 0,
  y = 0,
  z = 0
)

for(i in 1:14) {
  f = eval_split(secs[[i]])
  cat(i, nrow(res), "\n")
  
  res = res %>%
    expand_grid(tibble(input = 1:9), .) %>%  
    do.call(f, .) %>%
    as_tibble() %>%
    mutate(
      other = paste0(other, input)
    ) %>%
    select(-input) %>%
    distinct(w,x,y,z, .keep_all = TRUE)
}
res %>% filter(z == 0) %>% arrange(other)

