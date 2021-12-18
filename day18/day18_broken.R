library(tidyverse)

parse_line = function(line) {
  line %>%
    str_replace_all("]", ")") %>%
    str_replace_all("\\[", "list(") %>%
    parse(text = .) %>%
    eval()
}

read_data = function(file) {
  #scan(file, sep=",")
  readLines(file) %>%
    map(parse_line)
}



test  = read_data(here::here("day18/test.txt"))
test2  = read_data(here::here("day18/test2.txt"))
input = read_data(here::here("day18/input.txt")) 

add = function(x, y) {
  r = list()
  r[[1]] = x
  r[[2]] = y
  r
}

is_pure = function(x) {
  if (is.numeric(x))
    TRUE
  else
    is.numeric(x[[1]]) & is.numeric(x[[2]])
}

check_explode  = function(x, depth=0) {
  if(length(x) > 2) {
    dput(x)
    stop("Oops")
  }
  
  if (!is.list(x)) {
    x
  } else if (depth >= 3 & is_pure(x[[1]])) {
    #print("Explode1")
    #dput(x)
    list(0, x[[1]][[2]] + x[[2]])
  } else if (depth >= 3 & is_pure(x[[2]])) {
    #print("Explode2")
    #dput(x)
    list(x[[2]][[1]] + x[[1]], 0)

  } else {
    #dput(x)
    list(
      check_explode(x[[1]], depth =  depth+1),
      check_explode(x[[2]], depth =  depth+1)
    )
  }
}

add_left = function(x, val) {
  if (is.numeric(x)) {
    x
  } else if (is.numeric(x[[1]])) {
    list(
      x[[1]] + val,
      x[[2]]
    )
  } else {
    list(
      add_left(x[[1]], val),
      x[[2]]
    )
  }
}

add_right = function(x, val) {
  if (is.numeric(x)) {
    x
  } else if (is.numeric(x[[2]])) {
    list(
      x[[1]],
      x[[2]] + val
    )
  } else {
    list(
      x[[1]],
      add_right(x[[2]], val)
    )
  }
}

list(1,list(1,1)) %>% add_right(3) %>% dput()
list(list(1,1), 1) %>% add_right(3) %>% dput()

list(1,list(1,1)) %>% add_left(3) %>% dput()
list(list(1,1), 1) %>% add_left(3) %>% dput()



check_explode2  = function(x, depth=0) {
  if(length(x) > 2) {
    dput(x)
    stop("Oops")
  }
  
  if (is.numeric(x)) {
    list(val=x, l=0, r=0)
  } else if (depth >= 4 & is_pure(x)) {
    list(val=0, l=x[[1]], r=x[[2]])
  } else {
    #dput(x)
    
    ex1 = check_explode2(x[[1]], depth =  depth+1)
    if (ex1$r == 0 & ex1$l == 0)
      ex2 = check_explode2(x[[2]], depth =  depth+1)
    else
      ex2 = list(val = x[[2]], l=0, r=0)
    
    #dput(ex1)
    #dput(ex2)
    
    if (ex1$r != 0) {
      if (is.numeric(ex2$val))
        ex2$val = ex2$val + ex1$r
      else
        ex2$val = add_left(ex2$val, ex1$r)
      ex1$r = 0
    }
    
    if (ex2$l != 0) {
      if (is.numeric(ex1$val))
        ex1$val = ex1$val + ex2$l
      else
        ex1$val = add_right(ex1$val, ex2$l)
      ex2$l = 0
    }

    if (depth != 0)
      list(
        val = list(ex1$val, ex2$val),
        l = ex1$l,
        r = ex2$r
      )
    else
      list(ex1$val, ex2$val)
  }
}

parse_line("[1,1]") %>% check_explode2() %>% dput()



parse_line("[[[[[9,8],1],2],3],4]") %>% check_explode2() %>% dput()
parse_line("[7,[6,[5,[4,[3,2]]]]]") %>% check_explode2() %>% dput()
parse_line("[[6,[5,[4,[3,2]]]],1]") %>% check_explode2() %>% dput()

parse_line("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]") %>% check_explode2() %>% dput()

parse_line("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]") %>% check_explode2() %>% dput()

add(x,y) %>% check_explode2() %>% dput()


check_split  = function(x) {
  if (!is.list(x)) {
    if ( x < 10) {
      x
    } else {
      v1 = floor(x/2)
      v2 = x-v1
      list(v1, v2)
    }
  } else {
    list(
      check_split(x[[1]]),
      check_split(x[[2]])
    )
  }
}

"[10,0]" %>% parse_line() %>% check_split() %>% dput()







calc_mag = function(x) {
  if(is.numeric(x))
    x
  else
    3*calc_mag(x[[1]])+2*calc_mag(x[[2]])
}

"[[1,2],[[3,4],5]]" %>% parse_line() %>% calc_mag()                   # 143.
"[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" %>% parse_line() %>% calc_mag()   # 1384.
"[[[[1,1],[2,2]],[3,3]],[4,4]]" %>% parse_line() %>% calc_mag()       # 445.
"[[[[3,0],[5,3]],[4,4]],[5,5]]" %>% parse_line() %>% calc_mag()       # 791.
"[[[[5,0],[7,4]],[5,5]],[6,6]]" %>% parse_line() %>% calc_mag()       # 1137.
"[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" %>% parse_line() %>% calc_mag()       # 3488.



## Task 1



x = "[[[[4,3],4],4],[7,[[8,4],9]]]" %>% parse_line()
y = "[1,1]" %>% parse_line()




f = function(x, y) {
  z = add(x,y)
  
  repeat {
    #dput(z)
    #cat("\n")
    
    start = z
    z = z %>% check_explode2()
    
    #dput(z)
    #cat("\n")
    
    z = z %>% check_split()
    #dput(z)
    #cat("\n")
    
    
    if (identical(start, z))
      break
  }
  
  return(z)
}

identical(
  "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" %>% parse_line(),
  f(x,y)
)


f_sum = function(x) {
  z = x[[1]]
  for(i in 2:length(x)) {
    z = f(z, x[[i]])
    cat(i,":\n")
    dput(z)
  }
  dput(z)
}


c("[1,1]","[2,2]", "[3,3]","[4,4]") %>%
  map(parse_line) %>%
  f_sum() %>%
  identical(
    "[[[[1,1],[2,2]],[3,3]],[4,4]]" %>% parse_line()
  )

f(
  c("[1,1]","[2,2]", "[3,3]","[4,4]") %>%  map(parse_line) %>% f_sum(),
  "[5,5]" %>% parse_line()
) %>%
  identical(
    "[[[[3,0],[5,3]],[4,4]],[5,5]]" %>% parse_line()
  )

f(
  c("[1,1]","[2,2]", "[3,3]","[4,4]","[5,5]") %>%  map(parse_line) %>% f_sum(),
  "[6,6]" %>% parse_line()
) %>%
  dput()

f_sum(test2)

## Task 2