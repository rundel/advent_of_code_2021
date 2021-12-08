library(tidyverse)

read_data = function(file) {
  readLines(file) %>%
    tibble(d = .) %>%
    separate(d, into = c("seq","digits"),sep = "\\|") %>%
    mutate(
      seq = str_split(str_trim(seq), " "),
      digits = str_split(str_trim(digits), " ")
    )
  
}

test = read_data(here::here("day08/test.txt"))  

input = read_data(here::here("day08/input.txt")) 

n_segs = c(
  6, 2, 5, 5, 4, 5, 6, 3,7,6
) 

segs = list(
  "0" = c("a","b", "c","e","f","g"),
  "1" = c("c", "f"),
  "2" = c("a","c","d","e","g"),
  "3" = c("a","c","d","f","g"),
  "4" = c("b","c", "d", "f"),
  "5" = c("a","b","d","f","g"),
  "6" = c("a","b","d","e","f","g"),
  "7" = c("a","c","f"),
  "8" = c("a","b","c","d","e","f","g"),
  "9" = c("a","b","c","d","f","g")
) 




## Task 1

input %>%
  mutate(
    n = map_int(
      digits,
      ~ {sum(nchar(.x) %in% c(2,4,3,7))}
    )
  ) %>%
  {sum(.$n)}


## Task 2

#1 4 7 8


translate_digits = function(seq, digits) {
  
  
  seq_lets = str_split(seq, "")
  n_seq =map_int(seq, nchar)
  
  a = setdiff(
    seq_lets[[which(n_seq == 3)]], # 7
    seq_lets[[which(n_seq == 2)]]  # 1
  )
  
  bd = setdiff(
    seq_lets[[which(n_seq == 4)]], # 7
    seq_lets[[which(n_seq == 2)]]  # 1
  )
  
  bdeg = setdiff(
    seq_lets[[which(n_seq == 7)]], # 7
    c(a, seq_lets[[which(n_seq == 3)]])
  )
  
  eg  = setdiff(bdeg, bd)
  
  # 6 segement numbers 9, 6, 0
  
  seg_6 = seq_lets[which(n_seq == 6)]
  i6 = map_lgl(seg_6, ~ all(seq_lets[[which(n_seq == 2)]] %in% .x)) %>%
    {which(!.)}
  
  w4 = map_int(seg_6, ~ sum(seq_lets[[which(n_seq == 4)]] %in% .x)) 
  
  i9 = which(w4 == 4)
  i0 = setdiff(which(w4 == 3), i6)
  
  e = setdiff(seg_6[[i0]], seg_6[[i9]])
  d = setdiff(seg_6[[i9]], seg_6[[i0]])
  c = setdiff(seg_6[[i0]], seg_6[[i6]])
  
  g = setdiff(eg, e)
  b = setdiff(bd, d)
  
  f = setdiff(seq_lets[[which(n_seq == 2)]], c)
  
  lookup = c("a", "b", "c", "d", "e", "f", "g") %>%
    setNames(c(a, b, c, d, e, f, g))
  
  translated = map(
    digits %>% str_split(""),
    ~ sort(lookup[.x]) %>%
      setNames(NULL)
  )
  
  find_digit = function(x) {
    which(map_lgl(segs, ~ identical(.x, x)))-1
  }
  
  map_dbl(translated, find_digit) %>%
    as.character() %>%
    paste(collapse="") %>%
    as.integer()
}

pmap_int(
  input,
  translate_digits
) %>%
  sum()
