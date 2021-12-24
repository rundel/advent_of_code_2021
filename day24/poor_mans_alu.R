alu = function(instr, input_stack) {
  reg  = c(w=0, x=0, y=0, z=0)
  
  for(cmd in instr) {
    #print(cmd)
    if (cmd[1] == "inp") {
      reg[cmd[2]] = input_stack[1]
      input_stack = input_stack[-1]
    } else if (cmd[1] == "add") {
      if (str_detect(cmd[3], '\\d+'))
        reg[cmd[2]] = reg[cmd[2]] + as.numeric(cmd[3])
      else
        reg[cmd[2]] = reg[cmd[2]] + reg[cmd[3]]
    } else if (cmd[1] == "mul") {
      if (str_detect(cmd[3], '\\d+'))
        reg[cmd[2]] = reg[cmd[2]] * as.numeric(cmd[3])
      else
        reg[cmd[2]] = reg[cmd[2]] * reg[cmd[3]]
    } else if (cmd[1] == "div") {
      if (str_detect(cmd[3], '\\d+'))
        reg[cmd[2]] = floor(reg[cmd[2]] / as.numeric(cmd[3]))
      else
        reg[cmd[2]] = floor(reg[cmd[2]] / reg[cmd[3]])
    } else if (cmd[1] == "mod") {
      if (str_detect(cmd[3], '\\d+'))
        reg[cmd[2]] = reg[cmd[2]] %% as.numeric(cmd[3])
      else
        reg[cmd[2]] = reg[cmd[2]] %% reg[cmd[3]]
    } else if (cmd[1] == "eql") {
      if (str_detect(cmd[3], '\\d+'))
        reg[cmd[2]] = reg[cmd[2]] == as.numeric(cmd[3])
      else
        reg[cmd[2]] = reg[cmd[2]] == reg[cmd[3]]
    }
    #cat(cmd, '|', reg,"\n")
  }
  
  reg
}


c("inp x","mul x -1") %>% str_split(" ") %>%
  alu(5)

c("inp z","inp x","mul z 3","eql z x")  %>% str_split(" ") %>%
  alu(c(1,3))

alu(test, 5)

#alu(input, "13579246899999" %>% str_split("") %>% unlist() %>% as.numeric())


for(i in 1:9) {
  alu(input[1:18], i) %>% print()
}
