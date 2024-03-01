################################################################################
################# R-Tutorial WU 2022 - Hannah Massenbauer ######################
########################### Basic R and R-Studio ###############################


################################################################################
# Arithemtic Operations 
################################################################################

# Code in script produce output in console 
  10 + 1
  
  5 - 1
  
  11 * 5
  
  11 / 5

# White spaces don't count
  10 +            1
  10+1
  10   

# Creating variables 
## important since you will mostly works with variables which includes many values (observations of the real world) 
 a <- 10 
 b <- 10 + 1
 
 a
 b
 
 # But R is case-sensitive
  A 
  a
  
# Deleting variables 
  
  remove(a)
  rm(b) # shortcut
  
  
# Calculations with variables 
  
  a <- 10 
  b <- 10 + 1
  
  c <- a + b 
  d <- a/b  
  e <- (a^2) * b # like with math by hand you can use the power operation and brackets
  

 
################################################################################



  
  
  ######################### (Some) Basic functions ###############################
  
  
  x <- c(1,3,4,6.6,8,1) # create variable (in tis case its a vector containing integers)
  
  sort(x) # sort variable
  
  length(x) # length of object 
  
  max(x) # numeric maximum of variable 
  
  min(x) # numeric minimum of variable
  
  unique(x) # unique values in variable
  
  table(x) # frequency table of variable
  
  plot(x) # simple plot with index on x-axis and values on y-axis
  
  summary(x) # summarise info on object, output depends on object!
  
  print(x) # print object 
  
  round(3.5467, digits = 3) # round value, second argument gives decimal place
  
  seq(from=1, to=10, by=3) # create sequence of numbers
  
  ?seq # get information on functions
  
  
  # You do not need to type in the arguments' names if you know their position
  seq(1,10,2)
  




