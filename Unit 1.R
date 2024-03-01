################################################################################
#                 R-Tutorial WU 2022 - Hannah Massenbauer                      #
#                           Basic R and R-Studio                               #
################################################################################


####################### Arithmetic Operations ##################################


# Code in script produce output in console 
  10 + 1
  
  5 - 1
  
  11 * 5
  
  11 / 5

# White spaces don't count
  10 +            1
  10+1
  10   

################################# Variables ###################################
  
# important since you will mostly works with variables 
# which includes many values (observations of the real world) 
  
 a <- 10 # <- assigns values to a character
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
  

 
############################# Vectors and matrices #############################

  # Vector 
  
  k1 <- c(1,1,2,3,3,3,4) # in this case its a vector containing integers (whole number, without decimals)
  
  # Breakdown. c binds several values together and with the <- we assign the vector to the name "k1"
  
  # Basically, all variables are vectors, when you think about it. You observe one characteristica e.g. age (1) for n people 
  # this results in a (n x 1) vector

  

  # Matrices 
  
  # create some more vectors
  k2 <- c(2,3,5,6,6,8,7) 
  k3 <- c(4,8,2,6,4,4,0)
  
  # bind vectors together
  matrix <- cbind(k1,k2,k3) # 7 x 3 matrix 
  
  # cbind command = binds columns together. Watch out, they have to be of equal length!

  
  # Datasets are a matrix
  # Give columns new meaning 
  colnames(matrix)[colnames(matrix) == "k1"] <- "ID" # implies how many kids exist per HH
  colnames(matrix)[colnames(matrix) == "k2"] <- "Age" # Age of kid
  colnames(matrix)[colnames(matrix) == "k3"] <- "Books" # number of books per kid 
  
  # Breakdown of code: change "colnames" of "matrix" in the square brackets we 
  #                    enter information which column we want to change &
  #                    we assign a new name, just like we assigned values to 
  #                    letters by adding "<- new_name"
  
  
  Books_children <- matrix # rename matrix 
  
  # Now you can interpret the values differently -> they got a meaning
  
  Books_children$Age # Doesn't work yet
  
  
  ################ Intro to various ways to store datasets in R   ############### 
  
  # Data frame 
  df <- data.frame(Books_children) 
  
  # Data table 
  dt <- data.table::data.table(Books_children)
  
  # very similar in its look, but there are differences:
  # 1) Data frame is included in base R
  # 2) Data table is a package (Next week more details on it!) and is optimized,
  #    which is especially, important when working with large datasets.
  
  df$Age # now it works :) 
  
  # R stores information differently, depending on how you create and store objects
  
  # class command = super useful to understand how objects are stored
  
  class(Books_children)
  
  class(df)
  
  class(dt)

  
################# Intro to various variables of datasets in R   ################

  # numbers   
  
  a <- 1
  
  b <- 1.1
  
  # What about other characters?
  
  c <- "Hi" 
  
  # For other forms of information we can use "" to mark that its not a number
  
  
  # class can be also applied to variables
  
  class(a)
  
  class(c)
  
  
########################### Understanding dataset  ###########################
  
 
# 1) table  
  
  table(Books_children) 
  
  # shows all elements of our data

   
# 2) sort 
  
  sort(Books_children$Age) # ! 
  
# What could be wrong? 
  sort(?$Age)
 
    
# 3) Descriptive basic  
 
  max(df$Age) # maximum age
  
  min(df$Age) # minimum age
  
  summary(df)   # info of whole data frame 
  
  summary(df$Kids.per.HH) # info about one variable of dataset
  
  
# 4) Plot 
 
# Patterns in data are easier to catch when you can see it 
  
  plot(Books_children)  # What can you see? How does that relate to our data? 
  
  
  # Choose variables for plot
  
  plot(df$Age, df$Books) 
  
  # recall R demands for many functions that the data is stored as a data frame
  # therefore, i use 
  
  
  # you can modify your graphs
  
  plot(df$Age, df$Books,
       pch = 16,          # Fills circles
       col = "darkgreen",   # Point color
       xlab = "Age",      # x-axis label
       ylab = "Number of Books",  # y-axis label
       main = "Books by Age",       # Title
       las = 1             # Rotate y-axis labels by 90 degrees
  )
  
  # Plots should be as self-explaining as possible!



