################################################################################
#                 R-Tutorial WU 2022 - Hannah Massenbauer                      #
#                                Descriptives                                  #
################################################################################


###################### Start with loading the packages ##########################

# Packages enable specific codes which can help you achieving your goal faster

# Most important package (https://tidyverse.org/)

  install.packages("tidyverse") # install package 
  library(tidyverse) # if installed you can simply recall it

# Things to consider: Order of packages loaded, same command names (fixed by adding e.g. tidyverse:: in front)


############################# Load data  #######################################

# set path (to the place were your files are stored)

  getwd() # find out where you are 
  
  setwd("C:\\Users\\hamassen\\Documents\\GitHub\\R-Tutorium") # determine where your files are
  setwd("C:/Users/hamassen/Documents/GitHub/R-Tutorium")

# Note: Windows user have to add a second slash "\\" or change toward forward slash "/"

  
# load data
  
  data <- read.csv("personal_test.csv") 
  
  View(data) # opens tab showing data

# load data such that we can use it 
  
  data <- read.csv("personal_test.csv",  sep = ";", header = TRUE) 

# attention how data is stored_  
  # for csv: read.csv("datasetname.csv")  
  # for xlsx: read.xlsx("datasetname.xlsx")
  # for dta: read_dta("datasetname.dta")

# 1) Understand the data you're using!

  data$Age # results in list
  
  glimpse(data$ID) # displays first few rows
  
  head(data$ID_p) # very similar as glimpse
  
  data$Gender
  

############################ Use dplyr ##################################

############################ 
# Select columns
############################ 

  data_Gender <- data %>% 
    select(Age)

# What is %>% 

  library(magrittr)

# It resembles a pipe and it functions like on
# You can link functions with it, just like a pipe connects two places 
# Like in this examples it connects the select command with the information on which dataset this command should be applied to 
  

############################ 
# Filter data
############################ 
  
  data_W <- data %>% 
    filter(Gender == "W") # creates a subset containing only persons with an assigned W 
  
  data_M <- data %>% 
    filter(Gender == "M")
    
  View(data_W)
  View(data_M)
  
  
############################ 
# summarize 
############################ 
  
# get the mean using the mean function 
  
  mean_age_W <- data_W %>%
    summarize(mean_age = mean(Age))
  
  mean_age_M <- data_M %>%
    summarize(mean_age = mean(Age))

# get standard deviation
  
  sd_age_W <- data_W %>%
    summarize(mean_age = sd(Age))
  
  sd_age_M <- data_M %>%
    summarize(mean_age = sd(Age))
  
  print(mean_age_W$mean_age)
  
  print(mean_age_M$mean_age) 
  

############################ 
# Mutate = creates a variable
############################ 
  
  data <- mutate(data, Gender_binary = ifelse(Gender == "M", 0, 1)) 

# Breakdown of code: 
# 
#   1) data = name of dataset 
#  
#   2) Gender_binary = indicates the variable name we want to create
#  
#   3) ifelse = a condition which can be used in creating binary 
#               variables (either 0 or 1). This could also be "Age > 18" 
#               or whatever you desire 
#  
#   4) Gender == "M" = the condition which has to be fulfilled for the if 
#  
#   5) 0,1 = if the condition is fulfilled the value is "0", else the value is "1" 
#
# "Place where variable is stored"  <- mutate(dataset, new_variable = ifelse(condition, 0, 1)) 


############# Packages are not always the better option! #######################
 
# Remember: Don't make easy things hard :) 

  mean_age_W <- mean(data_W$Age)
  
  mean_age_M <- mean(data_M$Age)
  
  mean_age_W
  mean_age_M
  
  
  sd_age_W <- sd(data_W$Age)
  
  sd_age_M <- sd(data_M$Age)

# there are no wrong ways, but more and less efficient ways

  final_data <- data %>%             # indicates dataset 
    
    group_by(Gender) %>%             # for each group are the next steps computed
    
    summarise(mean = mean(Age),      # compute mean
              
              sd = sd(Age))          # compute standard deviation
  
  # immediately obtain an overview of the age by its mean and standard deviation
  
 
################################ Save data #####################################
  
  # as csv file 
  
  save(final_data, file = "final_data.csv")
  
  # as dta file 
  
  library(haven) # required to use .dta files
  
  write_dta(final_data, file = "final_data.dta")
  



############################ Keep data clean ###################################

  # round values 
  
  mean_age_W <- round(mean_age_W, digits = -1) # remove the decimal after comma 
  
  mean_age_M <- round(mean_age_M, digits = -1)

 # Remove objects you dont need anymore 
  
  rm(data_Gender)
  
  rm(list = ls()) # remove everything in environment


############################################################################### 
  
  
  
  
################################### Exercise ###################################   
  

# Use any dataset from this class
  
# 1) load data
  
  
# 2) Which data do we see? How is the structure? Which variables are used?
  

# 3) How is the data stored? 
  
  
# 4) Compute descriptive values (min, max, mean, median)
  
  
# 5) What can you say about the dispersion of the data? (Tip: either numerically or graphically)
  
  
# 6) Create a dataset containing only the summary statistics per unit (ID, person, HH)
  
  
# 7) Save data set 
  
  
  
  
  
  
  
  
  
  
  
  
  
 