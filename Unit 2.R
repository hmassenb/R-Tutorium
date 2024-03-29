################################################################################
#                 R-Tutorial WU 2022 - Hannah Massenbauer                      #
#                             Unit 2 - Descriptives                            #
################################################################################


######################   Start with loading packages ##########################


  install.packages("tidyverse") 

# install package - can also be searched for in the right bottom window under "packages"


# if installed you can simply recall packages
  library(tidyverse) 

  # Tip: 
  # Look into the bottom right window and go to "Packages" -> "Install" and then
  # search for the package. Under "package" you can also tick the check box 
  # instead of the library() command


# Things to consider: Order of packages loaded, same command names (fixed by adding e.g. tidyverse:: in front)



############################## Get path  #######################################

# set path (to the place were your files are stored)

  getwd() # find out where you are 
  
  setwd("C:\\Users\\Hannah\\Documents\\GitHub\\R-Tutorium") # determine where your files are
  setwd("C:/Users/hamassen/Documents/GitHub/R-Tutorium")


# Note: Windows user have to add a second slash "\\" or change toward forward slash "/"

  
############################# Load data  #######################################
  
  data <- read.csv("./data/personal_test.csv") 
  
  # Paths and Subfolders: 
  # I store my data in a subfolder called "data". 
  # As the path chosen in setwd leads only to the general Tutorium folder I have to tell R to use the datafolder
  
  # ./ = use prespecified path
  # data/ = specify the subfolder we want to access
  # personal_test.csv = dataset we want to laod
  
  
  
  View(data) # opens tab showing data
  

# load data such that we can use it
  
  data <- read.csv("./data/personal_test.csv",  sep = ";", header = TRUE) 
  
  # sep = what seperates data, 
  # header = TRUE implies using the var names

# attention how data is stored:  
  # for csv: read.csv("datasetname.csv")
  # for xlsx: read.xlsx("datasetname.xlsx")
  # for dta: read_dta("datasetname.dta")  
  
  

# 1) Understand the data you're using!

  data$Age # results in list
  
  glimpse(data$ID) # displays first few rows
  
  head(data$ID_p) # very similar as glimpse
  
  data$Gender
  

#################################  Use dplyr  ##################################

###############################  Select columns  ###############################


  data_Gender <- data %>% 
    select(Age)

# What is %>% 

  library(magrittr)

# It resembles a pipe and it functions like on
# You can link functions with it, just like a pipe connects two places 
# Like in this examples it connects the select command with the information on which dataset this command should be applied to 
  
 
################################# Filter data  ################################# 

  
  data_W <- data %>% 
    filter(Gender == "W") 
  
  # filter = creates a subset containing only observations where condition is true
  
  
  data_M <- data %>% 
    filter(Gender == "M")
    
  View(data_W)
  View(data_M)
  
  
############################## Summarize data  ################################# 
  
# Mean 
  
  mean_age_W <- data_W %>%
    summarize(mean_age = mean(Age)) # calculate the mean_x = mean(x) 
  
  mean_age_M <- data_M %>%
    summarize(mean_age = mean(Age))

  # mean() resembles a function in R. 
  # function = define what R should do with an input
  # depending on function you receive specified output for a certain input
  # E.g. mean function = for a certain variable (Age) you get as output the mean of Age

  
# Standard deviation
  
  sd_age_W <- data_W %>%
    summarize(sd_age = sd(Age))
  
  sd_age_M <- data_M %>%
    summarize(sd_age = sd(Age))
  
  # here we use the function sd() to obtain the standard deviation of age
  
  
# median 
  
  med_age_W <- data_W %>% 
    summarize(med_age = median(Age))
  

  
# quartiles (25th, 75th)
  
  q25_age <- quantile(data$Age, probs = 0.25)
  
  q75_age <- quantile(data$Age, probs = 0.75)
  
  iqr <- data %>% 
    summarize(iqr = IQR(Age))
    
    
# Look at the output
  
  print(mean_age_W$mean_age)
  
  print(sd_age_W)
  

############################### Mutate data #################################### 

  # creates new variable
  
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

# Base R offers already some straightforward options 
  
  mean_age_W <- mean(data_W$Age)
  
  mean_age_M <- mean(data_M$Age)
  
  sd_age_W <- sd(data_W$Age)
  
  sd_age_M <- sd(data_M$Age)
  

# there are not really wrong ways, but more and less efficient ways

  final_data <- data %>%             # indicates dataset 
    
    group_by(Gender) %>%             # for each group are the next steps computed
    
    summarise(mean = mean(Age),      
              sd = sd(Age),
              var = var(Age))          
  
  
  # immediately obtain an overview of the age by its mean and standard deviation
  
 
  
################################ Save data #####################################
  
  # as csv file 
  
  save(final_data, file = "final_data.csv")
  
 
  # Breakdown: save(datset name in R, file = "(specifiy how it shall be stored).(dataset type)")
  
  
  # as dta file
  
  save(final_data, file = "final_data.dta") 
  # will not produce an error, but cannot be used
  
  
  # Load package for .dta data
  library(haven) 
  
  fail <- read_dta("final_data.dta")
  
  
  # predefining paths can be helpful 
  
  path_data <- "C:/Users/hamassen/Documents/GitHub/R-Tutorium/Data" # tell R where to store data
  
  write_dta(final_data, file.path(path_data, "final_data.dta"))
  
  succesful <- read_dta("./Data/final_data.dta")
  

############################ Keep data clean ###################################

  # round values 
  
  mean_age_W <- round(mean_age_W, digits = -1) # remove the decimal after comma 
  
  mean_age_M <- round(mean_age_M, digits = 2) # allow to decimals after comma
  

 # Remove objects you don't need anymore 
  
  rm(data_Gender)
  
  rm(list = ls()) # remove everything in environment
  
  # Remove last column in dataset 
  
  reduced_data <- subset(
                final_data,                        # subset command selects columns
                select = -c((ncol(final_data))))   #  -c indicates deleting last column from final data
  
  
  # Remove range of columns in dataset
  
  reduced_data <- subset(final_data, 
                         select = -c((ncol(final_data)-1):ncol(final_data))) 
                      
  # (n-1) : n columns get deleted = last two 
  
  
  
########################   Missing variables  ################################## 
  

  # Drop whole observation 
  
  vector_NA <- c(2, NA, 4, 6,34, NA, NA)
  
  vector_cleaned <- na.omit(vector_NA)
  
  # Dropping whole observation is only advisable if the variable missing is very important to you
  # e.g. if its your variable of interest to explain
  
  
  # A more elegant way is the option "na.rm"
  
  mean_NA <- mean(vector_NA)   # we cannot leave NA's in sample
  
  mean_clean <- mean(vector_NA, na.rm = TRUE)  # we remove them with na.rm
  
  
  
  ################################### Exercise ###################################   
  
  
  # Use any dataset 
  
  library(help='datasets') # datasets directly included in R
  
  # pick any (Tip to load data: data <- Name of Dataset)
  
  # To check what variables are included use head(dataset name)
  
  
  # 1) load data
  
  
  # 2) Which data do we see? How is the structure? Which variables are used?
  
  
  # 3) How is the data stored? 
  
  
  # 4) Compute descriptive values (min, max, mean, median)
  
  
  # 5) What can you say about the dispersion of the data? (Tip: either numerically or graphically)
  
  
  # 6) Create a dataset containing only the summary statistics per unit (ID, person, HH)
  
  
  # 7) Save data set 
  
  
  
  
  
  
  
  
  
  
  
  
  
 