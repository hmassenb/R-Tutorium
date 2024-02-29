##############################################################################
#                          Unit 2 - Descriptives                             #
##############################################################################


# Start with loading the packages
# Packages enable specific codes which can help you achieving your goal faster

# For example dplyr can generate, filter and summarize variables (https://dplyr.tidyverse.org/)
install.packages("dplyr")
library(dplyr)

# Things to consider: Order of packages loaded, same command names (fixed by adding e.g. dplyr:: in front)


##############################################################################
# Load data  

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

data$Gender # $-operator links the variable you want to look at with the dataset 


##############################################################################
# Use dplyr 
##############################################################################

# Select certain columns
data_age <- data %>% 
  select(Age)


# What is %>% 

library(magrittr)

# It resembles a pipe and it functions like on
# You can link functions with it, just like a pipe connects two places 
# Like in this examples it connects the select command with the information with which dataset this command should be connected to 


# Filter data
data_W <- data %>% 
  filter(Gender == "W") # creates a subset containing only persons with an assigned W 

data_M <- data %>% 
  filter(Gender == "M")
  
View(data_W)
View(data_M)


# summarize 

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


# Mutate = creates a variable
data <- mutate(data, Gender_binary = ifelse(Gender == "M", 0, 1)) 

# Breakdown of code: 
# 1) data = name of dataset 
# 2) "Gender_binary" = indicates the variable name we want to create
# 3) ifelse = a condition which can be used in creating binary variables (either 0 or 1). This could also be "Age > 18" or whatever you desire 
# 4) Gender == "M" = the condition which has to be fulfilled for the if 
# 5) 0,1 = if the condition is fulfilled the value is "0", else the value is "1" 

# "Place where variable ends"  <- mutate(dataset, new_variable = ifelse(condition, 0, 1)) 


##############################################################################
# Keep data clean 

# round data
mean_age_W <- round(mean_age_W, digits = -1) # -1 remove the decimal after comma 
mean_age_M <- round(mean_age_M, digits = -1)

# Remove objects you dont need anymore 
rm(data_age)


##############################################################################
# Packages are not always the better option!
# Remember: Don't make easy things hard :) 

# The original R has already many useful functions
mean_age_W <- mean(data_W$Age)
mean_age_M <- mean(data_M$Age)
mean_age_W
mean_age_M

sd_age_W <- sd(data_W$Age)
sd_age_M <- sd(data_M$Age)




##############################################################################
# there are seldom right / wrong ways, but more and less efficient ways 
##############################################################################
# Another option to obtain immediately the mean age per group of gender

mean <- data %>%  
  group_by(Gender) %>% 
  summarise(mean = mean(Age))


final_data <- data %>%  
  group_by(Gender) %>%  # All the following commands will be computed by each group (M and W)
  summarise(mean_age = mean(Age), # summarise has more options than only computing the mean
            number = n()) # n() counts the number of observations, in our case how many M and W exist




##############################################################################
# Save data 
##############################################################################
# Nice now we have data, but how to save it for the future?

save(final_data, file = "Unit2_finaldata.csv") 

# Breakdown: save(name_of_dataset, file = "(specify name of saved dataset).(type of dataset)")

# Other way: Save as a .dta file
library(haven) # package to read .dta data (Stata data)
write_dta(data = final_data, "Unit2_finaldata.dta")





##############################################################################
# More useful functions:
##############################################################################

# 1) Cut the variables in parts
breaks <- seq(0, max(data$Age) + 10, by = 10)
data$age_bins <- cut(data$Age, breaks, labels = NULL, dig.lab = 3)

# current labeling is not really useful

# transform a factor variable to a numeric
data$numeric_age_bin <- as.integer(data$age_bins)
View(data$numeric_age_bin)

# this is super important since certain commands are only possible for e.g. numeric values



# 2) (L)Apply 

library(data.table) # other way of storing data
data_table <- data.table(data)

HH_mean <- data_table[, lapply(.SD, mean), by = ID, .SDcols = "Age"] # computes mean age per HH 

# Rename a variable 
HH_mean <- HH_mean %>% rename(mean_age = Age) # we observe the mean now so we should call it also like that









