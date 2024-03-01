##############################################################################
#                        Unit 3 - Graphs with ggplot                         #
##############################################################################

# load packages
  library(dplyr)
  library(magrittr)
  library(ggplot2)

# set path 

  setwd("C:\\Users\\hamassen\\Documents\\GitHub\\R-Tutorium")
  

# load data
  
  data <- readRDS("wage1.RData") 

# a new command to load data since we are using another kind of dataset .RData!

############################# Get to know data #################################

  summary(data$wage) # displays min, max, quartile, mean, median
  
  sum(data$construc == 1) # displays sum of all construction workers
  
  sum(data$services == 1) 

# Some new ways to look at data
  sum(c(data$profocc, data$profserv) == 1) # counts the sum of both groups together -> number is rather high, what could be off?
  
  count_both_1 <- sum(data$profocc == 1 & data$profserv == 1) # overlap of both groups
  
  
  summary(data$trade)
  
  summary(data$services)
  
  summary(data$profserv)

# How could we display the data? 

 
############################### 1)  Bar plot  ##################################
    
  data <- data %>% 
   mutate(occ_type = case_when(
      trade == 1 ~ 1,
      services == 1 ~ 2,
      profserv == 1 ~ 3))
  
# how many NA's exist 
  
  sum(is.na(data$occ_type)) 

  # there are observations in neither of these three categories
  # they are labelled as `NA`
  

# Always reflect about what a NA means! (look either into codebook or other source of dataset) 
  
  data$occ_type <- replace(data$occ_type, is.na(data$occ_type), 0) 
  
  # in our setting NA means someone is in another sector than the three (serv, trade, prof)
  
  summary(data$occ_type)
  

# We want to observe data across categorial variables (occupation type)
 
  ggplot(data, aes(x = factor(occ_type), fill = factor(occ_type))) +
    geom_bar() +
    labs(title = "Bar Plot of Values",
         x = "Occupation Type",
         y = "Count") +
    theme_minimal()
  
  
  # good, but make it nicer looking 
 
  library(wesanderson) # package for nice colors
  
  # Create info what the categories mean 
  data$occ_type <- factor(data$occ_type, levels = c(0, 1, 2, 3), labels = c("Other", "Trade", "Services", "Profserv")) # create labels
  
  
  # plot it again 
  ggplot(data, aes(x = occ_type, fill = factor(occ_type))) +    # in factor () we add the prior created info on categories
    
    geom_bar() +                                                # type of graph 
    
    scale_fill_manual(values = wes_palette("Zissou1", n = 4)) + # changes color
    
    labs(title = "Bar Plot of Values",                          # add title 
         x = "Occupation Type",                                 # add titles to axis
         y = "Count") +
    
    guides(fill = guide_legend(title = "Occupation Type")) +  # Add legend title
    theme_minimal() +
    theme(legend.position = "bottom",                         # Move legend to bottom
          legend.box = "horizontal",                          # Arrange legend items horizontally
          legend.key.size = unit(1, "cm"),               
          plot.title = element_text(hjust = 0.5))             # Center the title


  
  
 
##############################  2) Line plot  ##################################
 
  # create mean wage by experience
  
  
  data <- data %>% 
    group_by(exper) %>% 
     mutate(mean_wage = mean(wage))
    
   
  # Plot wage across experience 
  
  ggplot(data, aes(x = exper, y = mean_wage, color = occ_type)) +
    geom_line() +  
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +  # Wes Anderson color palette
    labs(title = "Line Plot of Wages",
         x = "Years of Experience",
         y = "Wages") +
    theme_minimal() +
    theme(legend.position = "bottom",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5))  
  
  
  # error? What is the cause? 
  
  
  data$occ_type <- as.factor(data$occ_type)  
  # store variable as factor is one solution!
  
  
  # use only wage of professional service
  
  data_prof <- data %>% 
    filter(occ_type == 3 )
  
  # create mean wage only for profserv
  data_prof <- data_prof %>% 
    mutate(mean_wage = mean(wage))
  
  # mean wage within profession
  data_prof <- data_prof %>% 
    group_by(exper) %>% 
    mutate(mean_wage_prof = mean(wage))
  
  
  ggplot(data_prof , aes(x = exper, y = mean_wage_prof, color = occ_type)) +
    geom_line() +  
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +  
    labs(title = "Mean wages in professional service",
         x = "Years of Experience",
         y = "Wages") +
    theme_minimal() +
    theme(legend.position = "none",       
          plot.title = element_text(hjust = 0.5)) 
  
  
  
  ##############################################################################
  # 3) Scatter plot 
  ##############################################################################
  
  
  
  
  
  
  
  
  
# Themes -----------------------------------------------------------------------

# Themes change the overall look of your plot, e.g. background, grid, axis

p +
  theme_gray()

p +
  theme_bw()

p +
  theme_linedraw()

p +
  theme_light()

p +
  theme_dark()

p +
  theme_minimal()

p +
  theme_classic()

p +
  theme_void()
