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

  # displays min, max, quartile, mean, median
  summary(data$wage) 
  
  # counts frequency of a variable
  sum(data$construc == 1) 
  
  sum(data$services == 1) 

 # Some new ways to look at data
  sum(c(data$profocc, data$profserv) == 1) # counts the sum of both groups together -> number is rather high, what could be off?
  
  count_both_1 <- sum(data$profocc == 1 & data$profserv == 1) # overlap of both groups
  
  
  summary(data$trade)
  
  summary(data$services)
  
  summary(data$profserv)

# How could we display the data? 

 
###############################  1)  Bar plot  #################################

####### Goal: Get an overview how many workers are in which occupation ######### 
  
  
# 1) Prepare data    
  
  data <- data %>% 
   mutate(occ_type = case_when(         # case when = when a worker is in 
      trade == 1 ~ 1,                   # that category (=1) they get values 
      services == 1 ~ 2,                # (1,2,3) assigned
      profserv == 1 ~ 3))

  
  sum(is.na(data$occ_type))             # sum how many NA's exist 

  # there are observations in neither of these three categories
  # they are labelled as `NA`
  

# Always reflect about what a NA means! 
  
  data$occ_type <- replace(data$occ_type, is.na(data$occ_type), 0)  # in our setting NA means someone is 
                                                                    # in another sector than the three (serv, trade, prof)
  summary(data$occ_type)
  

# 2) Create Plot
 
  ggplot(data,                                   # dataset we use
         aes(x = occ_type)) +                    # we want to see the numbers per occupation type
    geom_bar()                                   # geom_bar gives us a bar plot
  
  # goal achieved, but make it nicer
  
  
# 3) Make your graph enjoyable to look at
 
  library(wesanderson) # package for nice colors
  
  # Create info what the categories mean 
  data$occ_type <- factor(data$occ_type, levels = c(0, 1, 2, 3), labels = c("Other", "Trade", "Services", "Prof serv")) # create labels
  

  
  # plot it again 
  ggplot(data, aes(x = occ_type, fill = factor(occ_type))) +    # in factor () we add the prior created info on categories
    geom_bar() +                                                # type of graph 
    scale_fill_manual(values = wes_palette("Zissou1", n = 4)) + # changes color
   
    # changes concerning look of graph 
    labs(title = "Bar Plot of workers in different sectors",    # add title 
         x = "Occupation Type",                                 # add titles to axis
         y = "Count") +
    guides(fill = guide_legend(title = "Occupation Type")) +  # Add legend title
    theme_minimal() +
    theme(legend.position = "bottom",                         # Move legend to bottom
          legend.box = "horizontal",                          # Arrange legend items horizontally
          legend.key.size = unit(1, "cm"),               
          plot.title = element_text(hjust = 0.5))             # Center the title


  
  
 
##############################  2) Line plot  ##################################
 
# Goal: create average growth of by experience
  
  

# 1) Create the data we need
  
  data <- data %>% 
    group_by(exper) %>%               # group_by = creates per group 
    mutate(mean_wage = mean(wage))    # we create the mean for each year of experience
  
  

# 2) Plot 
  
  ggplot(data, aes(x = exper, y = mean_wage)) + 
    geom_line() +                                                 # geom_line = line plots
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +  # Wes Anderson color palette
    labs(title = "Line Plot of Wages",
         x = "Years of Experience",
         y = "Wages") +
    theme_minimal() +
    theme(legend.position = "bottom",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5))  
  
  
# smoothed line plot 
  
  ggplot(data, aes(x = exper, y = mean_wage)) +
    geom_smooth(method = "loess", se = FALSE) +                   # geom_smooth = smoothed line plot
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +  # n = 4, for each category one color   
    labs(title = "Smooth Line Plot of Wages",
         x = "Years of Experience",
         y = "Wages") +
    theme_minimal() +
    theme(legend.position = "bottom",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5)) 
  
  # make trends more clear 
  
  
# 3) Investigate in Heterogeneities: 
  
# you can also create lines per group 
  
  data <- data %>% 
    group_by(exper, occ_type) %>%  # group by applied per experience within each occupation type
     mutate(mean_wage_perocc = mean(wage))
    
   
  # Plot wage across experience 
  
  ggplot(data, aes(x = exper, y = mean_wage_perocc, color = occ_type)) +
    geom_smooth(method = "loess", se = FALSE) +                     # method loess = non-linear line, se=FALSE - removes band 
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +     
    labs(title = "Average Wage Growth per Occupation Group",
         x = "Years of Experience",
         y = "Wages") +
    guides(color = guide_legend(title = "Occupation Type")) +       # Change the legend title
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0.5))
  
  
  # Reveals the heterogeneity across groups
  
  # Info on geom_smooth(method = "")
  # method = exist various options, lm for example would be a linear fit. 
  # More info: https://ggplot2.tidyverse.org/reference/geom_smooth.html
  
#########################  3) Scatter plot  ####################################

  
# Goal: Have younger workers more education? 
  
# How to approach? 
# We don't have the age but use experience as an approximation for the age of workers
  

# Efficiency tip 1: Predefine color scheme 
  
  color <- wes_palette("GrandBudapest1", n = 2)
  
  
  ggplot(data, aes(x = exper, y = educ)) +
    geom_point(color = color[1]) +                                  # using the first color [1] of our predefined colors
    geom_smooth(method = "loess", se = FALSE, col = color[2]) +     # using the second color [2]
    labs(title = "Scatter Plot of Experience and Education",
         x = "Years of Experience",
         y = "Education") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
           plot.title = element_text(hjust = 0.5))
  
  

#########################  4) Density plot  ####################################  

# Goal: Is education equally distributed?
  
# How things are distributed is an important part of improving our economic understanding
  
  
  
# Efficiency tip 2: Predefine save plots
  
  save_plot <- function(plot, filename) {                               # creating own functions can be super useful, but in the beginning copy-paste 
    file_path <- file.path(                                           
      "C:\\Users\\hamassen\\Documents\\GitHub\\R-Tutorium"  # insert your path here
      
      , paste0(filename, ".pdf"))                                        # filename is still variable and will be defined later on
                                                                         #  ".pdf" defines in which format you store the graph
    
    ggsave(file_path, plot, width = 10, height = 6, dpi = 300)           # some predefined options how graph is stored
  }
  
# You can reuse this function, just adapt the path where its stored
  
  
# 1) Plot the distribution 
  
  distribution <- 
    ggplot(data, aes(x = educ)) +
    geom_density(fill = "skyblue", color = "blue", alpha = 0.7) +     # geom_density = distribution plot
                                                                      # alpha regulates transparency of color - try it out :) 
    theme_minimal() +
    labs(title = "Distribution of Education", 
         x = "Years of Education",
         y = "Density") + 
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0.5))
  
  
 # 2) save plot  
  
   save_plot(distribution, "distribution") 
   
   # We can assign graphs to names like we did with variables
   # This is done with  [distribution <-]
   # Then we use our predefined function , where we have to fill in now simply the name how the graph is stored
   # And secondly, we define the name how the variable will be stored on the computers
   
   
  
################################################################################
#                      Always reflect what the goal is                         #
#                   and what the easiest way is to reach it                    # 
################################################################################
  
  
  
  
  ################################### Exercise ###################################   
  
  
  # Use any dataset 
  
  library(help='datasets') # datasets directly included in R
  
  # pick any (Tip to load data: data <- Name of Dataset)
  
  
  
  # 1) Load data
  
  
  # 2) Which data do we see? How is the structure? Which variables are used?
  
  
  # 3) Describe what you want to plot
  
  
  # 4) Create an appropriate plot 
  
  
  # 5) Make some modifications to the plot (colors, legend position, labelling of axis)
  
  
  # 6) Can you think of subgroups that are differently affected?
  
  
  # 7) Export graph 
  
  
  
   