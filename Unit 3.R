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
  data$occ_type <- factor(data$occ_type, levels = c(0, 1, 2, 3), labels = c("Other", "Trade", "Services", "Prof serv")) # create labels
  

  
  # plot it again 
  ggplot(data, aes(x = occ_type, fill = factor(occ_type))) +    # in factor () we add the prior created info on categories
    geom_bar() +                                                # type of graph 
    scale_fill_manual(values = wes_palette("Zissou1", n = 4)) + # changes color
   
    # changes concerning look of graph 
    labs(title = "Bar Plot of workers in different Sectors",    # add title 
         x = "Occupation Type",                                 # add titles to axis
         y = "Count") +
    guides(fill = guide_legend(title = "Occupation Type")) +  # Add legend title
    theme_minimal() +
    theme(legend.position = "bottom",                         # Move legend to bottom
          legend.box = "horizontal",                          # Arrange legend items horizontally
          legend.key.size = unit(1, "cm"),               
          plot.title = element_text(hjust = 0.5))             # Center the title


  
  
 
##############################  2) Line plot  ##################################
 
  # create mean wage growth by experience
  
  data <- data %>% 
    arrange(exper)
  
  data <- data %>% 
    group_by(exper) %>%  # group by applied per experience within each occupation type
    mutate(mean_wage = mean(wage))
  
  
  # Plot wage across experience 
  
  ggplot(data, aes(x = exper, y = mean_wage)) +
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
  
  
  # smoothed line plot 
  ggplot(data, aes(x = exper, y = mean_wage)) +
    geom_smooth(method = "loess") +                               # change geom option
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +  # Wes Anderson color palette
    labs(title = "Line Plot of Wages",
         x = "Years of Experience",
         y = "Wages") +
    theme_minimal() +
    theme(legend.position = "bottom",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5)) 
  
  # make trends more clear 
  
  

# you can also gen lines per group 
  data <- data %>% 
    group_by(exper, occ_type) %>%  # group by applied per experience within each occupation type
     mutate(mean_wage_perocc = mean(wage))
    
   
  # Plot wage across experience 
  
  ggplot(data, aes(x = exper, y = mean_wage_perocc, color = occ_type)) +
    geom_smooth(method = "loess", se = FALSE) +                     # method loess = non-linear line, se=FALSE - removes band 
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +    # n=4, for each category one color
    labs(title = "Line Plot of Wages per Occupation Group",
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

  
# Have younger workers more education? 
# We don't have the age but use experience as an approximation for the age of workers
  
  # Predefining color scheme before creating the graph
  
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

# How variables are distributed is an important part of improving our economic understanding
  
  ggplot(data, aes(x = educ)) +
    # Here we define that we obtain a density plot
    geom_density(fill = "skyblue", color = "blue", alpha = 0.7) +  # alpha regulates transparency of color - try it out :) 
    theme_minimal() +
    labs(title = "Density Plot", 
         x = "Years of Education",
         y = "Density") + 
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0.5))
  
  
  # Background info: 
  # The density used is the kernel density, which is useful to observe distributions
  # In its use its similar to a histogram, but it allows for a smooth distribution
  # This can be useful for continuous variables
  # For categorical variables I would stick to histograms / bar plots
  
  # For those who are more interested in this non-parametric tool: 
  # https://www.nbi.dk/~koskinen/Teaching/AdvancedMethodsInAppliedStatistics2021/Lecture8_KDE.pdf
  
  
################################################################################
#                      Always reflect what the goal is                         #
#                   and what the easiest way is to reach it                    # 
################################################################################
  
   