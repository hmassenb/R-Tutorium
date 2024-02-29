##############################################################################
#                        Unit 3 - Graphs with ggplot                         #
##############################################################################

library(dplyr)
library(magrittr)
library(ggplot2)

setwd("C:\\Users\\hamassen\\Documents\\GitHub\\R-Tutorium")
data <- readRDS("wage1.RData") 

# a new command to load data since we are using another kind of dataset .RData!

##############################################################################
# Get to know data
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

##############################################################################
# 1)  Bar plot 
##############################################################################
data <- data %>% 
 mutate(occ_type = case_when(
    trade == 1 ~ 1,
    services == 1 ~ 2,
    profserv == 1 ~ 3))

# there are observations in neither of these three categories
# they are labelled as `NA`

# Always reflect about what a NA means 
  data$occ_type <- replace(data$occ_type, is.na(data$occ_type), 0) # in our setting NA means someone is in another sector than the three (serv, trade, prof)
  summary(data$occ_type)

# We want to observe data across categorial variables (occupation type)
 
  ggplot(data, aes(x = factor(occ_type), fill = factor(occ_type))) +
    geom_bar() +
    labs(title = "Bar Plot of Values",
         x = "Occupation Type",
         y = "Count") +
    theme_minimal()
  
  
  # nice, but make it nicer
  
  library(viridisLite) # get nice colors
  library(viridis)
  library(wesanderson)
  
  data$occ_type <- factor(data$occ_type, levels = c(0, 1, 2, 3), labels = c("Other", "Trade", "Services", "Profserv")) # get nice labels
  
  
  ggplot(data, aes(x = occ_type, fill = factor(occ_type))) +
    geom_bar() +  
    scale_fill_manual(values = wes_palette("Zissou1", n = 4)) + # changes color
    labs(title = "Bar Plot of Values",
         x = "Occupation Type",
         y = "Count") +
    guides(fill = guide_legend(title = "Occupation Type")) +  # Add legend title
    theme_minimal() +
    theme(legend.position = "bottom",        # Move legend to bottom
          legend.box = "horizontal",         # Arrange legend items horizontally
          legend.key.size = unit(1, "cm"),   # Adjust legend key size
          plot.title = element_text(hjust = 0.5))  # Center the title


  
  
  ##############################################################################
  # 2) Line plot 
  ##############################################################################
  
  data <- data %>% 
    group_by(exper) %>% 
     mutate(mean_wage = mean(wage))
    
            
  ggplot(data, aes(x = exper, y = mean_wage, color = occ_type)) +
    geom_line() +  
    scale_color_manual(values = wes_palette("Zissou1", n = 4)) +  # Wes Anderson color palette
    labs(title = "Line Plot of Wages",
         x = "Years of Experience",
         y = "Wages") +
    theme_minimal() +
    theme(legend.position = "bottom",        # Move legend to bottom
          legend.box = "horizontal",         # Arrange legend items horizontally
          legend.key.size = unit(1, "cm"),   # Adjust legend key size
          plot.title = element_text(hjust = 0.5))  # Center the title
  
  
  
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
