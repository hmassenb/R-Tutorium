################################################################################
#                                 Unit 4                                       #
#                           Basic Econometrics                                 #
################################################################################

# Load Packages

library(stargazer) # required to export regression tables
library(ggplot2)
library(plm)       # fixed effects 
library(dplyr)
library(magrittr)
library(gridExtra) # used to place graphs in certain order e.g. next to each other




############################ Get to know data ##################################


  setwd("C:\\Users\\hamassen\\Documents\\GitHub\\R-Tutorium")
  data <- load("participants.csv")


# take a look at data

  summary(participants)

# what can you say about median, min, max, outliers?
  
# before 
  ggplot(participants, aes(x = r_skills_before, fill = program)) +
    geom_density(alpha = 0.5) +
    labs(title = "Kernel Density Estimate of R Skills Before",
         x = "R Skills",
         y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0.5))

# after
  ggplot(participants, aes(x = r_skills_after, fill = program)) +
    geom_density(alpha = 0.5) +
    labs(title = "Kernel Density Estimate of R Skills Before",
         x = "R Skills",
         y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0.5))



########################## Linear Regression Model ############################# 

# Goal: We are interested in how the skills change by attending the R-Tutorium 
  
  
  # Estimate OLS

  linear_model <- lm(r_skills_change ~ tutorium_attendance, 
                     data = participants)

# Breakdown: 
# 1) Estimated value (y_hat) = r_skills_change  
# 2) Explanatory / independent variable (x_i) = tutorium attendance
# 3) data = our dataset (participants)
  

  # Print the regression outcome 
  summary(linear_model)

  
  
#################  How to add more independent variables  ################# 
  
  # We can add multiple explanatory variables with "+"
  
  linear_model_2 <- lm(r_skills_after ~ tutorium_attendance + learning_hours, data = participants)
  
  summary(linear_model_2)
  
  
  
#################  Export regression table  ################# 

# Efficiency tip:   
  # Export regression results to LaTeX for Overleaf
  
  stargazer(linear_model_2, title = "Regression Results", header = FALSE, type = "latex", out = "regression_results.tex")
  
  
  # Export regression results to Word
  
  stargazer(linear_model_2, title = "Regression Results", header = FALSE, type = "text", out = "regression_results.docx")
  
  

########################### Fixed Effects Model ################################ 

  
  # Story: 
  # Students from different programs have different skills and experience
  # It is hard to catch all these factors, but it is likely that students of 
  # the same program exhibit similar factors -> it is time-invariant and group-
  # specific. Thus, we can use program fixed effects to capture these unobserved
  # characteristics

  fe_reg_1 <- plm(r_skills_change ~ tutorium_attendance,    # plm package
                      data = participants,             
                      index = c("program"),            # we have to indicate the level on which we apply fixed effects
                      model = "within")                # within groups we are considered with unobserved characteristics
  
  summary(fe_reg_1)


# Add a second independent variable 

  fe_reg_2 <- plm(r_skills_change ~ tutorium_attendance + learning_hours , 
                  data = participants,             
                  index = c("program"),            
                  model = "within")                
  
  summary(fe_reg_2)


############################ What is the difference? ###########################


# Create scatter plots to illustrate 

# Regular OLS model 
  
model1 <-  ggplot(participants, aes(x = learning_hours, y = r_skills_change)) +
    
    geom_point() +                                          # scatter plot layer
    
    # Add a second layer to show slope of OLS model
    geom_smooth(method = "lm", se = FALSE) +                # linear fit, resembles beta in regression
    labs(title = "OLS",
         x = "Learning hours",
         y = "R Skills Change") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  

# Apply within transformation 
  
model2 <-  ggplot(participants, 
                  aes(x = learning_hours, y = r_skills_change, 
                       color = program)) +             # differentiate color by program
  
               geom_point() +                           # First layer: scatter plot   
  
               geom_smooth(method = "lm", se = FALSE) + # Second layer: Slope per program
  
                  labs(title = "Fixed effect",
                   x = "Learning hours",
                   y = "R Skills Change") +
                  guides(color = guide_legend(title = "Program")) +   
                  theme_minimal() +
                  theme(legend.position = "bottom",
                        plot.title = element_text(hjust = 0.5))
  


###############   Combines graphs next to each other  ########################## 


# Grid.arrange allows you to place several graphs next to each other

  grid.arrange(
    arrangeGrob(model1),     # add graph 1
    
    arrangeGrob(model2),     # add graph 2
    
    ncol = 2,                # define amount of columns
    
    top = "Impact of Learning hours on R Skills Change"
  )



##########################  Hypothesis Test  ###################################
  
  
#########################  One sided T-test ########################    

# Null Hypothesis (H0): The mean change in R skills is less than or equal to 5.
# Alternative Hypothesis (H1): The mean change in R skills is greater than 5.


 # value we assume to be population mean

  population_mean <- 5                             


# Numerical output

  t_test <- t.test(participants$r_skills_change, mu = population_mean, alternative = "less") 
  
  t_test
  
 


# To make comparisons of means more tangible we compute sample mean explicitly 
  
  sample_mean <- mean(participants$r_skills_change)


# Graphical output
  
  ggplot(participants, aes(x = r_skills_change)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    
    # This resembles our H0 line 
    geom_vline(xintercept = population_mean, linetype = "dashed", color = "darkgreen", size = 1) +
    annotate("text", x = population_mean + 0.8 , y = 0.02, label = "Null Hypothesis", color = "darkgreen", vjust = -7) +
    
    # This resembles sample mean line
    geom_vline(xintercept = mean(participants$r_skills_change), linetype = "dashed", color = "darkblue", size = 1) +
    annotate("text", x = sample_mean - 0.7 , y = 0.02, label = "Sample mean", color = "darkblue", vjust = -2 ) +
    
    labs(title = "Is the change in R-skills greater 5?",
         x = "R Skills Change",
         y = "Density") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  
# We can reject the null hypothesis, since the mean seems to be smaller than 5 

  
 
# For completion, there exist also a two-sided t-test. 
  
# E.g. Is the change in R-skills unequal 0? 
  
# H0 : R-skill-change is unequal 0 
  
  

######################### Two-sample T-test ####################################    
  
# Goal: Are the r skill changes of economics and philosophy students different?
  
  # Create subsets for philosophy and economics students
  
  philosophy_skill_change <- participants[participants$program == "Philosophy", "r_skills_change"] # selects r_skills_change if the program is philo
  
  economics_skill_change <- participants[participants$program == "Economics", "r_skills_change"]   # -II- program is economics
  

  
# Now we test whether the means of the two groups are equal or not 
  
  t_test_result <- t.test(philosophy_skill_change, economics_skill_change) 
  
  

# Create a density plot for illustration
  
  # 1) Create a combined data set two plot them in one graph together 
  
  combined_data <- data.frame( 
    Program = rep(c("Philosophy", "Economics"),                                                 # create one column combining philo and economics students in data
                  times = c(length(philosophy_skill_change), length(economics_skill_change))),  # they have the same number of observations as in original data
    R_Skills_Change = c(philosophy_skill_change, economics_skill_change)                        # also keep the data we need to analyze changs in r skills
  )

  
# 2) Create the graph 
  
  ggplot(combined_data, aes(x = R_Skills_Change, fill = Program)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Skills Change by Program",
         x = "R Skills Change",
         y = "Density") +
    scale_fill_manual(values = c("lightblue", "lightgreen")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
  
  
