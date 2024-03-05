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

  # OLS can be obtained with the lm command

  linear_model <- lm(r_skills_change ~ tutorium_attendance, data = participants)

# Breakdown: 
# 1) Estimated value (y_hat) = r_skills_change
#   We are interested in how the skills change
#
# 2) Explanatory / independent variable (x_i) = tutorium attendance
#    Our model aims to predict how the attendance in the tutorium affects the R skills
#
# 3) dataset used = data

  # Print the regression outcome 
  summary(linear_model)

  
  # How to add more independent variables
  
  # We can add multiple explanatory variables with "+"
  linear_model_2 <- lm(r_skills_after ~ tutorium_attendance + learning_hours, data = participants)
  
  summary(linear_model_2)
  
  
  
  # Alternatives to export regression table
  
  # Export regression results to LaTeX for Overleaf
  stargazer(linear_model_2, title = "Regression Results", header = FALSE, type = "latex", out = "regression_results.tex")
  
  # Export regression results to Word
  stargazer(linear_model_2, title = "Regression Results", header = FALSE, type = "text", out = "regression_results.docx")
  
  
  

########################### Fixed Effects Model ################################ 

  # Students from different programs have different skills and experience
  # It is hard to catch all these factors, but it is likely that students of 
  # the same program exhibit similar factors -> it is time-invariant and group-
  # specific. Thus, we can use program fixed effects to capture these unobserved
  # characteristics

  fe_reg_1 <- plm(r_skills_change ~ tutorium_attendance, # like in OLS
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
    
    # Add a second layer to show slop of OLS model
    geom_smooth(method = "lm", se = FALSE) +                # linear fit, resembles beta in regression
    labs(title = "OLS",
         x = "Learning hours",
         y = "R Skills Change") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))

# Apply within 
model2 <-  ggplot(participants, aes(x = learning_hours, y = r_skills_change, 
                                    color = program)) + # differentiate color by program
    geom_point() +                                      # scatter plot layer              
  # Add a second layer to highlight the different slopes
    geom_smooth(method = "lm", se = FALSE) +            # we have now one slope per program
    labs(title = "Fixed effect",
         x = "Learning hours",
         y = "R Skills Change") +
   guides(color = guide_legend(title = "Program")) +   
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  
# Combines graphs next to each other
grid.arrange(
  arrangeGrob(model1),
  arrangeGrob(model2),
  ncol = 2,
  top = "Impact of Learning hours on R Skills Change"
)



##########################  Hypothesis Test  ###################################
  
  
#########################  one sided T-test ########################    

# Null Hypothesis (H0): The mean change in R skills is less than or equal to 5.
# Alternative Hypothesis (H1): The mean change in R skills is greater than 5.
  
  population_mean <- 5 # value we assume to be population mean

  t_test <- t.test(participants$r_skills_change, mu = population_mean, alternative = "greater") # one sided T-test
  
  t_test

  

# Create a density plot to illustrate the one-sample t-test results
  
  ggplot(participants, aes(x = r_skills_change)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    
    # This resembles our H0 line 
    geom_vline(xintercept = population_mean, linetype = "dashed", color = "darkblue", size = 1) +
    annotate("text", x = population_mean+1.3, y = 0.02, label = "Null Hypothesis", color = "black", vjust = -7) +
    
    # This resembles sample mean
    geom_vline(xintercept = sample_mean, linetype = "dashed", color = "darkblue", size = 1) +
    annotate("text", x = sample_mean +1.3, y = 0.02, label = "Sample mean", color = "black", vjust = -7 ) +
    
    labs(title = "Density Plot with One-Sample t-Test",
         x = "R Skills Change",
         y = "Density") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))


######################### Two-sample T-test ####################################    
  
# Are the r skill changes of economics and philosophy students different?
  
  philosophy_data <- participants[participants$program == "Philosophy", "r_skills_change"] # selects r_skills_change if the program is philo
  economics_data <- participants[participants$program == "Economics", "r_skills_change"]
  
  t_test_result <- t.test(philosophy_data, economics_data) # two sample t-test
  

  # Create a density plot
  
  combined_data <- data.frame(
    Program = rep(c("Philosophy", "Economics"), each = c(length(philosophy_data), length(economics_data))),
    R_Skills_Change = c(philosophy_data, economics_data)
  )

  ggplot(combined_data, aes(x = R_Skills_Change, fill = Program)) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot of Skills Change by Program",
         x = "R Skills Change",
         y = "Density") +
    scale_fill_manual(values = c("lightblue", "lightgreen")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5))
  



  t_test <- t.test(participants$r_skills_change[participants$program == "Economics"], mu = 15, method = "greater") # mu = 20 is the H0 we are testing
  
  # H0: mean = 0  would imply there is no change in R skills
  
  t_statistic <- t_test_result$statistic
  
  critical_values <- c(quantile(t_statistic, c(0.01, 0.05, 0.10)))
  
  # Create a data frame for plotting
  economics_data_df <- data.frame(economics_data)
  
  # Plot the density and add vertical lines
  ggplot(economics_data_df, aes(x = economics_data)) +
    geom_density() +
    geom_vline(xintercept = 15, linetype = "dashed", color = "red") +
    geom_vline(xintercept = critical_values, linetype = "dotted", color = "blue") +
    labs(title = "Density Plot with Vertical Lines",
         x = "Economics Data",
         y = "Density") +
    theme_minimal()
  
  
  
  
  
  
  critical_quantiles <- c(0.01, 0.05, 0.10)
  critical_values <- quantile(density(economics_data_df$economics_data), critical_quantiles)
  
  # Create a data frame for plotting
  economics_data_df <- data.frame(economics_data = participants$r_skills_change)
  
  # Plot the density and add vertical lines
  ggplot(economics_data_df, aes(x = economics_data)) +
    geom_density() +
    
    # Add vertical lines for the mean and critical values
    geom_vline(xintercept = mean(participants$r_skills_change), linetype = "dashed", color = "red") +
    geom_vline(xintercept = critical_values, linetype = "dotted", color = "blue") +
    
    # Add horizontal lines for significance levels
    geom_hline(yintercept = c(quantile(density(economics_data_df$economics_data)$x, 0.99),
                              quantile(density(economics_data_df$economics_data)$x, 0.95),
                              quantile(density(economics_data_df$economics_data)$x, 0.90)),
               linetype = "dotted", color = "green") +
    
    labs(title = "Density Plot with Vertical Lines",
         x = "Economics Data",
         y = "Density") +
    
    theme_minimal()
  
  