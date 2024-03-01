################################################################################
#                                 Unit 4                                       #
#                           Basic Econometrics                                 #
################################################################################

# Load Packages

library(stargazer) # required to export regression tables
library(ggplot2)
library(plm)
library(dplyr)
library(magrittr)

setwd()

# add this to supplemental material!
set.seed(123)
participants <- data.frame(
  tutorium_attendance = sample(0:4, 100, replace = TRUE),
  r_skills_before = rnorm(100, mean = 50, sd = 10), 
  r_skills_change = rnorm(100, mean = 0, sd = 1),
  learning_hours = rnorm(100, mean = 10, sd = 3),
  grade_change = rnorm(100, mean = 0, sd = 1),
  program = factor(sample(c("Philosophy", "Informatics", "Economics"), 100, replace = TRUE))
)

# Calculate r_skills_after separately
participants$r_skills_after <- participants$r_skills_before + participants$r_skills_change


save(participants, file="participants.csv")
#######################################################################

data <- load("participants.csv")


# take a look at data

summary(participants)

# Use a boxplot
ggplot(participants, aes(x = program, y = r_skills_before)) +
  geom_boxplot() + 
  theme_minimal()

# what can you say about median, min, max, outliers?

ggplot(participants, aes(x = r_skills_before, fill = program)) +
  geom_density(alpha = 0.5) +
  labs(title = "Kernel Density Estimate of R Skills Before vs. After",
       x = "R Skills",
       y = "Density") +
  theme_minimal()


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

# Look at the outcome 
linear_model

# Export regression table
stargazer(linear_model)


linear_model_2 <- lm(r_skills_after ~ tutorium_attendance + learning_hours, data = participants)


# Fixed Effects Model (load plm package)

fe_reg <- plm(r_skills_change ~ tutorium_attendance, # like in OLS
                    data = participants,             
                    index = c("program"),            # we have to indicate the level 
                                                     # on which we apply fixed effects
                    model = "within")                # within specify fixed effects model

fe_reg

##########################  Hypothesis Test  ###################################

# (Testing if attendance has an effect)
hypothesis_test_ols <- summary(linear_model)
hypothesis_test_fe <- summary(fe_reg)

# T-test
t_test_result <- t.test(participants$r_skills_change)

# p-value
p_value <- t_test_result$p.value



