################################################################################
#                      Supplemental code for slides                            #
################################################################################



#################################  Skewness  ###################################

  N <- 1000
  
  right_skewed <- data.frame(skew = rbeta(N, 5, 2))        # create distribtion
  right_skewed$mean_values <- mean(right_skewed$skew)      # extract mean
  right_skewed$median_values <- median(right_skewed$skew)  # extract median
  right_skewed$type <- 'right skewed'
  
  left_skewed <- data.frame(skew = rbeta(N, 2, 5))
  left_skewed$mean_values <- mean(left_skewed$skew)
  left_skewed$median_values <- median(left_skewed$skew)
  left_skewed$type <- 'left skewed'

  # Combine the two data frames
   combo <- rbind(right_skewed, left_skewed)
  
  # Plotting
    ggplot(combo, aes(x = skew, fill = type)) +
        geom_density(alpha = 0.2, color = "black") +
        geom_vline(aes(xintercept = mean_values, color = "Mean"), linetype = "dashed", size = 1) +   # add mean and median            
        geom_vline(aes(xintercept = median_values, color = "Median"), linetype = "dashed", size = 1) +
        labs(title = "Skewness",           # add titles
             x = "Value",
             y = "Density") +
        theme_minimal() +
        theme(legend.position = "bottom") +
        scale_color_manual(values = c("right skewed" = "red", "left skewed" = "blue",
                                      "Mean" = "black", "Median" = "grey"),
                           name = "Distribution") +
        guides(fill = guide_legend(title = NULL)) + 
        theme(plot.title = element_text(hjust = 0.5)) 
    
    
##########################  Create data yourself  ############################## 
    
  # this is generally a super useful way to test whether your code is correct
  # especially, if your dataset were huge you can safe a lot of time
    
    max_r_skills <- 10
    min_r_skills <- 0 
    
    # Create the dataset with specified characteristics for each group
    participants <- data.frame(
      tutorium_attendance = sample(0:4, 120, replace = TRUE),
      r_skills_before = c(
        rnorm(40, mean = 1, sd = 0.1),             # Philosophy - Almost 0
        rnorm(40, mean = 5, sd = 2.5),          # Economics - Small value at the beginning
        rnorm(40, mean = 8, sd = 2)),            # Informatics - High level
      r_skills_change = pmin(c(
        abs(rnorm(40, mean = 0, sd = 0.2))  ,       # Philosophy - Almost no rise
        abs(rnorm(40, mean = 3, sd = 1)),        # Economics - Strongest R change
        abs(rnorm(40, mean = 1, sd = 0.5))        # Informatics - Small rise
      )),
      learning_hours = rnorm(120, mean = 10, sd = 3),
      grade_change = rnorm(120, mean = 0, sd = 1),
      program = factor(rep(c("Philosophy", "Economics", "Informatics"), c(40, 40, 40)))
    )
    
    # Add learning effect by attending tutorium
    participants$r_skills_change <- participants$r_skills_change + 0.4 * participants$tutorium_attendance
  
    
    # Create level differences between programs
    desired_order <- c("Informatics", "Economics", "Philosophy")
    participants$program <- factor(participants$program, levels = desired_order)
    participants <- participants[order(participants$program, participants$r_skills_change), ]
  
    
    # Add that learning effort matters
    participants$learning_hours <- participants$learning_hours + rnorm(120, mean = 0, sd = 1)
    participants$r_skills_change <- participants$r_skills_change + 0.2 * participants$learning_hours + rnorm(120, mean = 0, sd = 1)

    # Add r_skills_after 
    participants$r_skills_after <- participants$r_skills_before + participants$r_skills_change
    
    save(participants, file="participants.csv")
    
    
# Can I trust the data? 
    
    # Calculate the means for each group
    participants <- participants %>% 
      group_by(program) %>% 
      mutate(mean_r_change = mean(r_skills_change),
             mean_r_before = mean(r_skills_before),
             mean_r_after = mean(r_skills_after))
    
    library(viridis)
    
    # Show how attendance influence affects r skill change
    ggplot(participants, aes(x = tutorium_attendance, y = r_skills_change, fill = program)) +
      geom_col( position = "stack") +  # Specify the border color and use stacked bars
      scale_fill_viridis(discrete = TRUE) +  
      theme_minimal() +
      labs(title = "Relationship between Tutorium Attendance and R Skills Change",
           x = "Tutorium Attendance",
           y = "R Skills Change")
    
    # Learning effect
    ggplot(participants, aes(x=learning_hours, y=r_skills_change)) +
      geom_smooth(method = "loess")
    

