################################################################################
#                      Supplemental code for slides                            #
################################################################################


################################  Unit 2  ######################################

# Skewness

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