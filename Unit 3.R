##############################################################################
#                        Unit 3 - Graphs with ggplot                         #
##############################################################################

# load packages
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(haven)
  library(wesanderson)

# set path 
  setwd("C:\\Users\\hamassen\\Documents\\GitHub\\R-Tutorium")
  

# load data
  data  <- read_dta("traffic.dta")
  

# clean data
  data[] <- lapply(data, function(x) { attributes(x) <- NULL; x })     # remove all attributes
  
  data[] <- lapply(data, function(x) {
    if ("format.stata" %in% names(attributes(x))) {     # remove attr "format.stata"
      attributes(x)$format.stata <- NULL} 
    x  })

############################# Get to know data #################################

  # displays min, max, quartile, mean, median
  summary(data$spircons) # spirit consumption
  summary(data$unrate)   # unemployment rate
  summary(data$perinc)   # Income pc
  summary(data$allmort)  # Vehicle fatalities (VF)
  
  
###############################  1)  Bar plot  #################################

####### Goal: Get an overview how many workers are in which occupation ######### 
  
  
# 1) Prepare data    
  
  data$decile_spircons <- cut(data$spircons,
                            breaks = quantile(data$spircons,
                            probs = seq(0, 1, by = 0.1)),
                            include.lowest = TRUE,
                            labels = FALSE)
  data$decile_allmort <- cut(data$allmort,
                              breaks = quantile(data$allmort,
                              probs = seq(0, 1, by = 0.1)),
                              include.lowest = TRUE,
                              labels = FALSE)

  
  sum(is.na(data$spircons))             # sum how many NA's exist 

# 2) Create Plot
 
  ggplot(data,                                   # dataset we use
         aes(x = decile_spircons)) +             # we want to see the how spirit consumption is distributed
    geom_bar()                                   # geom_bar gives us a bar plot
  
  # goal achieved, but make it nicer
  
  
# 3) Make your graph enjoyable to look at
 
  library(viridis) # package for nice colors
  library(viridisLite)
  
  # Create info what the categories mean 
  data$decile_spircons <- factor(data$decile_spircons, levels = c(1:10), labels = 1:10)
  

  
  # plot it again 
  ggplot(data, aes(x = spircons, fill = factor(decile_spircons))) +    # in factor () we add the prior created info on categories
    geom_bar() +                                              # type of graph 
    scale_fill_viridis_d() +                                  # changes color
   
    # changes concerning look of graph 
    labs(title = "Bar Plot of Spirit Consumption",            # add title 
         x = "Occupation Type",                               # add titles to axis
         y = "Count") +
    guides(fill = guide_legend(title = "Spirit consumption")) +  # Add legend title
    theme_minimal() +
    theme(legend.position = "bottom",                         # Move legend to bottom
          legend.box = "horizontal",                          # Arrange legend items horizontally
          legend.key.size = unit(1, "cm"),               
          plot.title = element_text(hjust = 0.5))             # Center the title


  
# Who are those outliers? 
  insights <- data[data$spircons > 3, c("state", "year")]
  print(insights)

 
##############################  2) Line plot  ##################################
 
# Goal: create Spirit consumption over time per state
  ggplot(data,                                         # dataset
         aes(x = year, y = spircons,                   # axis definition
             group = state, color = state)) +          # group data by state
    geom_line() +                                      # defines line plot 
    labs(title = "Line Plot of Consumption",           # plot title
         x = "Year",                                   # x-axis title
         y = "Consumption") +                          # y-axis title
    theme_minimal() +
    theme(legend.position = "right",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5))
  
  # not so informative
  
  
  # make it cleaner
  subsample <- subset(data, state %in% c(32, 33, 38, 42)) # select a subsample
  
  # Efficiency tip 1: # predefine labels
  label <- unique(subsample$stabrv)                       
   
  ggplot(subsample,                               
         aes(x = year, y = spircons,               
             group = state, color = stabrv)) +  
    geom_line() +                                 
    labs(title = "Line Plot of Consumption",
         x = "Year",
         y = "Consumption",
         color = "Statefips") +
    theme_minimal() + 
    scale_color_manual(values = wes_palette("Zissou1", n=4),  # n = 4, for each category one color   
                       name = "States",               # define legend title
                       labels = label) +              # we add our predefined labels here 
    theme(legend.position = "bottom",  
          plot.title = element_text(hjust = 0.5))

  
  
# smoothed line plot 
  ggplot(data, aes(x = year, y = unrate)) +
    geom_smooth(method = "loess", se = FALSE) +                   # geom_smooth = smoothed line plot
    scale_color_manual(values = wes_palette("Zissou1", n = 1)) +  
    labs(title = "Smooth Line Plot of Unemployment",
         x = "Year",
         y = "Unemployment Rate") +
    theme_minimal() +
    theme(legend.position = "bottom",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5)) 
  
  # make trends clearer
  
# Explanation: 
# method loess = non-linear line, se=FALSE - removes band 
  
  ggplot(data, aes(x = year, y = unrate)) +
    geom_smooth(method = "lm", se = FALSE) +                   # "lm" = linear model 
    scale_color_manual(values = wes_palette("Zissou1", n = 1)) +  
    labs(title = "Smooth Line Plot of Unemployment",
         x = "Year",
         y = "Unemployment Rate") +
    theme_minimal() +
    theme(legend.position = "bottom",      
          legend.box = "horizontal",        
          legend.key.size = unit(1, "cm"), 
          plot.title = element_text(hjust = 0.5)) 
  
  # More info: https://ggplot2.tidyverse.org/reference/geom_smooth.html
  
  
  
#########################  3) Scatter plot  ####################################

  ggplot(data, aes(x=perinc, y=mrall)) +
    geom_point() +    
    labs(title = "Scatter Plot of income and fatality rate",
         x = "Income",
         y = "Vehicle Fatality rate") +
    theme_minimal() 
  
  
  ggplot(data, aes(x=perinc, y=mrall)) +
    geom_point() +                               
    geom_smooth(method = "lm", se = FALSE) +    
    labs(title = "Scatter Plot of income and fatality rate",
         x = "Income",
         y = "Vehicle Fatality rate") +
    theme_minimal() 
  
  
  
# Goal: Have states with more spirit consumption more vehicle fatalities? 
  

# Efficiency tip 2: Predefine color scheme 
  color <- wes_palette("GrandBudapest2", n = 2)
  
  ggplot(data, aes(x = perinc, y = mrall)) +
    geom_point(color = color[1]) +                                  # using the first color [1] of our predefined colors
    geom_smooth(method = "lm", se = FALSE, col = color[2]) +     # using the second color [2]
    labs(title = "Scatter Plot of Consumption and vehicle fatality rate",
         x = "Income pc",
         y = "Vehicle Fatality rate") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.box = "horizontal",                  # Centers title
           plot.title = element_text(hjust = 0.5))
  

  

#########################  4) Density plot  ####################################  

# Goal: How is income distributed?
  
# How things are distributed is an important part of improving our economic understanding
  
  
# Efficiency tip 3: Predefine save plots
  
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
    ggplot(data, aes(x = perinc)) +
    geom_density(fill = "skyblue", color = "blue", alpha = 0.7) +     # geom_density = distribution plot
                                                                     # alpha regulates transparency of color - try it out :) 
    theme_minimal() +
    labs(title = "Distribution of Income", 
         x = "Income per capita",
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
   
   
  
   
#########################  5) Heatmap plot  ####################################  
   
   library(reshape2)
   
   heatmap_data_us18 <- matrix(
     c(
       964, 978, 971, 793, 605, 517, 448, 437, 461, 661,
       927, 1038, 975, 815, 605, 492, 427, 423, 480, 652,
       1058, 1079, 1115, 1004, 651, 461, 388, 326, 363, 390,
       1062, 1090, 1107, 1055, 756, 537, 391, 325, 267, 244,
       832, 849, 847, 950, 881, 796, 609, 493, 324, 254,
       661, 657, 631, 764, 932, 864, 780, 685, 540, 320,
       513, 477, 472, 543, 840, 975, 950, 907, 683, 474,
       384, 308, 322, 433, 694, 932, 1071, 1038, 960, 693,
       263, 238, 254, 301, 547, 731, 963, 1158, 1265, 1114,
       171, 120, 141, 176, 324, 529, 807, 1043, 1491, 2033
     ),
     ncol = 10, byrow = TRUE,
     dimnames = list(
       c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
       c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
     )
   )
   
   # Prepare data
   N_US_18 <- sum(heatmap_data_us18)
   matrix18_US <- (heatmap_data_us18 / N_US_18)  * 10 
   
   
   # Convert to data frame
   heatmap_data_us18_df <- as.data.frame(matrix18_US)     # into dataframe
   heatmap_data_us18_long <- melt(heatmap_data_us18_df)   # reshape
   heatmap_data_us18_long <- heatmap_data_us18_long %>%   # count frequency per decile
     group_by(variable) %>%
     mutate(variable_count = row_number())
   
   plot_US18 <- ggplot(heatmap_data_us18_long, aes(variable, variable_count, fill = value)) + 
     geom_tile() +  # geom_tile gives us grid
     labs(x = "Capital income", y = "Labor income", title = "US 2018", fill = "Freq.") +
     geom_text(aes(label = round(value, 2)), color = "white", vjust = 1) +  # Add text into tiles
     scale_fill_gradient() +
     theme_minimal() +
     theme(panel.grid = element_blank(),  # ,   
        # More options to make it nicer looking   
           legend.position = "off",              # surpress legend
           plot.title = element_text(hjust = 0.5, size = 20),  # center title
           legend.text = element_text(size = 16),              # text size in tiles
           axis.title.x = element_text(size = 16),             # x-axis title size
           axis.title.y = element_text(size = 16 ),            # y-axis title size
           axis.text.x = element_text(size = 14),              # x-label size
           axis.text.y = element_text(size = 14), 
           plot.margin = margin(30, 30, 30, 30))               # Increase border around graph
  
   
   
   
  
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
  
  
  
   