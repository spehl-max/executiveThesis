lead.ytrium.decay.function.days <- function(df, na.rm = TRUE, ...){
  # create list of samples in data to loop over 
  sample_list <- unique(df$sample_name)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sample_list)) { 
    
 
  current_df <- data.frame(x = unlist(subset(df, df$sample_name==sample_list[i])[,"days_from_filtering"]), y = unlist(subset(df, df$sample_name==sample_list[i])[,"cpm"]))
  m <- lm(y ~ exp(-1.56939*x) + exp(-.259525*x), current_df)
  errors <- sqrt(diag(vcov(m)))
  attr(errors, 'names') <- c("std error of intercept", "a", "b")
  print(m$coefficients)
  print(errors)
  
##fixing the errant values
  current_tap_student_resids <- as.data.frame(rstudent(m)) 
  
  colnames(current_tap_student_resids) <- "student_residuals"
  
  current_df <- current_df %>% bind_cols(current_tap_student_resids) %>% filter(student_residuals < 3) %>% filter(student_residuals > -3)
  
  plot2 <- ggplot(current_df,
                  aes(x=x, y=y)) + 
    geom_point()+
    theme(legend.position="none") + 
    
    #      scale_y_continuous("County Population within Age Categories (thousands)", 
    #                         limits=c(0, max(df$value[df$County==county_list[i]]))/1000) +
    #      scale_x_continuous("Year") +
    
    geom_smooth(method="lm", formula = y ~ exp(-1.56939*x) + exp(-.259525*x), se=FALSE) +
    ggtitle(paste(sample_list[i], '\n', 
                  "counts per minute by days elapsed from filtering\n",
                  sep=''))

  # print plots to screen
  print(plot2)
  
  
  
  
  
  }
}  