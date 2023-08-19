thorium.decay.function.mins <- function(df, na.rm = TRUE, ...){
  
  # create list of samples in data to loop over 
  sample_list <- unique(df$sample_name)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sample_list)) { 
    
    # begin creating decay graph 
    plot1 <- 
      ggplot(subset(df, df$sample_name==sample_list[i]),
             aes(x=decay_function, y=cpm)) + geom_point()+
      
      
      theme(legend.position="none") + 
      
      #      scale_y_continuous("County Population within Age Categories (thousands)", 
      #                         limits=c(0, max(df$value[df$County==county_list[i]]))/1000) +
      #      scale_x_continuous("Year") +
      
      ggtitle(paste(sample_list[i])) +
      
      stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
      geom_smooth(method="lm",se=FALSE)
    
    
    # save plots as .png
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        county_list[i], ".png", sep=''), scale=2)
    
    #save plots as .pdf
    #ggsave(plot, file=paste(sample_list[i], ".pdf", sep=''), scale=1)
    
    # print plots to screen
    print(plot1)
    
  }
}

