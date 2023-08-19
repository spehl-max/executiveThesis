thorium.decay.function.days <- function(df, na.rm = TRUE, ...){
  # create list of samples in data to loop over 
  sample_list <- unique(df$sample_name)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(sample_list)) { 
    
    plot2 <- ggplot(subset(df, df$sample_name==sample_list[i]),
                    aes(x=days_from_filtering, y=cpm)) + 
      geom_point()+
      theme_pander()+
      theme(legend.position="none") + 
      
      #      scale_y_continuous("County Population within Age Categories (thousands)", 
      #                         limits=c(0, max(df$value[df$County==county_list[i]]))/1000) +
      #      scale_x_continuous("Year") +
      
      geom_smooth(method="lm", formula = y ~ exp(-.028761*x), se=FALSE) +
      ggtitle(paste(sample_list[i], '\n', 
                    "counts per minute by days elapsed from filtering\n",
                    sep=''))
    
    
    
    #  plot2 <- plot2 + geom_text(x = 5, y = 7, label = lm_eqn(subset(df, df$sample_name==sample_list[i])), parse = TRUE)
    # save plots as .png
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        county_list[i], ".png", sep=''), scale=2)
    
    #save plots as .pdf
    #ggsave(plot, file=paste(sample_list[i], ".pdf", sep=''), scale=1)
    
    # print plots to screen
    print(plot2)
    
    current_df <- data.frame(x = unlist(subset(df, df$sample_name==sample_list[i])[,"days_from_filtering"]), y = unlist(subset(df, df$sample_name==sample_list[i])[,"cpm"]))
    m <- lm(y ~ exp(-.028761*x), current_df)
    errors <- sqrt(diag(vcov(m)))
    attr(errors, 'names') <- c("std error of intercept", "a")
    print(m$coefficients)
    print(errors)
  }  }